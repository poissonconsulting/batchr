save_config <- function(path, regexp, recurse, fun, dots, time = sys_time_utc()) {
  args <- list(
    time = time, regexp = regexp,
    recurse = recurse, fun = fun,
    dots = dots
  )
  saveRDS(args, file = file.path(path, ".batchr.rds"))
}

read_lines_log <- function(path) {
  file <- file.path(path, ".batchr.log")
  if (!file.exists(file)) {
    return(character(0))
  }
  readLines(file.path(path, ".batchr.log"), warn = FALSE)
}

no_log_data <- function() {
  time <- sys_time_utc()[-1]
  tibble(
    type = character(0), time = time, file = character(0),
    message = character(0)
  )
}

logged_data <- function(path) {
  lines <- read_lines_log(path)
  if (!length(lines)) {
    return(no_log_data())
  }

  regexp <- p0(
    "^([SUCESFAILUR]{7,7})( )([123456789]\\d*/\\d+/\\d+)( \\[)",
    "(\\d{4,4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d)",
    "(\\] ')([^']+)('\\s*)(.*)$"
  )

  type <- gsub(regexp, "\\1", lines)
  count <- gsub(regexp, "\\3", lines)
  time <- gsub(regexp, "\\5", lines)
  file <- gsub(regexp, "\\7", lines)
  message <- gsub(regexp, "\\9", lines)

  time <- as.POSIXct(time, tz = "UTC")
  is.na(message[message == ""]) <- TRUE

  tibble(type = type, time = time, file = file, message = message)
}

failed_files <- function(path) {
  log <- logged_data(path)
  log <- log[!duplicated(log$file, fromLast = TRUE), ]
  log <- log[log$type == "FAILURE", ]
  sort(log$file)
}

file_time <- function(path, file) {
  time <- file.mtime(file.path(path, file))
  attr(time, "tzone") <- "UTC"
  time
}

touch_file <- function(path, file) {
  Sys.setFileTime(file.path(path, file), Sys.time())
}

clean_msg <- function(msg) {
  msg <- gsub("\n", " ", msg)
  msg <- sub("\\s*$", "", msg)
  msg <- p0(msg, "\n")
  msg
}

log_msg <- function(path, msg) {
  file <- file.path(path, ".batchr.log")
  msg <- clean_msg(msg)
  cat(msg, file = file, append = file.exists(file))
}

console_msg <- function(msg) {
  msg <- clean_msg(msg)
  cat(msg)
}

validate_remaining_file <- function(path, file, config_time) {
  if (!file.exists(file.path(path, file))) {
    err(
      "File '", normalizePath(file.path(path, file)),
      "' has been deleted by a different process!"
    )
  }
  if (file_time(path, file) > config_time) {
    err(
      "File '", normalizePath(file.path(path, file)),
      "' has been modified by a different process!"
    )
  }
}

formatc <- function(i, n) {
  n <- formatC(n, format = "fg")
  formatC(i, format = "fg", width = nchar(n), flag = "0")
}

rinteger <- function(n = 1) as.integer(runif(n, -.max_integer, .max_integer))

process_file <- function(file, fun, dots, path, config_time, progress,
                         seed, i = 1, n = 1, e = 0) {
  validate_remaining_file(path, file, config_time)

  dots <- c(file.path(path, file), dots)

  .Random.seed <<- seed
  seed <- digest2int(file, seed = rinteger())
  set.seed(seed)

  output <- try(do.call("fun", dots), silent = TRUE)

  time <- sys_time_utc()
  time <- format(time, format = "%Y-%m-%d %H:%M:%S")
  msg <- p0("[", time, "]", " '", file, "'")

  if (is_try_error(output)) {
    output <- as.character(output)
    output <- sub("^Error[^:]+:\\s+", "", output)
    count <- p(formatc(i, n), formatc(n, n), formatc(e + 1L, n), sep = "/")
    msg <- p("FAILURE", count, msg, output)
    log_msg(path, msg)
    if (progress) console_msg(msg)
    return(FALSE)
  }
  if (isFALSE(output)) {
    count <- p(formatc(i, n), formatc(n, n), formatc(e + 1L, n), sep = "/")
    msg <- p("FAILURE", count, msg)
    log_msg(path, msg)
    if (progress) console_msg(msg)
    return(FALSE)
  }
  touch_file(path, file)
  count <- p(formatc(i, n), formatc(n, n), formatc(e, n), sep = "/")
  msg <- p("SUCCESS", count, msg)
  if (vld_string(output)) msg <- p(msg, output)
  log_msg(path, msg)
  if (progress) console_msg(msg)
  TRUE
}

process_files <- function(remaining, fun, dots, path, config_time, parallel,
                          progress, options) {
  if (!exists(".Random.seed")) runif(1)
  seed <- .Random.seed

  if (parallel) {
    success <- future_map(remaining, process_file,
      fun = fun, dots = dots,
      path = path, config_time = config_time,
      progress = FALSE, .options = options,
      .progress = progress, seed = seed
    )
  } else {
    n <- length(remaining)
    success <- rep(NA, n)
    e <- 0L
    for (i in seq_along(remaining)) {
      success[i] <- process_file(remaining[i],
        fun = fun, dots = dots,
        path = path, config_time = config_time,
        progress = progress, seed = seed,
        i = i, n = n, e = e
      )
      if (!success[i]) e <- e + 1L
    }
  }
  success <- unlist(success)
  invisible(set_names(success, remaining))
}

cleanup_log_file <- function(path) {
  file <- file.path(path, ".batchr.log")
  if (file.exists(file)) unlink(file)
}

cleanup_config <- function(path, force, remaining, failed) {
  remaining_files <- batch_files_remaining(path, failed = failed)
  if (length(remaining_files)) {
    if (!force) {
      return(FALSE)
    }
    if (remaining) unlink(file.path(path, remaining_files))
  }
  unlink(file.path(path, ".batchr.rds"))
  cleanup_log_file(path)
  TRUE
}

config_files <- function(path, recursive) {
  list.files(path,
    pattern = "^[.]batchr[.]rds$", recursive = recursive,
    all.files = TRUE
  )
}
