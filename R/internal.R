report_file <- function(x) {

  time <- x$time
  if(!is.na(time)) {
    time <- time_to_character(time)
    time <- paste0(" [", time, "]")
  }

  switch(x$type,
    SUCCESS = cli_alert_success(c(col_white(x$file), col_blue(time))),
    FAILURE = cli_alert_danger(c(col_white(x$file), col_blue(time))),
    REMAING = cli_alert_warning(col_white(x$file)))
}

report_files <- function(status) {
  status <- split(status, f = status$file)
  lapply(status, report_file)
}

report_types <- function(status) {
  status$type <- factor(status$type, levels = c("SUCCESS", "FAILURE", "REMAING"))

  table <- table(status$type)
  table <- as.data.frame(table)

  freq <- table$Freq

  cli_par()
  cli_text(col_white("Success: "), col_green(freq[1]))
  cli_text(col_white("Failure: "), if(freq[2] == 0) col_green(freq[2]) else col_red(freq[2]))
  cli_text(col_white("Remaining: "), if(freq[3] == 0) col_green(freq[3]) else col_red(freq[3]))
}

save_config <- function(path, regexp, recurse, fun, dots, time) {
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
  tibble(
    type = character(0), 
    time = hms::as_hms(integer(0)), 
    file = character(0),
    message = character(0)
  )
}

logged_data <- function(path) {
  lines <- read_lines_log(path)
  if (!length(lines)) {
    return(no_log_data())
  }

  regexp <- p0(
    "^([SUCESFAILUR]{7,7})( \\[)",
    "(\\d{2,}:\\d{2,2}:\\d{2,2}[.]\\d{3,3})",
    "(\\] ')([^']+)('\\s*)(.*)$"
  )

  type <- gsub(regexp, "\\1", lines)
  time <- gsub(regexp, "\\3", lines)
  file <- gsub(regexp, "\\5", lines)
  message <- gsub(regexp, "\\7", lines)

  time <- as_hms(time)
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

time_to_character <- function(time) {
  time <- tmr_round(time, digits = 3)

  msecs <- as.numeric(time) - floor(as.numeric(time))
  msecs <- formatC(msecs, digits = 3, format = "f")
  msecs <- substr(msecs, 2, 5)

  time <- tmr_floor(time)
  time <- paste0(time, msecs)
  time
}

process_file <- function(file, fun, dots, path, config_time) {

  validate_remaining_file(path, file, config_time)

  dots <- c(file.path(path, file), dots)

  time <- tmr_timer(start = TRUE)
  output <- try(do.call("fun", dots), silent = TRUE)
  time <- tmr_stop(time)
  time <- time_to_character(time)

  msg <- p0("[", time, "]", " '", file, "'")

  if (is_try_error(output)) {
    output <- as.character(output)
    output <- sub("^Error([^:]*:\\\\){0,1}[^:]+:\\s+", "", output)
    msg <- p("FAILURE", msg, output)
    log_msg(path, msg)
    return(FALSE)
  }
  if (isFALSE(output)) {
    msg <- p("FAILURE", msg)
    log_msg(path, msg)
    return(FALSE)
  }
  touch_file(path, file)
  msg <- p("SUCCESS", msg)
  if (vld_string(output)) msg <- p(msg, output)
  log_msg(path, msg)
  TRUE
}

process_files <- function(remaining, fun, dots, path, config_time,
                          progress, options) {

  success <- future_map(remaining, process_file,
    fun = fun, dots = dots,
    path = path, config_time = config_time,
    .progress = progress, .options = options)

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
