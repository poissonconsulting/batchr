save_config <- function(path, regexp, recurse, fun, dots) {
  args <- list(time = sys_time_utc(), regexp = regexp, 
               recurse = recurse, fun = fun, 
               dots = dots)
  saveRDS(args, file = file.path(path, ".batchr.rds"))
}

read_lines_log <- function(path) {
  file <- file.path(path, ".batchr.log")
  if(!file.exists(file)) return(character(0))
  readLines(file.path(path, ".batchr.log"), warn = FALSE)
}

no_log_data <- function() {
  time <- sys_time_utc()[-1]
  tibble(type = character(0), time = time, file = character(0), 
         error = character(0))
}

logged_data <- function(path) {
  lines <- read_lines_log(path)
  if(!length(lines)) return(no_log_data())
  
  regexp <- p0("^([SUCESFAILUR]{7,7})( \\[)",
               "(\\d{4,4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d)",
               "(\\] ')([^']+)('\\s*)(.*)$")
  
  type <- gsub(regexp, "\\1", lines)
  time <- gsub(regexp, "\\3", lines)
  file <- gsub(regexp, "\\5", lines)
  error <- gsub(regexp, "\\7", lines)
  
  time <- as.POSIXct(time, tz = "UTC")
  is.na(error[error == ""]) <- TRUE
  
  tibble(type = type, time = time, file = file, error = error)
}

failed_files <- function(path) {
  log <- logged_data(path)
  log <- log[!duplicated(log$file, fromLast = TRUE),]
  log <- log[log$type == "FAILURE",]
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

log_msg <- function(path, msg) {
  file <- file.path(path, ".batchr.log")
  msg <- p0(msg, "\n")
  cat(msg, file = file, append = file.exists(file))
}

console_msg <- function(msg) {
  msg <- p0(msg, "\n")
  cat(msg)
}

validate_remaining_file <- function(path, file, config_time) {
  if(!file.exists(file.path(path, file)))
    err("File '", normalizePath(file.path(path, file)), 
        "' has been deleted by a different process!")
  if(file_time(path, file) > config_time) 
    err("File '", normalizePath(file.path(path, file)), 
        "' has been modified by a different process!")
}

process_file <- function(file, fun, dots, path, config_time, progress,
                         i = 1, n = 1, e = 0) {
  validate_remaining_file(path, file, config_time)
  
  dots <- c(file.path(path, file), dots)
  output <- try(do.call("fun", dots), silent = TRUE)
  
  time <- sys_time_utc()
  time <- format(time, format = "%Y-%m-%d %H:%M%:%S")
  msg <- p0("[", time, "]", " '", file, "'")
  
  if(is_try_error(output)) {
    output <- gsub("\n+", " ", as.character(output))
    msg <- p("FAILURE", msg, output)
    log_msg(path, msg)
    if(!isFALSE(progress)) console_msg(msg)
    return(FALSE)
  }
  if(isFALSE(output)) {
    msg <- p("FAILURE", msg)
    log_msg(path, msg)
    if(!isFALSE(progress)) console_msg(msg)
    return(FALSE)
  }
  touch_file(path, file)
  msg <- p("SUCCESS", msg)
  log_msg(path, msg)
  if(isTRUE(progress)) console_msg(msg)
  TRUE
}

process_files <- function(remaining, fun, dots, path, config_time, parallel, 
                          progress) {
  if(parallel) {
    if(!requireNamespace("plyr", quietly = TRUE))
      err("plyr is required to batch process files in parallel")
    success <- plyr::llply(remaining, process_file, fun = fun, dots = dots, 
                           path = path, config_time = config_time, 
                           .parallel = TRUE, progress = FALSE)
  } else {
    n <- length(remaining)
    success <- rep(NA, n)
    e <- 0L
    for(i in seq_along(remaining)) {
      success[i] <- process_file(remaining[i], fun = fun, dots = dots, 
                                 path = path, config_time = config_time, 
                                 progress = progress, i = i, n = n, e = e)
      if(!success[i]) e <- e + 1L
    }
  }
  success <- unlist(success)
  invisible(set_names(success, remaining))
}

cleanup_log_file <- function(path) {
  file <- file.path(path, ".batchr.log")
  if(file.exists(file)) unlink(file)
}

cleanup_config <- function(path, force, remaining, failed) {
  remaining_files <- batch_files_remaining(path, failed = failed)
  if(length(remaining_files)) {
    if(!force) return(FALSE)
    if(remaining) unlink(file.path(path, remaining_files))
  }
  unlink(file.path(path, ".batchr.rds"))
  cleanup_log_file(path)
  TRUE
}

config_files <- function(path, recursive) {
  list.files(path, pattern = "^[.]batchr[.]rds$", recursive = recursive,
             all.files = TRUE)
}
