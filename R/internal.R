save_config <- function(path, pattern, recursive, fun, dots) {
  args <- list(time = sys_time_utc(), pattern = pattern, 
               recursive = recursive, fun = fun, 
               dots = dots)
  saveRDS(args, file = file.path(path, ".batchr.rds"))
}

lock_config <- function(path) {
  file <- file.path(path, ".batchr.rds.lck")
  lock <- try(lock(file.path(path, ".batchr.rds.lck"), timeout = 0))
  !is_try_error(lock)
}

read_lines_log <- function(path) {
  file <- file.path(path, ".batchr.log")
  if(!file.exists(file)) return(character(0))
  readLines(file.path(path, ".batchr.log"), warn = FALSE)
}

no_log_data <- function() {
  time <- sys_time_utc()[-1]
  tibble(type = character(0), time = time, file = character(0))
#  tibble(type = type, time = time, file = file, error_msg = character(0))
}

logged_data <- function(path) {
  lines <- read_lines_log(path)
  if(!length(lines)) return(no_log_data())
  
  type <- str_extract(lines, "^\\w+")
  time <- str_extract(lines, "\\[\\d{4,4}(-\\d{2,2}){2,2} \\d{2,2}(:\\d{2,2}){2,2}\\]")
  time <- as.POSIXct(substr(time, 2, 20), tz = "UTC")
  file <- str_extract(lines, "[^ ]+$")
  tibble(type = type, time = time, file = file)
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
  msg <- p(msg )
  cat(msg, file = file, append = file.exists(file))
}

process_file <- function(file, fun, dots, path, progress) {
  dots <- c(file.path(path, file), dots)
  output <- try(do.call("fun", dots), silent = TRUE)
  
  time <- sys_time_utc()
  time <- format(time, format = "%Y-%m-%d %H:%M%:%S")
  msg <- p0("[", time, "]", " ", file, "\n")
  
  if(is_try_error(output)) {
    msg <- p("FAILURE", msg)
    log_msg(path, msg)
    if(progress != "none") cat(msg)
    return(FALSE)
  }
  if(isFALSE(output)) {
    msg <- p("FAILURE", msg)
    log_msg(path, msg)
    if(progress != "none") cat(msg)
    return(FALSE)
  }
  touch_file(path, file)
  msg <- p("SUCCESS", msg)
  log_msg(path, msg)
  if(progress == "all") cat(msg)
  TRUE
}

cleanup_log_files <- function(path) {
  file <- file.path(path, ".batchr.log")
  if(file.exists(file)) unlink(file)
}

cleanup_config <- function(path, force, remaining, failed) {
  remaining_files <- batch_remaining_files(path, failed = failed)
  if(length(remaining_files)) {
    if(!force) return(FALSE)
    if(remaining) unlink(remaining_files)
  }
  unlink(file.path(path, ".batchr.rds"))
  cleanup_log_files(path)
  TRUE
}
