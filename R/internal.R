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

read_log <- function(path) {
  file <- file.path(path, ".batchr.log")
  if(!file.exists(file)) return(character(0))
  readLines(file.path(path, ".batchr.log"))
}

read_log_error <- function(path) {
  file <- file.path(path, ".batchr_error.log")
  if(!file.exists(file)) return(character(0))
  readLines(file.path(path, ".batchr_error.log"))
}

no_log_data <- function(error_msgs) {
  level <- ordered(character(0), levels = .levels)
  time <- sys_time_utc()[-1]
  file <- character(0)
  if(!error_msgs) return(tibble(level = level, time = time, file = file))
  tibble(level = level, time = time, file = file, error_msg = character(0))
}

logged_data <- function(path, error_msgs) {
  lines <- read_log(path)
  if(!length(lines)) return(no_log_data(error_msgs))
  
  level <- str_extract(lines, "^\\w+")
  level <- ordered(level, levels = .levels)
  time <- str_extract(lines, "\\[\\d{4,4}(-\\d{2,2}){2,2} \\d{2,2}(:\\d{2,2}){2,2}\\]")
  time <- as.POSIXct(substr(time, 2, 20), tz = "UTC")
  file <- str_extract(lines, "[^ ]+$")
  data <- tibble(level = level, time = time, file = file)
  if(!error_msgs) return(data)
}

failed_files <- function(path) {
  log <- logged_data(path, error_msgs = FALSE)
  log <- log[!duplicated(log$file, fromLast = TRUE),]
  log <- log[log$level != "INFO",]
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

process_file <- function(file, fun, dots, path, progress) {
  dots <- c(file.path(path, file), dots)
  logger <- create.logger(file.path(path, ".batchr.log"), level = "INFO")
  output <- try(do.call("fun", dots), silent = TRUE)
  
  msg <- p(sys_time_utc(), file)
  
  if(is_try_error(output)) {
    logger_error <- create.logger(file.path(path, ".batchr_error.log"), 
                                  level = "ERROR")
    error(logger, msg)
    error(logger_error, p(msg, as.character(output)))
    
    if(progress != "none") cat("ERROR ", file, "\n")
    return(FALSE)
  }
  if(isFALSE(output)) {
    warn(logger, msg)
    if(!progress %in% c("none", "error")) cat("WARN  ", file, "\n")
    return(FALSE)
  }
  touch_file(path, file)
  info(logger, msg)
  if(progress == "info") cat("INFO  ", file, "\n")
  TRUE
}

cleanup_log_files <- function(path) {
  file <- file.path(path, ".batchr.log")
  if(file.exists(file)) unlink(file)
  file2 <- file.path(path, ".batchr_error.log")
  if(file.exists(file2)) unlink(file2)
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
