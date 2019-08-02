save_config <- function(path, pattern, recursive, FUN, dots) {
  args <- list(time = sys_time(), pattern = pattern, 
               recursive = recursive, FUN = FUN, 
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

logged_data <- function(lines) {
  if(!length(lines)) {
    levels <- c("DEBUG", "INFO", "WARN",  "ERROR", "FATAL")
    level <- ordered(character(0), levels = levels)
    return(tibble(level = level, time = sys_time()[-1], file = character(0)))
  }
  levels <- c("DEBUG", "INFO", "WARN",  "ERROR", "FATAL")
  level <- str_extract(lines, "^\\w+")
  level <- ordered(level, levels = levels)
  time <- str_extract(lines, "\\[\\d{4,4}(-\\d{2,2}){2,2} \\d{2,2}(:\\d{2,2}){2,2}\\]")
  time <- as.POSIXct(substr(time, 2, 20), tz = "UTC")
  file <- str_extract(lines, "[^ ]+$")
  return(tibble(level = level, time = time, file = file))
}

failed_files <- function(path) {
  log <- read_log(path)
  sort(unique(logged_data(log)$file))
}

file_time <- function(path, file) {
  time <- file.mtime(file.path(path, file))
  attr(time, "tzone") <- "UTC"
  time
}

touch_file <- function(path, file) {
  Sys.setFileTime(file.path(path, file), Sys.time())
}

process_file <- function(file, fun, dots, path, logger) {
  dots <- c(file, dots, logger)
  output <- try(do.call("fun", dots), silent = TRUE)
  
  if(isTRUE(output)) {
    touch_file(path, file)
    return(TRUE)
  }
  
  logger <- create.logger(file.path(path, ".batchr.log"), level = "INFO")
  
  if(isFALSE(output)) {
    info(logger, file)
  } else if(is_try_error(output)) {
    error(logger, file)
  } else {
    fatal(logger, file)
    err("processing file '", file, "' returned an object of class '", class(output), "'")
  }
  FALSE
}
