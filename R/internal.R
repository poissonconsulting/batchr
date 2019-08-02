sys_time <- function() {
  time <- Sys.time()
  attr(time, "tzone") <- "UTC" 
  time
}

is_try_error <- function(x) {
  inherits(x, "try-error")
}

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

process_file <- function(file, .fun, .dots) {
  .dots <- c(file, .dots)
  output <- try(do.call(".fun", .dots), silent = TRUE)
  if(!is_try_error(output)) return(TRUE)
  # need to log name and output....
  FALSE
}
