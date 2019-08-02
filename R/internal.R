sys_time <- function() {
  time <- Sys.time()
  attr(time, "tzone") <- "UTC" 
  time
}

.batch_files <- function(path, pattern, recursive) {
  list.files(path = path, pattern = pattern, recursive = recursive)
}

list_batchr_setup_rds_files <- function(path, recursive) {
}

save_batchr_setup <- function(path, pattern, recursive, FUN, dots) {
  args <- list(time = sys_time(), pattern = pattern, 
               recursive = recursive, FUN = FUN, 
               dots = dots)
  saveRDS(args, file = file.path(path, ".batchr.rds"))
}

#' Batch File Names
#'
#' @inheritParams batch_setup
#' @param processed A logical scalar specifying whether to return the names of 
#' all the file (default), the number processed (TRUE) or the number remaining (FALSE).
#'
#' @return A character vector of the paths to the files.
batch_files <- function(path = ".", processed = NA) {
  chk_lgl(processed)
  
#  args <- read_batchr_setup.rds(path)
  # files <- .batch_files(path, args$pattern, args$recursive)
  # if(is.na(processed)) return(files)
  # times <- vapply(files, file.mtime, sys_time())
  # if(isFALSE(processed)) return(files[times <= args$time])
  # files[times > args$time]
}

