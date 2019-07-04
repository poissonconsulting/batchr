#' Batch File Names
#'
#' @inheritParams batch_setup
#' @param processed A logical scalar specifying whether to return the names of 
#' all the file (default), the number processed (TRUE) or the number remaining (FALSE).
#'
#' @return A character vector of the paths to the files.
#' @export
batch_files <- function(path = ".", processed = NA) {
  batch_check(path)
  check_scalar(processed, c(TRUE, NA))
  
  args <- read_args(path)
  files <- .batch_files(path, args$pattern, args$recursive)
  if(is.na(processed)) return(files)
  times <- vapply(files, file.mtime, sys_time())
  if(isFALSE(processed)) return(files[times <= args$time])
  files[times > args$time]
}
