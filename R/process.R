#' Batch Process
#' 
#' Performs set up, processing and clean up.
#'
#' @inheritParams batch_setup
#' @inheritParams batch_start
#'
#' @return A flag specifying whether it was able to process.
#' @export
batch_process <- function(FUN, path = ".", pattern = ".*", recursive = FALSE, 
                      ..., parallel = TRUE) {
  batch_setup(FUN, path = path, pattern = pattern, recursive = recursive, ...)
  batch_start(path, parallel = parallel)  
  batch_cleanup(path)
  TRUE
}
