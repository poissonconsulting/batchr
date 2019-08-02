#' Batch Process
#' 
#' Performs set up, processing and clean up.
#'
#' @inheritParams batch_config
#' @inheritParams batch_start
#'
#' @return A flag specifying whether it was able to process.
batch_process <- function(FUN, path = ".", pattern = ".*", recursive = FALSE, 
                      ..., parallel = TRUE) {
  batch_config(FUN, path = path, pattern = pattern, recursive = recursive, ...)
#  batch_start(path, parallel = parallel)  
  batch_cleanup(path)
  TRUE
}
