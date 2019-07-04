#' Batch Process
#' 
#' Performs set up, processing and clean up.
#' It can be halted during processing and restarted using \code{\link{batch_restart}()}
#'
#' @inheritParams batch_setup
#' @inheritParams batch_restart
#'
#' @return An invisible character vector of the processed files.
#' @export
batch_process <- function(FUN, path = ".", pattern = ".*", recursive = FALSE, 
                      ..., parallel = TRUE) {
  batch_setup(FUN, path, pattern, recursive = recursive, ...)
  batch_process(path, parallel = parallel)  
  batch_cleanup(path)
}
