#' Batch Completed?
#' 
#' Tests if there are any remaining files to batch process.
#' 
#' By default, files that were not successfully processed are 
#' excluded.
#'
#' @inheritParams batch_config
#' @inheritParams batch_run
#' @return A flag specifying whether batch processing is complete.
#' @seealso \code{\link{batch_process}()}
#' @export
batch_completed <- function(path = ".", failed = FALSE) {
  length(batch_remaining_files(path = path, failed = failed)) == 0L
}