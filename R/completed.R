#' Batch Completed?
#'
#' Tests if there are any remaining files to process as listed
#' by [batch_files_remaining()].
#'
#' By default, files that previously failed to process are excluded.
#'
#' @inheritParams batch_config
#' @inheritParams batch_run
#' @return A flag specifying whether batch processing is complete.
#' @seealso [batch_process()]
#' @export
batch_completed <- function(path, failed = FALSE) {
  length(batch_files_remaining(path = path, failed = failed)) == 0L
}
