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
#' @examples 
#' path <- tempdir()
#' write.csv(mtcars, file.path(path, "file1.csv"))
#' batch_config(function(x) TRUE, path, regexp = "[.]csv$")
#' batch_completed(path)
#' batch_run(path, ask = FALSE)
#' batch_completed(path)
#' batch_cleanup(path)
#' unlink(file.path(path, "file1.csv"))
batch_completed <- function(path, failed = FALSE) {
  length(batch_files_remaining(path = path, failed = failed)) == 0L
}
