#' Batch Summary
#'
#' Writes a summary of individual file status etc to the console.
#'
#' @inheritParams batch_config
#' @seealso \code{\link{batch_file_status}()}
#' @export
batch_summary <- function(path = ".") {
  status <- batch_file_status(path)

  summary_files(status)
  summary_types(status)
  invisible()
}
