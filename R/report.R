#' Batch Report
#'
#' Outputs a report of the status of individual files to the console.
#'
#' @inheritParams batch_config
#' @seealso [batch_file_status()]
#' @export
batch_report <- function(path) {
  status <- batch_file_status(path)

  report_files(status)
  report_types(status)
  invisible()
}
