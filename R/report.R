#' Batch Report
#'
#' Outputs a report of the status of individual files to the console.
#'
#' @inheritParams batch_config
#' @return An invisible NULL. The function is called for its side-effect of 
#' outputting a report of the status of individual files to the console.
#' @seealso [batch_file_status()]
#' @export
#' @examples 
#' path <- tempdir()
#' write.csv(mtcars, file.path(path, "file1.csv"))
#' batch_config(function(x) TRUE, path, regexp = "[.]csv$",)
#' batch_report(path)
#' batch_run(path, ask = FALSE)
#' batch_report(path)
#' batch_cleanup(path)
#' unlink(file.path(path, "file1.csv"))
batch_report <- function(path) {
  status <- batch_file_status(path)

  report_files(status)
  report_types(status)
  invisible()
}
