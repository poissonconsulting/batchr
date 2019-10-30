#' Batch File Processing
#'
#' Performs batch processing of files in a directory using the
#' [batch_config()], [batch_run()]
#' and [batch_cleanup()] functions.
#' For more control the user should call these three functions instead.
#'
#' @inheritParams batch_config
#' @inheritParams batch_run
#' @inheritParams batch_cleanup
#' @param report A flag specifying whether to outputs a report of the status of individual files to the console.
#'
#' @seealso [batch_config()], [batch_run()]
#' and [batch_cleanup()]
#' @return An invisible flag indicating whether all the files where
#' successfully processed.
#' @export
batch_process <- function(fun, path, regexp = ".*", recurse = FALSE,
                          progress = FALSE, force = TRUE, report = TRUE,
                          seeds = NULL,
                          options = furrr::future_options(),
                          ask = getOption("batchr.ask", TRUE),
                          ...) {
  chk_flag(report)
  batch_config(fun, path = path, regexp = regexp, recurse = recurse, ...)
  success <- batch_run(
    path = path, progress = progress,
    seeds = seeds, options = options, ask = ask
  )
  if(report) batch_report(path)
  batch_cleanup(path, force = force)
  invisible(all(success))
}
