#' Batch File Processing
#'
#' Performs batch processing of files in a directory using the
#' \code{\link{batch_config}()}, \code{\link{batch_run}()}
#' and \code{\link{batch_cleanup}()} functions.
#' For more control the user should call these three functions instead.
#'
#' \code{\link{batch_gsub}()} provides a simple wrapper for
#' batch text replacement.
#'
#' @inheritParams batch_config
#' @inheritParams batch_run
#' @inheritParams batch_cleanup
#'
#' @seealso \code{\link{batch_config}()}, \code{\link{batch_run}()}
#' and \code{\link{batch_cleanup}()}
#' @return An invisible flag indicating whether all the files where
#' successfully processed.
#' @export
batch_process <- function(fun, path = ".", regexp = ".*", recurse = FALSE,
                          progress = FALSE, force = TRUE,
                          seeds = NULL,
                          options = furrr::future_options(),
                          ask = getOption("batchr.ask", TRUE), ...) {
  batch_config(fun, path = path, regexp = regexp, recurse = recurse, ...)
  success <- batch_run(
    path = path, progress = progress,
    seeds = seeds, options = options, ask = ask
  )
  batch_cleanup(path, force = force)
  all(success)
}
