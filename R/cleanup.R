#' Cleanup Batch Processing
#'
#' Deletes configuration file created by [batch_config()]
#' and log file created by [batch_run()].
#'
#' The [batch_completed()] function can be used to test
#' if batch processing is complete.
#'
#' @inheritParams batch_config
#' @inheritParams batch_run
#' @param force A flag specifying whether to delete configuration and
#' log files even if there are files remaining to be processed.
#' @param remaining A flag specifying whether to delete
#' any files that are remaining to be processed
#' (only applied when `force = TRUE`).
#' Files that have been processed are never deleted.
#' @param recursive A flag specifying whether to recurse into subdirectories
#' when cleaning up. This is unrelated to the `recurse` option
#' of [batch_config()] and is only expected to be used
#' if the user has neglected to clean up multiple nested directories.
#' @param silent A flag specifying whether to suppress warnings (and messages).
#' @return A named logical vector indicating which directories
#' were successfully cleaned up.
#' @seealso [batch_process()]
#' @export
#' @examples
#' path <- tempdir()
#' write.csv(mtcars, file.path(path, "file1.csv"))
#' batch_config(function(x) TRUE, path, regexp = "[.]csv$")
#' batch_run(path, ask = FALSE)
#' batch_cleanup(path)
#' unlink(file.path(path, "file1.csv"))
batch_cleanup <- function(path, force = FALSE,
                          remaining = FALSE, failed = NA,
                          recursive = FALSE,
                          silent = FALSE) {
  chk_dir(path)
  chk_flag(force)
  chk_flag(remaining)
  chk_lgl(failed)
  chk_flag(recursive)

  files <- config_files(path, recursive = recursive)
  if (!length(files)) {
    return(invisible(.named_logical0))
  }
  files <- dirname(files)
  paths <- file.path(path, files)
  clean <- vapply(paths, cleanup_config, TRUE,
    force = force,
    remaining = remaining, failed = failed
  )
  names(clean) <- files
  nfailed <- sum(!clean)
  if(nfailed > 0 && !silent) {
    wrn("Clean up of %n file%s failed", n = nfailed)
  }
  invisible(clean)
}
