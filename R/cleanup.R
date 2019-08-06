#' Cleanup Batch Processing
#' 
#' Removes configuration file created by \code{\link{batch_config}()} 
#' and log file created by \code{\link{batch_run}()}.
#'
#' @inheritParams batch_config
#' @param force A flag specifying whether to remove configuration and 
#' log files even if there are files remaining to be processed.
#' @param remaining When \code{force = TRUE} a flag specifying 
#' whether to remove files that have not yet been processed.
#' @param failed When \code{remaining = TRUE} a logical scalar specifying 
#' whether to remove all remaining files (NA), 
#' only those files that failed to process (TRUE),
#' or only untested files (FALSE).
#' @param silent A flag specifying whether to suppress warnings about
#' directories that could not be cleaned.
#' @param recursive A flag specifying whether to recurse into subdirectories
#' when cleaning up.
#' @return A named logical vector indicating which directories 
#' were successfully cleaned up.
#' @seealso \code{\link{batch_process}()}
#' @export
batch_cleanup <- function(path = ".", force = FALSE, 
                          remaining = FALSE, failed = NA, silent = FALSE,
                          recursive = FALSE) {
  chk_flag(recursive)
  chk_flag(force)
  chk_lgl(failed)
  
  files <- batch_config_files(path, recursive = recursive)
  if(!length(files)) return(structure(logical(0), .Names = character(0)))
  files <- dirname(files)
  paths <- file.path(path, files)
  clean <- vapply(paths, cleanup_config, TRUE, force = force, 
                      remaining = remaining, failed = failed)
  names(clean) <- files
  invisible(clean)
}
