#' Cleanup Batch Processing
#' 
#' Removes configuration and log files and optionally unprocessed files.
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
#' @return A named logical vector indicating which directories 
#' were successfully cleaned up.
#' @seealso \code{\link{batch_process}()}
#' @export
batch_cleanup <- function(path = ".", recurse = FALSE, force = FALSE, 
                          remaining = FALSE, failed = NA, silent = FALSE) {
  chk_flag(recurse)
  chk_flag(force)
  chk_lgl(failed)
  
  files <- batch_config_files(path, recurse = recurse)
  if(!length(files)) return(structure(logical(0), .Names = character(0)))
  files <- dirname(files)
  paths <- file.path(path, files)
  clean <- vapply(paths, cleanup_config, TRUE, force = force, 
                      remaining = remaining, failed = failed)
  names(clean) <- files
  invisible(clean)
}
