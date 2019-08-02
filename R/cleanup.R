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
#' @return An invisible character vector of the directories 
#' that were successfully cleaned up.
#' @seealso \code{\link{batch_process}()}
#' @export
batch_cleanup <- function(path = ".", recursive = FALSE, force = FALSE, 
                          remaining = FALSE, failed = NA, silent = FALSE) {
  chk_flag(recursive)
  chk_flag(force)
  chk_lgl(failed)
  
  configs <- batch_config_files(path, recursive = recursive)
  if(!length(configs)) return(character(0))
  configs <- lapply(configs, dirname)
  clean <- lapply(configs, cleanup_config, force = force, 
                      remaining = remaining, failed = failed)
  clean <- unlist(clean)
  configs[clean]
}
