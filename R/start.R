#' Start Batch Processing
#' 
#' Starts (or restarts if previously stopped) batch processing
#' files of files as specified in configuration file.
#' 
#' It locks the configuration file during processing to prevent
#' concurrent calls to batch_start().
#'
#' @inheritParams batch_config
#' @param parallel A flag specifying whether to process the files in 
#' parallel (not yet used).
#' @param failed A logical scalar specifying whether to exclude (FALSE),
#' include (NA), or only include (TRUE) files that failed to process.
#'
#' @return An invisible character vector of the files successfully 
#' processed by the current call.
#' @export
batch_start <- function(path = ".", failed = FALSE, parallel = FALSE) {
  chk_dir(path)
  chk_flag(failed)
  chk_flag(parallel)
  
  if(parallel) .NotYetUsed("parallel", error = FALSE) 
  
  config <- batch_read_config(path)
  
  if(!lock_config(path))
    err("file '", file.path(path, ".batchr.rds"), "' is already locked")
  
  recursive <- config$recursive
  if(recursive && length(batch_config_files(path = path, recursive)))
    err("subdirectories of '", path, "' contain '.batchr.rds' files")
  
  remaining <- batch_remaining_files(path, failed = failed)
  if(!length(remaining)) return(character(0))
  
  # need to do magic
}
