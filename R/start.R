#' Start Batch Processing
#' 
#' Starts (or restarts if previously stopped) processing the files
#' as specified in the configuration file.
#' For more information see \code{\link{batch_config}()}
#'
#' @inheritParams batch_config
#' @param parallel A flag specifying whether to process the files in 
#' parallel (not yet used).
#' @param failed A logical scalar specifying whether to exclude (FALSE),
#' include (NA), or only include (TRUE) files that have thus far failed to process.
#' @param logger A \code{\link[log4r]{logger}} object that is passed to 
#' the file processing function to allow the user to log their own messages.
#'
#' @return An invisible character vector of the files successfully 
#' processed by the current call.
#' @export
batch_start <- function(path = ".", failed = FALSE, parallel = FALSE, logger = NULL) {
  chk_dir(path)
  chk_flag(failed)
  chk_flag(parallel)
  if(!is.null(logger)) chk_is(logger, "logger")
  
  if(parallel) .NotYetUsed("parallel", error = FALSE) 
  
  config <- batch_read_config(path)
  
  recursive <- config$recursive
  fun <- config$FUN
  dots <- config$dots
  
  if(!lock_config(path))
    err("file '", file.path(path, ".batchr.rds"), "' is already locked")
  
  if(recursive && length(batch_config_files(path = path, recursive)) > 1)
    err("subdirectories of '", path, "' contain '.batchr.rds' files")
  
  remaining <- batch_remaining_files(path, failed = failed)
  if(!length(remaining)) return(character(0))
  success <- lapply(remaining, process_file, fun = fun, dots = dots, 
                    path = path, logger = logger)
  success <- unlist(success)
  remaining[success]
}
