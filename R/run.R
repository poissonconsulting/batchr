#' Runs Batch Processing
#' 
#' Starts (or restarts if previously stopped) processing the files
#' specified in the configuration file.
#'
#' @inheritParams batch_config
#' @param failed A logical scalar specifying whether to exclude (FALSE),
#' include (NA), or only include (TRUE) files that have thus far failed to process.
#' @param parallel A flag specifying whether to process the files in 
#' parallel (not yet used).
#' @param progress A string specifying the level of output to the console. 
#' The possible values are ""all", "failures" or "none".
#' @param ask A flag specifying whether to ask before processing the files.
#' @return An invisible named logical vector indicating for each file
#' whether it was successfully processed.
#' @seealso \code{\link{batch_process}()}
#' @export
batch_run <- function(path = ".", failed = FALSE, parallel = FALSE, 
                      progress = "none", 
                      ask = getOption("batchr.ask", TRUE)) {
  chk_dir(path)
  chk_flag(failed)
  chk_flag(parallel)
  chk_string(progress)
  chk_in(progress, c("all", "failures", "none"))
  chk_flag(ask)

  if(parallel) .NotYetUsed("parallel", error = FALSE) 
  
  config <- batch_read_config(path)
  
  recurse <- config$recurse
  fun <- config$fun
  dots <- config$dots
  
  if(!lock_config(path))
    err("File '", file.path(path, ".batchr.rds"), "' is already locked.")
  
  if(recurse && length(batch_config_files(path = path, recurse)) > 1)
    err("Subdirectories of '", path, "' contain '.batchr.rds' files.")
  
  remaining <- batch_remaining_files(path, failed = failed)
  if(!length(remaining)) return(structure(logical(0), .Names = character(0)))
  question <- p0("Batch process ", length(remaining), " files in '", path, "'?")
  if(ask && !yesno(question))
    return(invisible(set_names(rep(FALSE, length(remaining)), remaining)))

  success <- lapply(remaining, process_file, fun = fun, dots = dots, 
                    path = path, progress = progress)
  success <- unlist(success)
  invisible(set_names(success, remaining))
}
