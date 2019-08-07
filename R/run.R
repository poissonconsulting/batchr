#' Runs Batch Processing
#' 
#' Starts (or restarts if previously stopped) processing the remaining files
#' specified by \code{\link{batch_config}()}.
#' 
#' \code{batch_run()} logs all file processing attempts together with the
#' the type (SUCCESS or FAILURE), the 
#' system time in UTC, the file name and any error messages.
#' The hidden log file can be read using \code{\link{batch_log_read}()}.
#' 
#' \code{\link{batch_files_remaining}()} provides a vector of the files that
#' are remaining to be processed.
#' 
#' When processing is complete the hidden configuration file
#' and hidden log file can be deleted using \code{\link{batch_cleanup}()}.
#' 
#' If a remaining file is removed or modified by a separate process,
#' \code{batch_run()} throws an error.
#'
#' @inheritParams batch_config
#' @param failed A logical scalar specifying how to treat files 
#' that previously failed to process. If FALSE (the default) failed files 
#' are excluded, if NA they are included and if TRUE they are only included.
#' @param parallel A flag specifying whether to process the files in 
#' parallel (not yet used).
#' @param progress A logic scalar specifying whether to 
#' write logging information to the console (TRUE or FALSE) or only to write
#' logging information to the console for failed attempts (NA).
#' @param ask A flag specifying whether to ask before starting to process the files.
#' @return An invisible named logical vector indicating for each file
#' whether it was successfully processed.
#' @seealso \code{\link{batch_process}()}, \code{\link{batch_config}()} and 
#' \code{\link{batch_cleanup}()} 
#' @export
batch_run <- function(path = ".", 
                      failed = FALSE, parallel = FALSE, 
                      progress = !parallel, 
                      ask = getOption("batchr.ask", TRUE)) {
  chk_dir(path)
  chk_lgl(failed)
  chk_flag(parallel)
  chk_lgl(progress)
  chk_flag(ask)

  config <- batch_config_read(path)
  
  recurse <- config$recurse
  fun <- config$fun
  dots <- config$dots
  
  if(recurse && length(config_files(path = path, recursive = recurse)) > 1)
    err("Subdirectories of '", path, "' contain '.batchr.rds' files.")
  
  remaining <- batch_files_remaining(path, failed = failed)
  if(!length(remaining)) return(structure(logical(0), .Names = character(0)))
  question <- p0("Batch process ", length(remaining), " files in '", 
                 normalizePath(path), "'?")
  if(ask && !yesno(question))
    return(invisible(set_names(rep(FALSE, length(remaining)), remaining)))
  
  success <- process_files(remaining, fun = fun, dots = dots, 
                    path = path, config_time = config$time,
                    parallel = parallel, progress = progress)

  invisible(success)
}
