#' Configure Batch Processing
#' 
#' Configures a directory for batch file processing.
#' 
#' @param fun A function to process each of the files. 
#' @param path A string of the path to the directory with the files.
#' @param regexp A string of a regular expression. Only non-hidden file names 
#' which match the regular expression will be batch processed.
#' @param recurse A flag specifying whether to recurse into sub directories.
#' @param ... Additional arguments passed to fun.
#'
#' @return An invisible character vector of the paths to the files to
#' be processed.
#' @seealso \code{\link{batch_process}()}
#' @export
batch_config <- function(fun, path = ".", regexp = ".*", recurse = FALSE, ...) {
  chk_function(fun)
  chk_dir(path)
  chk_string(regexp)
  chk_flag(recurse)
  
  if(length(batch_config_files(path, recurse = FALSE)))
    err("Directory '", path, "' already contains a '.batchr.rds' file.")

  if(recurse && length(batch_config_files(path, recurse = TRUE)))
    err("Subdirectories of '", path, "' contain '.batchr.rds' files.")

  files <- list.files(path = path, pattern = regexp, recursive = recurse)
  
  if(!length(files)) { 
    err("Directory '", path, "' does not contain any files matching '", 
        regexp, "'.")
  }
  dots <- list(...)
  cleanup_log_files(path)
  save_config(path, regexp, recurse, fun = fun, dots = dots)
  invisible(files)
}

#' Reconfigures Batch Processing
#' 
#' Updates the batch processing function in the configuration file.
#' 
#' @inheritParams batch_config
#'
#' @return An invisible character vector of the paths to the files 
#' (failed and untested) remaining to be processed.
#' @seealso \code{\link{batch_process}()}
#' @export
batch_reconfig <- function(fun, path = ".", ...) {
  chk_function(fun)
  chk_dir(path)

  config <- batch_read_config(path)
  recurse <- config$recurse
  regexp <- config$regexp

  if(recurse && length(batch_config_files(path, recurse = TRUE)) > 1L)
    err("Subdirectories of '", path, "' contain '.batchr.rds' files.")
  
  dots <- list(...)
  save_config(path, regexp, recurse, fun = fun, dots = dots)
  invisible(batch_remaining_files(path, failed = NA))
}
