#' Configure Batch Processing
#' 
#' Configures a directory for batch file processing by \code{\link{batch_run}()}.
#' 
#' \code{batch_config()} creates a hidden configuration file in path 
#' named '.batchr.rds'. 
#' 
#' The contents of the file can be read using
#' \code{\link{batch_config_read}()} 
#' or updated using \code{\link{batch_config}()}.
#' 
#' @param fun A function to process each of the files. 
#' \code{fun}'s first argument should be a string of the path to a single file.
#' If processing is unsuccessful \code{fun} should return FALSE 
#' or throw an error (error messages are automatically logged).
#' @param path A string of the path to the directory with the files for processing.
#' @param regexp A string of a regular expression. Only non-hidden file names 
#' which match the regular expression will be batch processed.
#' @param recurse A flag specifying whether to recurse into path's subdirectories.
#' @param ... Additional arguments passed to \code{fun}.
#'
#' @return An invisible character vector of the paths to the files to
#' that will be processed when \code{\link{batch_run}()} is called.
#' @seealso \code{\link{batch_process}()}
#' @export
batch_config <- function(fun, path = ".", regexp = ".*", recurse = FALSE, ...) {
  chk_function(fun)
  chk_dir(path)
  chk_string(regexp)
  chk_flag(recurse)
  
  if(length(config_files(path, recursive = FALSE)))
    err("Directory '", path, "' already contains a '.batchr.rds' file.")

  if(recurse && length(config_files(path, recursive = TRUE)))
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

#' Updates Batch Processing Configuration
#' 
#' Updates a directory that was configured using \code{\link{batch_config}()}.
#' 
#' @inheritParams batch_config
#'
#' @return An invisible character vector of the paths to the files 
#' (failed and untested) remaining to be processed.
#' @seealso \code{\link{batch_process}()}
#' @export
batch_config_update <- function(fun, path = ".", ...) {
  chk_function(fun)
  chk_dir(path)

  config <- batch_config_read(path)
  recurse <- config$recurse
  regexp <- config$regexp

  if(recurse && length(config_files(path, recursive = TRUE)) > 1L)
    err("Subdirectories of '", path, "' contain '.batchr.rds' files.")
  
  dots <- list(...)
  save_config(path, regexp, recurse, fun = fun, dots = dots)
  invisible(batch_remaining_files(path, failed = NA))
}
