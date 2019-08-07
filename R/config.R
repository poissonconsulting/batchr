#' Configure Batch Processing
#' 
#' Configures a directory for batch file processing by \code{\link{batch_run}()}.
#' 
#' \code{batch_config()} creates a hidden configuration file in path 
#' named '.batchr.rds'. 
#' 
#' The contents of the file can be read using
#' \code{\link{batch_config_read}()} 
#' or updated using \code{\link{batch_config_update}()}.
#' 
#' Configuration is only possible if the directory does not already contain
#' a configuration file. 
#' If \code{recurse = TRUE} then the subdirectories
#' must also not contain configuration files.
#' 
#' The regexp must match at least one non-hidden file in the directory 
#' or if \code{recurse = TRUE} in the directory or subdirectories.
#' Hidden files are excluded to prevent accidental modification of system files.
#' 
#' @param fun A function to process each of the files. 
#' \code{fun}'s first argument should be a string of the path to a single file.
#' If processing is unsuccessful \code{fun} should return FALSE 
#' or throw an error (error messages are caught and automatically logged).
#' @param path A string of the path to the directory with the files for processing.
#' @param regexp A string of a regular expression. Only non-hidden file names 
#' which match the regular expression will be batch processed.
#' @param recurse A flag specifying whether to recurse into path's subdirectories.
#' @param ... Additional arguments passed to \code{fun}.
#'
#' @return An invisible character vector of the paths to the files to
#' that will be processed when \code{\link{batch_run}()} is called.
#' @seealso \code{\link{batch_process}()} and \code{\link{batch_run}()}
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
  cleanup_log_file(path)
  save_config(path, regexp, recurse, fun = fun, dots = dots)
  invisible(files)
}

#' Updates Batch Processing Configuration
#' 
#' Updates the function that was provided when a directory was configured 
#' using \code{\link{batch_config}()}.
#' 
#' \code{batch_config_update()} is useful if a new version of the files is required
#' to successfully process some of the files.
#' 
#' It should be noted that \code{batch_config_update()} does not alter the 
#' configuration time.
#' 
#' In order to process previously failed files \code{\link{batch_run}()}
#' should be called with \code{failed = NA} or \code{failed = TRUE}.
#' 
#' @inheritParams batch_config
#'
#' @return An invisible character vector of the paths to the files 
#' remaining to be processed.
#' @seealso \code{\link{batch_process}()} and \code{\link{batch_config}()}
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
  invisible(batch_files_remaining(path, failed = NA))
}
