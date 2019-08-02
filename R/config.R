#' Configure Batch Processing
#' 
#' Configures a directory of files for batch processing.
#' As well as the directory and a regular expression to identify files the
#' user must also provide the function that will be used to process each file.
#' Processing is started (or restarted) using \code{\link{batch_start}()}
#' which locks the configuration file with \code{\link[filelock]{lock}} 
#' to prevent concurrent calls.
#' 
#' The configuration details are saved in the '.batchr.rds file'
#' which can be read using \code{\link{batch_read_config}()}.
#' Configuration is only possible if the directory does not already contain
#' a configuration file. If \code{recursive = TRUE} then the subdirectories
#' must also not contain configuration files.
#' Existing configuration files can be listed using \code{\link{batch_config_files}()}.
#' 
#' The pattern must match at least one non-hidden file in the directory 
#' (or if \code{recursive = TRUE} in the directory or subdirectories).
#' Hidden files are excluded to prevent accidental modification of system files.
#' 
#' The FUN function's first argument should be a string of the path to a single file.
#' The function should return TRUE if processing was successful or FALSE
#' if processing was unsuccessful. 
#' If the function returns FALSE the failure is automatically logged as an INFO 
#' message in the '.batchr.log' file.
#' 
#' Processing is also considered to have failed if 
#' the function throws an error. 
#' In this case the error is caught, the failure is logged as an ERROR message 
#' and batch processing resumes at the next file.
#' If, however, the function returns an object other than 
#' a flag then a FATAL message is logged and processing 
#' ceases immediately with batchr throwing an error.
#' 
#' All failures are logged in the '.batchr.log' file with the level (WARN, ERROR, FATAL),
#' system time in UTC and file name using a \code{\link[log4r]{logger}} object.
#' The '.batchr.log' file can be parsed using 
#' 
#' When calling \code{\link{batch_start}()} 
#' the user can provide their own \code{\link[log4r]{logger}} object which is
#' passed to the FUN function to allow them to log custom messages.
#' In this case the FUN function must have an argument called logger.
#'
#' @param FUN A function to process each of the files. 
#' @param path A string of the path to the directory with the files.
#' @param pattern A string of a regular expression. Only non-hidden file names 
#' which match the regular expression will be batch processed.
#' @param recursive A flag specifying whether to recurse into sub directories.
#' @param ... Additional arguments passed to FUN.
#'
#' @return An invisible character vector of the paths to the files to
#' be processed.
#' @seealso \code{\link{batch_start}()}
#' @export
batch_config <- function(FUN, path = ".", pattern = ".*", recursive = FALSE, ...) {
  chk_function(FUN)
  chk_dir(path)
  chk_string(pattern)
  chk_flag(recursive)
  
  if(length(batch_config_files(path, recursive = FALSE)))
    err("directory '", path, "' already contains a '.batchr.rds' file")

  if(recursive && length(batch_config_files(path, recursive = TRUE)))
    err("subdirectories of '", path, "' contain '.batchr.rds' files")

  files <- list.files(path = path, pattern = pattern, recursive = recursive)
  
  if(!length(files)) { 
    err("directory '", path, "' does not contain any files matching '", 
        pattern, "'")
  }
  dots <- list(...)
  save_config(path, pattern, recursive, FUN = FUN, dots = dots)
  invisible(files)
}
