#' Configure Batch Processing
#' 
#' Configures a directory for batch file processing.
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
#' @seealso \code{\link{batch_process}()}
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
  cleanup_log_file(path)
  save_config(path, pattern, recursive, FUN = FUN, dots = dots)
  invisible(files)
}
