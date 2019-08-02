#' Set Up Batch Processing of Files
#' 
#' Sets up files for batch processing.
#' 
#' Hidden files are not included in those for batch processing to
#' prevent accidental modification of system files.
#'
#' @param FUN A function to process the files. 
#' The first argument should be a string of the absolute path to a single file.
#' @param path A string of the path to the directory with the files.
#' @param pattern A string of a regular expression. Only non-hidden file names 
#' which match the regular expression will be batch processed.
#' @param recursive A flag specifying whether to recurse into sub directories.
#' @param ... Additional arguments passed to FUN.
#'
#' @return An invisible character vector of the paths to the files to
#' be processed.
#' @seealso \code{\link{batch_read_setup}()}
#' @export
batch_setup <- function(FUN, path = ".", pattern = ".*", recursive = FALSE, ...) {
  chk_function(FUN)
  chk_dir(path)
  chk_string(pattern)
  chk_flag(recursive)
  
  if(length(batch_setup_files(path, recursive = FALSE)))
    err("directory '", path, "' already contains a '.batchr_setup.rds' file")

  if(recursive && length(batch_setup_files(path, recursive = FALSE)))
    err("subdirectories of '", path, "' contain '.batchr_setup.rds' files")

  files <- list.files(path = path, pattern = pattern, recursive = recursive)
  
  if(!length(files)) { 
    err("directory '", path, "' does not contain any files matching '", 
        pattern, "'")
  }
  dots <- list(...)
  save_batchr_setup(path, pattern, recursive, FUN = FUN, dots = dots)
  invisible(files)
}
