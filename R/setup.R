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
#' @export
batch_setup <- function(FUN, path = ".", pattern = ".*", recursive = FALSE, ...) {
  chk_function(FUN)
  check_directory(path)
  chk_string(pattern)
  chk_flag(recursive)
  
  files <- .batch_files(path, pattern, recursive)
  if(!length(files)) err("no files match '", pattern, "'")

  narg_files <- length(arg_files(path, recursive))
  if(narg_files) {
    err("there are existing '", .argsbatchr, "' files")
  }
  dots <- list(...)
  save_args(path, pattern, recursive, FUN = FUN, dots = dots)
  invisible(files)
}
