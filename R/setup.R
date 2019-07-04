#' Setup Batch Processing of Files
#'
#' @param FUN A function to process the files. The first argument should be a string of the absolute path to a single file.
#' @param path A string of the path to the directory with the files.
#' @param pattern A string of the regular expression to match to file names.
#' @param recursive A flag specifying whether to recurse into sub directories.
#' @param ... Additional arguments passed to FUN.
#'
#' @return An invisible character vector of the paths to the files to
#' be processed.
#' @export
batch_setup <- function(FUN, path = ".", pattern = ".*", recursive = FALSE, ...) {
  check_function(FUN)
  check_directory(path)
  check_string(pattern)
  check_flag(recursive)
  
  files <- .batch_files(path, pattern, recursive)
  if(!length(files)) err("no files match '", pattern, "'")

  narg_files <- length(arg_files(path, recursive))
  if(narg_files) {
    err("there are existing '", .argsbatchr, "' files (use batch_reset() to remove)")
  }
  dots <- list(...)
  save_args(path, pattern, recursive, FUN = FUN, dots = dots)
  invisible(files)
}
