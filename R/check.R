#' Check Batch Setup
#'
#' @inheritParams batch_setup
#'
#' @return An invisible copy of the values of .argsbatchr.rds.
#' @export
batch_check <- function(path = ".") {
  check_directory(path)
  
  args <- read_args(path)
  
  files <- .batch_files(path, args$pattern, args$recursive)
  if(!length(files)) err("no files match '", args$pattern, "'")
  
  narg_files <- length(arg_files(path, recursive = TRUE))
  if(narg_files > 1) {
    err(cn(narg_files - 1, "subdirectories contain %n '", .argsbatchr, 
           "' file%s (use batch_reset() to remove)"))
  }
 invisible(args)
}
