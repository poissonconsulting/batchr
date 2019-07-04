#' Batch Reset
#'
#' @inheritParams batch_check
#' @param recursive A logical scalar specifying whether to leave the recursive
#' setting unchanged (default) or reset it to be TRUE or FALSE.
#'
#' @return An invisible character vector of the paths to the files to
#' be processed.
#' @export
batch_reset <- function(path, recursive = NA) {
  check_directory(path)
  args <- read_args(path)
  
  if(is.na(recursive)) recursive <- args$recursive
  
  args <- arg_files(path, recursive = recursive)
  file.remove(args)
  
  save_args(path, pattern = args$pattern, recursive = recursive, 
            FUN = args$FUN, dots = args$dots)
  invisible(.batch_files(path, pattern = args$pattern, recursive = recursive))
}
