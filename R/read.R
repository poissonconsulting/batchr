#' Batch Read Arguments
#' 
#' Reads the arguments to batchr that were stored in the .argsbatchr.rds file
#' when the project was setup.
#'
#' @inheritParams batch_setup
#'
#' @return A named list of the argument values.
#' @export
batch_read_args <- function(path) {
  check_directory(path)
  read_args(path)
}
