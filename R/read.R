#' Batch Read Setup
#' 
#' Reads the arguments to batchr that were stored in the 
#' .batchr_setup.rds file when the project was setup.
#'
#' @inheritParams batch_setup
#'
#' @return A named list of the argument values.
#' @export
batch_read_setup <- function(path) {
  check_directory(path)
  read_batchr_setup.rds(path)
}
