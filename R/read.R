#' Batch Read Setup
#' 
#' Reads the values in the .batchr_setup.rds file which was created 
#' when the project was setup.
#'
#' @inheritParams batch_setup
#'
#' @return A named list of the argument values.
#' @export
batch_read_setup <- function(path) {
  chk_dir(path)
  file <- file.path(path, .batchr_setup)
  chk_file(file)
  readRDS(file)
}
