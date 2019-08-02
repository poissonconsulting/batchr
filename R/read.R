#' Batch Read Setup
#' 
#' Reads the values in the .batchr.rds file which was created 
#' when the project was setup.
#'
#' @inheritParams batch_setup
#'
#' @return A named list of the argument values.
#' @export
batch_read_setup <- function(path) {
  chk_dir(path)
  file <- file.path(path, ".batchr.rds")
  chk_file(file)
  readRDS(file)
}
