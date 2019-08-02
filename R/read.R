#' Read Configuration File
#' 
#' Reads the values in the .batchr.rds configuration file which was created 
#' when the project was set up.
#'
#' @inheritParams batch_config
#' @return A named list of the argument values.
#' @seealso \code{\link{batch_config}()}.
#' @export
batch_read_config <- function(path) {
  chk_dir(path)
  file <- file.path(path, ".batchr.rds")
  chk_file(file)
  readRDS(file)
}
