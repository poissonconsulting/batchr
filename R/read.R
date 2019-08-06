#' Read Configuration File
#' 
#' Reads the values in the .batchr.rds configuration file which was created 
#' when the project was set up.
#'
#' @inheritParams batch_config
#' @return A named list of the argument values.
#' @seealso \code{\link{batch_process}()}
#' @export
batch_read_config <- function(path) {
  chk_dir(path)
  file <- file.path(path, ".batchr.rds")
  chk_file(file)
  readRDS(file)
}

#' Read Log File
#' 
#' Reads the values in the .batchr.log processing failure file.
#'
#' @inheritParams batch_config
#' @return A tibble with four columns.
#' type is a character vector indicating SUCCESS or FAILURE,
#' time is a POSIXct of the datetime of processing in UTC, 
#' file is a character vector of the file name and 
#' error is a character vector of the error message (or NA if no error).
#' @seealso \code{\link{batch_process}()}
#' @export
batch_read_log <- function(path = ".") {
  batch_read_config(path) # checks configuration file exists
  logged_data(path)
}
