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
#' @param error_msgs A flag specifying whether to include error messages.
#' @return A tibble with an ordered factor of the \code{level} (WARN, ERROR, FATAL), 
#' a POSIXct of the time of failure in UTC, a character vector of the file name
#' and optionally a character vector of the error message (or NA if no error).
#' @seealso \code{\link{batch_process}()}
#' @export
batch_read_log <- function(path = ".", error_msgs = FALSE) {
  chk_flag(error_msgs)
  batch_read_config(path) # checks configuration file exists
  logged_data(path)
}
