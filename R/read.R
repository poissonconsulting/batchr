#' Read Configuration File
#' 
#' Reads the values in the 
#' configuration file created by \code{\link{batch_config}()}.
#'
#' @inheritParams batch_config
#' @return A named list of the configuration values.
#' @seealso \code{\link{batch_process}()} and \code{\link{batch_log_read}()}
#' @export
batch_config_read <- function(path) {
  chk_dir(path)
  file <- file.path(path, ".batchr.rds")
  chk_file(file)
  readRDS(file)
}

#' Read Log File
#' 
#' Reads the values in the log file created by \code{\link{batch_run}()}.
#'
#' @inheritParams batch_config
#' @return A tibble with four columns:
#' \describe{
#'   \item{type}{A character vector indicating SUCCESS or FAILURE}
#'   \item{time}{A POSIXct vector of the time of processing in UTC}
#'   \item{file}{A character vector of the file name}
#'   \item{error}{A character vector of the error message (or NA if no error)}
#' }
#' @seealso \code{\link{batch_process}()} and \code{\link{batch_config_read}()}
#' @export
batch_log_read <- function(path = ".") {
  batch_config_read(path)
  logged_data(path)
}
