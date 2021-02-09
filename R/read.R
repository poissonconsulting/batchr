#' Read Configuration File
#'
#' Reads the values in the
#' configuration file created by [batch_config()].
#'
#' @inheritParams batch_config
#' @return A named list of the configuration values.
#' @seealso [batch_process()] and [batch_log_read()]
#' @export
#' @examples 
#' path <- tempdir()
#' write.csv(mtcars, file.path(path, "file1.csv"))
#' batch_config(function(x) TRUE, path, regexp = "[.]csv$")
#' batch_config_read(path)
#' batch_cleanup(path, force = TRUE, remaining = TRUE)
#' unlink(file.path(path, "file1.csv"))
batch_config_read <- function(path) {
  chk_dir(path)
  file <- file.path(path, ".batchr.rds")
  if (!vld_file(file)) {
    abort_chk("directory path ('", path, "') must contain file '.batch.rds'")
  }
  readRDS(file)
}

#' Read Log File
#'
#' Reads the values in the log file created by [batch_run()].
#'
#' @inheritParams batch_config
#' @return A tibble with four columns:
#' \describe{
#'   \item{type}{A character vector indicating SUCCESS or FAILURE}
#'   \item{time}{A hms vector of the file processing time}
#'   \item{file}{A character vector of the file name}
#'   \item{error}{A character vector of the error message (or NA if no error)}
#' }
#' @seealso [batch_process()] and [batch_config_read()]
#' @export
#' @examples 
#' path <- tempdir()
#' write.csv(mtcars, file.path(path, "file1.csv"))
#' batch_config(function(x) TRUE, path, regexp = "[.]csv$")
#' batch_log_read(path)
#' batch_run(path, ask = FALSE)
#' batch_log_read(path)
#' batch_cleanup(path)
#' unlink(file.path(path, "file1.csv"))
batch_log_read <- function(path) {
  batch_config_read(path)
  logged_data(path)
}
