#' Batch File Status
#'
#' Gets the current status (SUCCESS, FAILURE, REMAING) of each
#' eligible file in path.
#'
#' @inheritParams batch_config
#' @return A tibble with four columns:
#' \describe{
#'   \item{type}{A character vector indicating SUCCESS,  FAILURE or REMAING}
#'   \item{time}{A hms vector of the file processing time}
#'   \item{file}{A character vector of the file name}
#'   \item{error}{A character vector of the error message (or NA if no error)}
#' }
#' @seealso [batch_log_read()]
#' @export
#' @examples
#' path <- tempdir()
#' write.csv(mtcars, file.path(path, "file1.csv"))
#' batch_config(function(x) TRUE, path, regexp = "[.]csv$")
#' batch_file_status(path)
#' batch_run(path, ask = FALSE)
#' batch_file_status(path)
#' batch_cleanup(path)
#' unlink(file.path(path, "file1.csv"))
batch_file_status <- function(path) {
  log <- batch_log_read(path)
  log <- log[!duplicated(log$file, fromLast = TRUE), ]
  remaining <- batch_files_remaining(path, failed = FALSE)
  nremaing <- length(remaining)
  nremaing <- tibble(type = rep("REMAING", nremaing),
    time = hms(rep(NA, nremaing)),
    file = remaining,
    message = rep(NA_character_, nremaing))
  log <- rbind(log, nremaing)
  log <- log[order(log$file), ]
  log
}
