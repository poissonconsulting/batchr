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

#' Read Log File
#' 
#' Reads the values in the .batchr.log processing failure file.
#'
#' @inheritParams batch_config
#' @return A tibble with an ordered factor of the \code{level} (WARN, ERROR, FATAL), 
#' a POSIXct of the time of failure in UTC and a character vector of the file name.
#' @seealso \code{\link{batch_config}()}.
#' @export
batch_read_log <- function(path = ".") {
  batch_read_config(path) # checks configuration file exists
  file <- file.path(path, ".batchr.log")
  if(!file.exists(file)) return(logged_data(character(0)))
  lines <- readLines(file)
  logged_data(lines)
}
