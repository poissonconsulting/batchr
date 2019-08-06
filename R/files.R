#' Configuration Files
#'
#' @inheritParams batch_cleanup
#' @return A character vector of the names of the configuration file(s).
#' @seealso \code{\link{batch_process}()}
#' @export
batch_config_files <- function(path = ".", recursive = FALSE) {
  chk_dir(path)
  chk_flag(recursive)

  list.files(path, pattern = "^[.]batchr[.]rds$", recursive = recursive,
             all.files = TRUE)
}

#' Log Files
#'
#' @inheritParams batch_config
#' @return A character vector of the names of the log file(s).
#' @seealso \code{\link{batch_process}()}
#' @export
batch_log_files <- function(path = ".", recurse = FALSE) {
  chk_dir(path)
  chk_flag(recurse)

  list.files(path, pattern = "^[.]batchr[.]log$", recursive = recurse,
             all.files = TRUE)
}

#' Remaining Files
#' 
#' Gets the names of the files that are remaining to be processed by
#' \code{\link{batch_run}()}.
#' 
#' By default, files that previously failed to process are excluded.
#' 
#' \code{\link{batch_completed}()} can be used to easily test if there are any
#' files remaining.
#'
#' @inheritParams batch_config
#' @inheritParams batch_run
#' @return A character vector of the names of the remaining files.
#' @seealso \code{\link{batch_process}()}
#' @export
batch_remaining_files <- function(path = ".", failed = FALSE) {
  chk_lgl(failed)
  config <- batch_read_config(path)
  files <- list.files(path, pattern = config$regexp, recursive = config$recurse)
  files <- files[file_time(path, files) <= config$time]
  if(!length(files) || is.na(failed)) return(files)
  failed_files <- failed_files(path)
  failed_files <- intersect(failed_files, files)
  if(isTRUE(failed)) return(failed_files)
  files <- setdiff(files, failed_files)
  files
}
