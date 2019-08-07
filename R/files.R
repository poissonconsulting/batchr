#' Batch Files
#' 
#' Gets the names of the files that are remaining to be processed by
#' \code{\link{batch_run}()}.
#' 
#' \code{\link{batch_completed}()} can be used to test if there are any
#' files remaining.
#'
#' @inheritParams batch_config
#' @inheritParams batch_run
#' @return A character vector of the names of the remaining files.
#' @seealso \code{\link{batch_process}()} and \code{\link{batch_run}()} 
#' @export
batch_files_remaining <- function(path = ".", failed = FALSE) {
  chk_lgl(failed)
  config <- batch_config_read(path)
  files <- list.files(path, pattern = config$regexp, recursive = config$recurse)
  files <- files[file_time(path, files) <= config$time]
  if(!length(files) || is.na(failed)) return(files)
  failed_files <- failed_files(path)
  failed_files <- intersect(failed_files, files)
  if(isTRUE(failed)) return(failed_files)
  files <- setdiff(files, failed_files)
  files
}
