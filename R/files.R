#' Batch Files
#'
#' Gets the names of the files that are remaining to be processed by
#' [batch_run()].
#'
#' [batch_completed()] can be used to test if there are any
#' files remaining.
#'
#' @inheritParams batch_config
#' @inheritParams batch_run
#' @return A character vector of the names of the remaining files.
#' @seealso [batch_process()] and [batch_run()]
#' @export
#' @examples 
#' path <- tempdir()
#' write.csv(mtcars, file.path(path, "file1.csv"))
#' batch_config(function(x) TRUE, path, regexp = "[.]csv$")
#' batch_files_remaining(path)
#' batch_run(path, ask = FALSE)
#' batch_files_remaining(path)
#' batch_cleanup(path)
#' unlink(file.path(path, "file1.csv"))
batch_files_remaining <- function(path, failed = FALSE) {
  chk_lgl(failed)
  config <- batch_config_read(path)
  files <- list.files(path, pattern = config$regexp, recursive = config$recurse)
  files <- files[file_time(path, files) <= config$time]
  if (!length(files) || is.na(failed)) {
    return(files)
  }
  failed_files <- failed_files(path)
  failed_files <- intersect(failed_files, files)
  if (isTRUE(failed)) {
    return(failed_files)
  }
  files <- setdiff(files, failed_files)
  files
}
