#' Configuration Files
#'
#' @inheritParams batch_config
#' @return A character vector of the names of the configuration file(s).
#' @seealso \code{\link{batch_config}()}
#' @export
batch_config_files <- function(path = ".", recursive = FALSE) {
  chk_dir(path)
  chk_flag(recursive)

  list.files(path, pattern = "^[.]batchr[.]rds$", recursive = recursive,
             all.files = TRUE)
}

#' Remaining Files
#' 
#' Gets the names of the files that are remaining to be processed.
#' 
#' By default, files that were not successfully processed are excluded.
#'
#' @inheritParams batch_config
#' @inheritParams batch_start
#' @return A character vector of the names of the remaining files.
#' @seealso \code{\link{batch_start}()}
#' @export
batch_remaining_files <- function(path = ".", failed = FALSE) {
  chk_lgl(failed)
  config <- batch_read_config(path)
  files <- list.files(path, pattern = config$pattern, recursive = config$recursive)
  files <- files[file.mtime(file.path(path, files)) < config$time]
  if(!length(files) || is.na(failed)) return(files)
  # need to get failed files...
  files
}


