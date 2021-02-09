#' Is Clean
#'
#' Tests whether directory contains configuration file created by [batch_config()].
#'
#' @inheritParams batch_config
#' @inheritParams batch_run
#' @return A flag specifying whether the directory is clean.
#' @seealso [batch_cleanup()]
#' @export
#' @examples 
#' path <- tempdir()
#' batch_is_clean(path)
#' write.csv(mtcars, file.path(path, "file1.csv"))
#' batch_config(function(x) TRUE, path, regexp = "[.]csv$")
#' batch_is_clean(path)
#' batch_cleanup(path, force = TRUE, remaining = TRUE)
#' batch_is_clean(path)
#' unlink(file.path(path, "file1.csv"))
batch_is_clean <- function(path, recurse = FALSE) {
  chk_dir(path)
  chk_flag(recurse)

  !length(config_files(path, recursive = recurse))
}
