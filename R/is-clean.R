#' Is Clean
#'
#' Tests whether directory contains configuration file created by [batch_config()].
#'
#' @inheritParams batch_config
#' @inheritParams batch_run
#' @seealso [batch_cleanup()]
#' @export
batch_is_clean <- function(path, recurse = FALSE) {
  chk_dir(path)
  chk_flag(recurse)
  
  !length(config_files(path, recursive = recurse))
}
