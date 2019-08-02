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
