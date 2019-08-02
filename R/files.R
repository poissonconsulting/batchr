#' List Set Up File Names
#'
#' @inheritParams batch_setup
#'
#' @return A character vector of the file names.
#' @export
batch_setup_files <- function(path = ".", recursive = FALSE) {
  chk_dir(path)
  chk_flag(recursive)

  list.files(path, pattern = "^[.]batchr[.]rds$", recursive = recursive,
             all.files = TRUE)
}
