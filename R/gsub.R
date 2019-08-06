#' File Text Replacement
#' 
#' Uses \code{\link[base]{gsub}} to perform text pattern replacement on a file.
#'
#' @param file A string of the name of the file to modify.
#' @param pattern_gsub A string of the regular expression to match.
#' @param replacement_gsub A string of the replacement text.
#'
#' @return TRUE
#' @seealso \code{\link[base]{gsub}}
#' @export
gsub_file <- function(file, pattern_gsub, replacement_gsub) {
  chk_file(file)
  lines <- readLines(file)
  lines <- gsub(pattern = pattern_gsub, replacement = replacement_gsub, lines)
  writeLines(lines, file)
  invisible(TRUE)
}

#' Batch File Text Replacement
#' 
#' Uses \code{\link{batch_process}()} and \code{\link{gsub_file}()} to
#' replace text in by default .R and .r files in the working directory.
#' 
#' @inheritParams batch_config
#' @inheritParams batch_run
#' @inheritParams batch_cleanup
#' @inheritParams gsub_file
#'
#' @seealso \code{\link{batch_process}()}
#' @return An invisible flag indicating whether all the files were 
#' successfully processed.
#' @export
batch_gsub <- function(pattern_gsub, replacement_gsub, 
                       path = ".", pattern = "[.](R|r)$", recursive = FALSE, 
                       parallel = FALSE) {
  batch_process(gsub_file, path = path, pattern = pattern, recursive = recursive,
                parallel = parallel, pattern_gsub = pattern_gsub, 
                replacement_gsub = replacement_gsub)
}
