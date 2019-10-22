#' File Text Replacement
#'
#' Uses \code{\link[base]{gsub}} to perform text replacement on a file.
#'
#' @param file A string of the name of the file to modify.
#' @param pattern A string of the regular expression to match.
#' @param replacement A string of the replacement text.
#'
#' @return TRUE
#' @seealso \code{\link[base]{gsub}}
#' @export
gsub_file <- function(file, pattern, replacement) {
  chk_file(file)
  lines <- readLines(file)
  lines <- gsub(pattern = pattern, replacement = replacement, lines)
  writeLines(lines, file)
  invisible(TRUE)
}

#' Batch File Text Replacement
#'
#' Uses \code{\link{batch_process}()} and \code{\link{gsub_file}()} to
#' perform batch text file replacement.
#' By default it replaces text in all .R and .r files in the working directory.
#'
#' @inheritParams batch_config
#' @inheritParams batch_run
#' @inheritParams batch_cleanup
#' @inheritParams gsub_file
#'
#' @seealso \code{\link{batch_process}()} and \code{\link{gsub_file}()}
#' @return An invisible flag indicating whether all the files were
#' successfully processed.
#' @export
batch_gsub <- function(pattern, replacement,
                       path = ".", regexp = "[.](R|r)$", recurse = FALSE,
                       progress = FALSE,
                       options = furrr::future_options(),
                       ask = getOption("batchr.ask", TRUE)) {
  batch_process(gsub_file,
    pattern = pattern, replacement = replacement,
    path = path, regexp = regexp, recurse = recurse,
    progress = progress, options = options, ask = ask
  )
}
