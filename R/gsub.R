#' File Text Replacement
#'
#' Uses [base::gsub()] to perform text replacement on a file.
#'
#' @param file A string of the name of the file to modify.
#' @param pattern A string of the regular expression to match.
#' @param replacement A string of the replacement text.
#'
#' @return TRUE
#' @seealso [base::gsub()]
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
#' Uses [batch_process()] and [gsub_file()] to
#' perform batch text file replacement.
#' By default it replaces text in all .R and .r files in the working directory.
#'
#' @inheritParams batch_config
#' @inheritParams batch_run
#' @inheritParams batch_cleanup
#' @inheritParams batch_process
#' @inheritParams gsub_file
#'
#' @seealso [batch_process()] and [gsub_file()]
#' @return An invisible flag indicating whether all the files were
#' successfully processed.
#' @export
batch_gsub <- function(pattern, replacement,
                       path, regexp = "[.](R|r)$", recurse = FALSE,
                       progress = FALSE, report = TRUE,
                       options = furrr::future_options(),
                       ask = getOption("batchr.ask", TRUE)) {
  batch_process(gsub_file,
    pattern = pattern, replacement = replacement, report = report,
    path = path, regexp = regexp, recurse = recurse,
    progress = progress, options = options, ask = ask
  )
}
