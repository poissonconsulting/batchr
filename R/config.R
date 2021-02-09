#' Configure Batch Processing
#'
#' Configures a directory for batch file processing by [batch_run()].
#'
#' `batch_config()` creates a hidden configuration file in path
#' named '.batchr.rds'.
#'
#' The contents of the file can be read using
#' [batch_config_read()]
#' or updated using [batch_reconfig_fun()].
#'
#' Configuration is only possible if the directory does not already contain
#' a configuration file.
#' If `recurse = TRUE` then the subdirectories
#' must also not contain configuration files.
#'
#' The regexp must match at least one non-hidden file in the directory
#' or if `recurse = TRUE` in the directory or subdirectories.
#' Hidden files are excluded to prevent accidental modification of system files.
#'
#' @param fun A function to process each of the files.
#' `fun`'s first argument should be a string of the path to a single file.
#' If processing is unsuccessful `fun` should return FALSE
#' or throw an error (error messages are caught and automatically logged).
#' If fun deletes or modifies the file then it is no longer considered
#' for processing.
#' @param path A string of the path to the directory with the files for processing.
#' @param regexp A string of a regular expression. Only non-hidden file names
#' which match the regular expression will be batch processed.
#' @param recurse A flag specifying whether to recurse into path's subdirectories.
#' @param ... Additional arguments passed to `fun`.
#'
#' @return An invisible character vector of the paths to the files to
#' that will be processed when [batch_run()] is called.
#' @seealso [batch_process()] and [batch_run()]
#' @export
#' @examples 
#' path <- tempdir()
#' write.csv(mtcars, file.path(path, "file1.csv"))
#' batch_config(function(x) TRUE, path, regexp = "[.]csv$")
#' batch_run(path, ask = FALSE)
#' batch_cleanup(path)
#' unlink(file.path(path, "file1.csv"))
batch_config <- function(fun, path, regexp = ".*", recurse = FALSE, ...) {
  chk_function(fun)
  chk_dir(path)
  chk_string(regexp)
  chk_flag(recurse)

  if (length(config_files(path, recursive = FALSE))) {
    err("Directory '", path, "' already contains a '.batchr.rds' file.")
  }

  if (recurse && length(config_files(path, recursive = TRUE))) {
    err("Subdirectories of '", path, "' contain '.batchr.rds' files.")
  }

  files <- list.files(path = path, pattern = regexp, recursive = recurse)

  if (!length(files)) {
    err(
      "Directory '", path, "' does not contain any files matching '",
      regexp, "'."
    )
  }
  dots <- list(...)
  cleanup_log_file(path)
  save_config(path, regexp, recurse, fun = fun, dots = dots, time = 
                sys_time_utc() + 1e-05) # 1e-05 required to ensure time check
  invisible(files)
}
