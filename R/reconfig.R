#' Reconfigures Batch Processing Function
#'
#' Updates the function and function arguments that were provided
#' when a directory was configured (using [batch_config()]).
#'
#' `batch_reconfig_fun()` is useful if a new version of the function is required
#' to successfully process some of the files.
#'
#' It should be noted that `batch_reconfig_fun()` does not alter the
#' configuration time.
#'
#' In order to process previously failed files [batch_run()]
#' should be called with `failed = NA` or `failed = TRUE`.
#'
#' @inheritParams batch_config
#'
#' @return An invisible character vector of the paths to the files
#' remaining to be processed.
#' @seealso [batch_process()] and [batch_config()]
#' @export
batch_reconfig_fun <- function(path, fun, ...) {
  chk_function(fun)
  chk_dir(path)

  config <- batch_config_read(path)
  recurse <- config$recurse
  regexp <- config$regexp
  time <- config$time

  if (recurse && length(config_files(path, recursive = TRUE)) > 1L) {
    err("Subdirectories of '", path, "' contain '.batchr.rds' files.")
  }

  dots <- list(...)
  save_config(path, regexp, recurse, fun = fun, dots = dots, time = time)
  invisible(batch_files_remaining(path, failed = NA))
}

#' Reconfigures Batch Processing File Set
#'
#' Updates the regular expression and/or recurse argument  that were provided
#' when a directory was configured (using [batch_config()]).
#'
#' `batch_reconfig_fileset()` is useful for including or excluding particular files.
#'
#' It should be noted that `batch_reconfig_fun()` does not alter the
#' configuration time.
#'
#' In order to process previously failed files [batch_run()]
#' should be called with `failed = NA` or `failed = TRUE`.
#'
#' @inheritParams batch_config
#'
#' @return An invisible character vector of the paths to the files
#' remaining to be processed.
#' @seealso [batch_process()] and [batch_config()]
#' @export
#' @examples 
#' path <- tempdir()
#' write.csv(mtcars, file.path(path, "file1.csv"))
#' batch_config(function(x) TRUE, path, regexp = "[.]csv$")
#' batch_config_read(path)
#' batch_reconfig_fileset(path, regexp = "file\\d+[.]csv$")
#' batch_config_read(path)
#' batch_cleanup(path, force = TRUE, remaining = TRUE)
#' unlink(file.path(path, "file1.csv"))
batch_reconfig_fileset <- function(path, regexp = NULL, recurse = NULL) {
  chk_dir(path)
  if (!is.null(regexp)) chk_string(regexp)
  if (!is.null(recurse)) chk_flag(recurse)

  if (is.null(regexp) && is.null(recurse)) {
    err("`regexp` and `recurse` must not both be NULL.")
  }

  config <- batch_config_read(path)
  if (is.null(regexp)) regexp <- config$regexp
  if (is.null(recurse)) recurse <- config$recurse
  fun <- config$fun
  dots <- config$dots
  time <- config$time

  if (recurse && length(config_files(path, recursive = TRUE)) > 1L) {
    err("Subdirectories of '", path, "' contain '.batchr.rds' files.")
  }

  save_config(path, regexp, recurse, fun = fun, dots = dots, time = time)
  invisible(batch_files_remaining(path, failed = NA))
}
