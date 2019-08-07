#' Updates Batch Processing Configuration
#' 
#' Updates the function that was provided when a directory was configured 
#' using \code{\link{batch_config}()}.
#' 
#' \code{batch_reconfig_fun()} is useful if a new version of the files is required
#' to successfully process some of the files.
#' 
#' It should be noted that \code{batch_reconfig_fun()} does not alter the 
#' configuration time.
#' 
#' In order to process previously failed files \code{\link{batch_run}()}
#' should be called with \code{failed = NA} or \code{failed = TRUE}.
#' 
#' @inheritParams batch_config
#'
#' @return An invisible character vector of the paths to the files 
#' remaining to be processed.
#' @seealso \code{\link{batch_process}()} and \code{\link{batch_config}()}
#' @export
batch_reconfig_fun <- function(fun, path = ".", ...) {
  chk_function(fun)
  chk_dir(path)

  config <- batch_config_read(path)
  recurse <- config$recurse
  regexp <- config$regexp

  if(recurse && length(config_files(path, recursive = TRUE)) > 1L)
    err("Subdirectories of '", path, "' contain '.batchr.rds' files.")
  
  dots <- list(...)
  save_config(path, regexp, recurse, fun = fun, dots = dots)
  invisible(batch_files_remaining(path, failed = NA))
}
