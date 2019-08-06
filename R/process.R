#' Batch File Processing
#' 
#' Performs batch processing of files in a directory using the 
#' \code{\link{batch_config}()}, \code{\link{batch_run}()}
#' and \code{\link{batch_cleanup}()} functions. 
#' For more control and interactivity the user should call these functions
#' instead of \code{\link{batch_process}()}.
#' 
#' As well as the directory and a regular expression to identify files the
#' user must provide the function that will be used to process each file.
#' Processing is started (and can be manually restarted) 
#' using \code{\link{batch_run}()}
#' which locks the configuration file with \code{\link[filelock]{lock}} 
#' to prevent concurrent calls. 
#' 
#' The files that have not yet been processed can be got 
#' using \code{\link{batch_remaining_files}()} while 
#' \code{\link{batch_completed}()} tests whether there are any remaining files.
#' By default (\code{failed = FALSE}) both functions exclude files that failed
#' to process. To only consider files that failed to process set
#' \code{failed = TRUE}. If \code{failed = NA} then remaining files are those
#' that have not yet been processed as well as those that have failed to process.
#' 
#' The configuration details are saved in the '.batchr.rds file'
#' which can be read using \code{\link{batch_read_config}()}.
#' Configuration is only possible if the directory does not already contain
#' a configuration file. If \code{recursive = TRUE} then the subdirectories
#' must also not contain configuration files.
#' Existing configuration files can be listed 
#' using \code{\link{batch_config_files}()}.
#' 
#' The pattern must match at least one non-hidden file in the directory 
#' (or if \code{recursive = TRUE} in the directory or subdirectories).
#' Hidden files are excluded to prevent accidental modification of system files.
#' 
#' The fun function's first argument should be 
#' a string of the path to a single file.
#' The function should return anything other than a FALSE if 
#' processing was successful.
#' 
#' Processing is only considered to have failed if the fun returns FALSE or
#' throws an error. 
#' Any errors are caught and batch processing resumes at the next file.
#' 
#' All attempts to process a file are logged in the '.batchr.log' file 
#' with the level (INFO (success), WARN (returned FALSE), or ERROR (threw an error)),
#' system time and file name using a \code{\link[log4r]{logger}} object.
#' All failed attempts which result in an error are also logged in the
#' '.batchr_error.log' file with the level (ERROR), system time, file name
#' and error message.
#' This information can be read as a tibble
#' using \code{\link{batch_read_log}()}.
#' 
#' @inheritParams batch_config
#' @inheritParams batch_run
#' @inheritParams batch_cleanup
#'
#' @seealso \code{\link{batch_config}()}, \code{\link{batch_run}()}
#' and \code{\link{batch_cleanup}()}
#' @return An invisible flag indicating whether all the files where 
#' successfully processed.
#' @export
batch_process <- function(fun, path = ".", pattern = ".*", recursive = FALSE, 
                          ..., parallel = FALSE, force = TRUE, 
                          ask = getOption("batchr.ask", TRUE)) {
  batch_config(fun, path = path, pattern = pattern, recursive = recursive, ...)
  success <- batch_run(path = path, parallel = parallel, ask = ask)
  batch_cleanup(path, force = force)
  all(success)
}
