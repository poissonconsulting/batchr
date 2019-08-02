#' Batch File Processing
#' 
#' Performs batch processing of files in a directory using the 
#' \code{\link{batch_config}()}, \code{\link{batch_start}()}
#' and \code{\link{batch_cleanup}()} functions. 
#' 
#' As well as the directory and a regular expression to identify files the
#' user must provide the function that will be used to process each file.
#' Processing is started (and can be manually restarted) using \code{\link{batch_start}()}
#' which locks the configuration file with \code{\link[filelock]{lock}} 
#' to prevent concurrent calls. 
#' The files that have not yet been processed can be got 
#' using \code{\link{batch_remaining_files}()}.
#' 
#' The configuration details are saved in the '.batchr.rds file'
#' which can be read using \code{\link{batch_read_config}()}.
#' Configuration is only possible if the directory does not already contain
#' a configuration file. If \code{recursive = TRUE} then the subdirectories
#' must also not contain configuration files.
#' Existing configuration files can be listed using \code{\link{batch_config_files}()}.
#' 
#' The pattern must match at least one non-hidden file in the directory 
#' (or if \code{recursive = TRUE} in the directory or subdirectories).
#' Hidden files are excluded to prevent accidental modification of system files.
#' 
#' The FUN function's first argument should be a string of the path to a single file.
#' The function should return TRUE if processing was successful or FALSE
#' if processing was unsuccessful. 
#' If the function returns FALSE the failure is automatically logged as an INFO 
#' message in the '.batchr.log' file.
#' 
#' Processing is also considered to have failed if 
#' the function throws an error. 
#' In this case the error is caught, the failure is logged as an ERROR message 
#' and batch processing resumes at the next file.
#' If, however, the function returns an object other than 
#' a flag then a FATAL message is logged and processing 
#' ceases immediately with batchr throwing an error.
#' 
#' All failures are logged in the '.batchr.log' file with the level (WARN, ERROR, FATAL),
#' system time in UTC and file name using a \code{\link[log4r]{logger}} object.
#' This information parsed as a tibble 
#' with level, time and file columns using \code{\link{batch_read_log}()}.
#' 
#' The user can log custom messages by providing a \code{\link[log4r]{logger}} 
#' object which is passed to the FUN function.
#' In this case the FUN function must accept an argument called logger.
#'
#' @inheritParams batch_config
#' @inheritParams batch_start
#'
#' @seealso \code{\link{batch_config}()}, \code{\link{batch_start}()}
#' and \code{\link{batch_cleanup}()}
#' @return A flag specifying whether all files were processed successfully.
#' @export
batch_process <- function(FUN, path = ".", pattern = ".*", recursive = FALSE, 
                          ..., parallel = FALSE, logger = NULL) {
  batch_config(FUN, path = path, pattern = pattern, recursive = recursive, ...)
  files <- batch_start(path, parallel = parallel, logger = logger)
  if(length(batch_remaining_files(failed = NA))) return(FALSE)
#    batch_cleanup(path)
  TRUE
}
