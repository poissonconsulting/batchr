#' Runs Batch Processing
#'
#' Starts (or restarts if previously stopped) processing the remaining files
#' specified by [batch_config()].
#'
#' `batch_run()` logs all file processing attempts together with the
#' the type (SUCCESS or FAILURE), the
#' system time in UTC, the file name and any error messages.
#' The hidden log file can be read using [batch_log_read()].
#'
#' [batch_files_remaining()] provides a vector of the files that
#' are remaining to be processed.
#'
#' When processing is complete the hidden configuration file
#' and hidden log file can be deleted using [batch_cleanup()].
#'
#' If a remaining file is removed or modified by a separate process,
#' `batch_run()` throws an error.
#'
#' @inheritParams batch_config
#' @param failed A logical scalar specifying how to treat files
#' that previously failed to process. If FALSE (the default) failed files
#' are excluded, if NA they are included and if TRUE they are only included.
#' @param progress A flag specifying whether to print a progress bar.
#' @param files A character vector of the remaining files to process.
#' If `NULL` then `files` is `batch_files_remaining(path, failed)`.
#' @param seeds A named list of the L'Ecuyer-CMRG seed to use for each
#' file. If `NULL` then `seeds` is `batch_seeds(files)`.
#' @param options The future specific options to use with the workers.
#' seed must be `FALSE`.
#' @param ask A flag specifying whether to ask before starting to process the files.
#' @return An invisible named logical vector indicating for each file
#' whether it was successfully processed.
#' @seealso [batch_process()], [batch_config()] and
#' [batch_cleanup()]
#' @export
#' @examples
#' path <- tempdir()
#' write.csv(mtcars, file.path(path, "file1.csv"))
#' batch_config(function(x) TRUE, path, regexp = "[.]csv$")
#' batch_run(path, ask = FALSE)
#' batch_cleanup(path)
#' unlink(file.path(path, "file1.csv"))
batch_run <- function(path,
                      failed = FALSE, progress = FALSE,
                      files = NULL,
                      seeds = NULL,
                      options = furrr::furrr_options(),
                      ask = getOption("batchr.ask", TRUE)) {
  chk_dir(path)
  chk_lgl(failed)
  chk_flag(progress)
  chk_flag(ask)
  if (!is.null(files)) {
    chk_s3_class(files, "character")
    chk_not_any_na(files)
    chk_unique(files)
  }
  if(!is.null(seeds)) {
    chk_list(seeds)
    chk_named(seeds)
    chk_unique(names(seeds))
  }
  chk_s3_class(options, "furrr_options")
  chk_false(options$seed)

  config <- batch_config_read(path)

  recurse <- config$recurse
  fun <- config$fun
  dots <- config$dots

  if (recurse && length(config_files(path = path, recursive = recurse)) > 1) {
    err("Subdirectories of '", path, "' contain '.batchr.rds' files.")
  }

  remaining <- batch_files_remaining(path, failed = failed)

  if (!is.null(files)) {
    if (!length(files)) {
      return(invisible(.named_logical0))
    }
    unknown <- setdiff(files, remaining)
    if (length(unknown)) {
      err(
        "The following files are not remaining: ",
        cc(unknown, " and "), "."
      )
    }
    remaining <- files
  }

  if (!length(remaining)) {
    return(invisible(.named_logical0))
  }

  if(is.null(seeds)) {
    seeds <- batch_seeds(remaining)
  } else {
    chk_superset(names(seeds), remaining)
    seeds <- seeds[remaining]
  }
  options$seed <- unname(seeds)

  question <- p0(
    "Batch process ", length(remaining), " files in '",
    normalizePath(path), "'?"
  )
  if (ask && !yesno(question)) {
    return(invisible(set_names(rep(FALSE, length(remaining)), remaining)))
  }

  success <- process_files(remaining,
    fun = fun, dots = dots,
    path = path, config_time = config$time,
    progress = progress,
    options = options
  )

  invisible(success)
}
