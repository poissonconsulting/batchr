#' Cleanup Batch Processing
#'
#' @inheritParams batch_config
#' @return An invisible character vector of the files that have been processed.
batch_cleanup <- function(path = ".") {
#  unprocessed <- batch_files(path, processed = FALSE)
#  if(length(unprocessed))
#    err(co_and(unprocessed, "the following %n file%s %r yet to be processed: %c"))
#  file.remove(file.path(path, ".batchr.rds"))
#  invisible(batch_files(path))
}
