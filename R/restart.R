#' (Re)Start Batch Processing
#' 
#' It can be halted during processing.
#'
#' @inheritParams batch_setup
#' @param parallel A flag specifying whether to process the files in parallel (not yet used).
#'
#' @return An invisible character vector of the files processed by the function call.
#' @export
batch_restart <- function(path, parallel = FALSE) {
  chk_flag(parallel)
  if(!parallel) .NotYetUsed("parallel", error = FALSE) 
    
  files <- batch_files(path, processed = FALSE)
  if(!length(files)) {
    wrn("there are no files remaining to process")
    return(character(0))
  }
  args <- read_batchr_setup.rds(path)
  # need to do magic
}
