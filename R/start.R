#' Start Batch Processing
#' 
#' Starts (or restarts if stopped during processing) batch processing
#' files identified during batch set up.
#'
#' @inheritParams batch_setup
#' @param parallel A flag specifying whether to process the files in 
#' parallel (not yet used).
#'
#' @return An invisible character vector of the 
#' files processed by the current function call.
#' @export
batch_start <- function(path, parallel = FALSE) {
  check_directory(path)
  chk_flag(parallel)
  
  if(parallel) .NotYetUsed("parallel", error = FALSE) 
  
  setup <- batch_read_setup(path)
  NULL
  # need to lock file (if not already locked)
    
#  files <- batch_files(path, processed = FALSE)
#  if(!length(files)) {
#    wrn("there are no files remaining to process")
#    return(character(0))
#  }
  # need to do magic
}
