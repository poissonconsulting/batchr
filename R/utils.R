sys_time <- function() {
  time <- Sys.time()
  attr(time, "tzone") <- "UTC" 
  time
}

is_try_error <- function(x) {
  inherits(x, "try-error")
}

str_extract <- function(x, y) regmatches(x, regexpr(y, x))

tibble <- function(...) {
  data <- data.frame(..., stringsAsFactors = FALSE)
  class(data) <- c("tbl_df", "tbl", "data.frame")
  data
}

set_names <- function(x, names) {
  names(x) <- names
  x
}
