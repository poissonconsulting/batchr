sys_time_utc <- function() {
  time <- Sys.time()
  attr(time, "tzone") <- "UTC"
  time
}

is_try_error <- function(x) {
  inherits(x, "try-error")
}

tibble <- function(...) {
  data <- data.frame(..., stringsAsFactors = FALSE)
  class(data) <- c("tbl_df", "tbl", "data.frame")
  data
}

set_names <- function(x, names) {
  names(x) <- names
  x
}
