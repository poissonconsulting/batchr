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

#' Random Integer
#' 
#' Generates random integers between -2147483647L and 2147483647L.
#'
#' @param n A non-negative whole number of the number of random integers to generate.
#'
#' @return A integer vector.
#' @export
#'
#' @examples
#' rinteger()
rinteger <- function(n = 1) {
  chk_whole_number(n)
  chk_gte(n, 0L)
  if(n == 0) integer(0)
  mx <- 2147483647L
  as.integer(runif(n, -mx, mx))
}
