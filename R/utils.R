sys_time_utc <- function() {
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

yesno <- function (...) {
    yeses <- c("Yes", "Definitely", "For sure", "Yup", "Yeah", 
        "I agree", "Absolutely")
    nos <- c("No way", "Not yet", "I forget", "No", "Nope", "Uhhhh... Maybe?")
    qs <- c(sample(yeses, 1), sample(nos, 2))
    rand <- sample(length(qs))
    cat(paste0(..., collapse = ""))
    utils::menu(qs[rand]) == which(rand == 1)
}

