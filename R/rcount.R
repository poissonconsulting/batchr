#' Random Count
#'
#' Generates a platform independent random count between 0 and 
#' 2,147,483,647.
#' It is useful for specifying values for \code{\link{set.seed}()}.
#' @param n A count of the number of random counts to generate.
#' @return An integer vector of random counts.
#' @export
#'
#' @examples
#' set.seed(45)
#' rcount()
#' set.seed(45)
#' rcount(n = 2L)
rcount <- function(n = 1L) as.integer(runif(n, 0, .max_integer))
