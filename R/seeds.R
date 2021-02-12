rinteger <- function(n = 1L) {
  chk_whole_number(n)
  chk_gte(n, 0L)
  if(n == 0) integer(0)
  mx <- 2147483647L
  as.integer(runif(n, -mx, mx))
}

get_random_seed <- function() {
  globalenv()$.Random.seed
}

set_random_seed <- function(seed, advance = FALSE) {
  env <- globalenv()
  env$.Random.seed <- seed
  if(advance) {
    fun <- if(is.null(seed)) suppressWarnings else identity
    fun(runif(1))
  }
  invisible(env$.Random.seed)
}

get_lecyer_cmrg_seed <- function() {
  seed <- get_random_seed()
  on.exit(set_random_seed(seed))
  RNGkind("L'Ecuyer-CMRG")
  set.seed(rinteger(1))
  get_random_seed()
}

#' L'Ecuyer-CMRG Seeds
#'
#' Generates a named list of L'Ecuyer-CMRG seeds.
#'
#' @param files A character vector of the names of the files.
#'
#' @return A named list of the L'Ecuyer-CMRG seed for each file name.
#' @export
#'
#' @examples
#' batch_seeds(c("a", "b"))
# inspired by furrr:::generate_seed_streams
batch_seeds <- function(files = batch_files_remaining()) {
  chk_s3_class(files, "character")
  
  oseed <- get_random_seed()
  on.exit(set_random_seed(oseed, advance = TRUE))
  
  seed <- get_lecyer_cmrg_seed()
  seeds <- vector("list", length = length(files))
  for (i in seq_len(length(files))) {
    seeds[[i]] <- nextRNGSubStream(seed)
    seed <- nextRNGStream(seed)
  }
  names(seeds) <- files
  seeds
}
