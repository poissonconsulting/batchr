
<!-- README.md is generated from README.Rmd. Please edit that file -->

# batchr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.com/poissonconsulting/batchr.svg?branch=master)](https://travis-ci.com/poissonconsulting/batchr)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/batchr?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/batchr)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/batchr/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/batchr?branch=master)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

`batchr` is an R package to batch process files using an R function.

## Installation

You can install the latest development version of batchr from
[GitHub](https://github.com/poissonconsulting/batchr) with:

``` r
# install.packages("remotes")
remotes::install_github("poissonconsulting/batchr")
```

## Demonstration

``` r
path <- file.path(tempdir(), "example")
unlink(path, force = TRUE)
dir.create(path)

write.csv(data.frame(x = 1), file.path(path, "file1.csv"), row.names = FALSE)
write.csv(data.frame(x = 3), file.path(path, "file2.csv"), row.names = FALSE)
```

``` r
library(batchr)

FUN <- function(file) {
  data <- read.csv(file)
  data$x <- data$x * 2
  write.csv(data, file, row.names = FALSE)
  TRUE
}

batch_process(FUN, path)
#> [1] TRUE
```

``` r
read.csv(file.path(path, "file1.csv"))
#>   x
#> 1 2
read.csv(file.path(path, "file2.csv"))
#>   x
#> 1 6
```

For a more realistic demonstration see

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/batchr/issues).

[Pull requests](https://github.com/poissonconsulting/batchr/pulls) are
always welcome.

Please note that the ‘batchr’ project is released with a [Contributor
Code of
Conduct](https://poissonconsulting.github.io/batchr/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
