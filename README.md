
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

`batchr` is an R package to batch process files using a user-supplied R
function.

## Installation

You can install the latest development version of batchr from
[GitHub](https://github.com/poissonconsulting/batchr) with:

``` r
# install.packages("remotes")
remotes::install_github("poissonconsulting/batchr")
```

## Demonstration

For the purposes of this demonstration let’s create a temporary
directory with two csv files.

``` r
path <- file.path(tempdir(), "demo")
dir.create(path)

write.csv(data.frame(x = 1), file.path(path, "file1.csv"))
write.csv(data.frame(x = 3), file.path(path, "file2.csv"))

list.files(path)
#> [1] "file1.csv" "file2.csv"
```

The first task is for the user to setup a project by specify the files
to process and the function to process them with.

``` r
library(batchr)

fun <- function(file) {
  data <- read.csv(file)
  data$y <- data$x * 2
  write.csv(data, file)
}

batch_setup(fun, path = path, pattern = "^file\\d[.]csv$")
```

Setting up a project creates a hidden file (`.argsbatchr.rds`) which can
be read using `batch_read_args()`

``` r
batch_read_args(path)
#> $time
#> [1] "2019-08-01 21:25:33 UTC"
#> 
#> $pattern
#> [1] "^file\\d[.]csv$"
#> 
#> $recursive
#> [1] FALSE
#> 
#> $FUN
#> function(file) {
#>   data <- read.csv(file)
#>   data$y <- data$x * 2
#>   write.csv(data, file)
#> }
#> 
#> $dots
#> list()
```

The `time` value specifies the system time (in UTC) that the project was
set up. A file is only treated as unprocessed if the time it was last
modified is *before* this `time` value. Whenever an attempt is made to
process a file its modification time is set to the current time (marking
it as processed) and it’s file name is logged in a hidden text file
(`.batchr_log.txt`) together with a flag (0 or 1) indicating whether
processing occurred without an error. This approach prevents newly
created files from being accidentally processed.

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/batchr/issues).

[Pull requests](https://github.com/poissonconsulting/batchr/pulls) are
always welcome.

Please note that the ‘batchr’ project is released with a [Contributor
Code of
Conduct](https://poissonconsulting.github.io/batchr/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
