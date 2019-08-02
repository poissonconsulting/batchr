
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
```

### Configuration

The first task is for the user to configure a directory by specify the
files to process and the function to process them with.

``` r
library(batchr)

fun <- function(file) {
  data <- read.csv(file)
  data$y <- data$x * 2
  write.csv(data, file)
}

batch_config(fun, path = path, pattern = "^file\\d[.]csv$")
```

Configuring a directory creates a hidden file (`.batchr.rds`) which can
be read using `batch_read_config()`

``` r
batch_config_files(path) # hidden file
#> [1] ".batchr.rds"
batch_read_config(path)
#> $time
#> [1] "2019-08-02 22:38:58 UTC"
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
configured. A file is only treated as unprocessed if the time it was
last modified is *before* the time of configuration If a file is
successfully processed its modification time is set to the current time;
otherwise the name of the file is logged in the `.batchr.log` file.

### Start

The next task is to start (or restart) processing the files using
`batch_start()`.

``` r
batch_start(path)
#> Warning in file(file, "rt"): cannot open file 'file1.csv': No such file or
#> directory
#> Warning in file(file, "rt"): cannot open file 'file2.csv': No such file or
#> directory
#> character(0)
```

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/batchr/issues).

[Pull requests](https://github.com/poissonconsulting/batchr/pulls) are
always welcome.

Please note that the ‘batchr’ project is released with a [Contributor
Code of
Conduct](https://poissonconsulting.github.io/batchr/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
