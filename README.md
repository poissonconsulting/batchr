
<!-- README.md is generated from README.Rmd. Please edit that file -->

# batchr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/poissonconsulting/batchr/workflows/R-CMD-check/badge.svg)](https://github.com/poissonconsulting/batchr/actions)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/batchr/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/batchr?branch=master)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![CRAN
status](https://www.r-pkg.org/badges/version/batchr)](https://CRAN.R-project.org/package=batchr)
<!-- badges: end -->

`batchr` is an R package to batch process files using an R function.

The key design principle is that only files which were last modified
*before* the directory was configured are processed. A hidden file
stores the configuration time and function etc while successfully
processed files are automatically touched to update their modification
date.

As a result batch processing can be stopped and restarted and any files
created (or modified or deleted) during processing are ignored.

To allow the user control over the reprocessing of problematic files,
all processing attempts (SUCCESS or FAILURE) are recorded in a hidden
log file.

## Installation

You can install the released version of batchr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("batchr")
```

And the development version from
[GitHub](https://github.com/poissonconsulting/batchr) with:

``` r
# install.packages("remotes")
remotes::install_github("poissonconsulting/batchr")
```

## Demonstration

Consider a directory with two .csv files

``` r
path <- file.path(tempdir(), "example")
unlink(path, force = TRUE)
dir.create(path)

write.csv(data.frame(x = 1), file.path(path, "file1.csv"), row.names = FALSE)
write.csv(data.frame(x = 3), file.path(path, "file2.csv"), row.names = FALSE)
```

First define the function to process them.

``` r
fun <- function(file) {
  data <- read.csv(file)
  data$x <- data$x * 2
  write.csv(data, file, row.names = FALSE)
  TRUE
}
```

Then simply call `batch_process()` to apply the function to all the
files.

``` r
library(batchr)
batch_process(fun, path, ask = FALSE)
#> ✓ file1.csv [00:00:00.004]
#> ✓ file2.csv [00:00:00.009]
#> Success: 2
#> Failure: 0
#> Remaining: 0
#> 
```

The files have been updated as follows.

``` r
read.csv(file.path(path, "file1.csv"))
#>   x
#> 1 2
read.csv(file.path(path, "file2.csv"))
#>   x
#> 1 6
```

For a more realistic demonstration with finer control over the batch
processing see the [Batchr
Demonstration](https://poissonconsulting.github.io/batchr/articles/batchr.html)
vignette.

### Parallel Chains

To process the files in parallel simply set

``` r
library(future)
plan(multisession)
```

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/batchr/issues).

[Pull requests](https://github.com/poissonconsulting/batchr/pulls) are
always welcome.

## Code of Conduct

Please note that the batchr project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
