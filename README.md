
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

batchr provides a template for a new R package.

## Instructions

In order to create a new package the user should

1)  Go to the batchr [GitHub
    repository](https://github.com/poissonconsulting/batchr) and choose
    ‘Use this template’.
2)  Clone the new repository and replace ‘batchr’ whereever it occurs
    with the new name of the package (including in this README.Rmd
    file).
3)  `devtools::check()` the package and fix any Errors, Warnings or
    Notes.
4)  Knit the README.Rmd file and `pkgdown::build_site()`.
5)  Add the project to
    [Travis](https://www.travis-ci.com/poissonconsulting/batchr) and in
    the
    [Settings](https://www.travis-ci.com/poissonconsulting/batchr/settings)
    add a Cron Job to run the master branch daily if there hasn’t been a
    build in the last 24h.
6)  Add the project to
    [Appveyor](https://ci.appveyor.com/project/poissonconsulting/batchr).
7)  Push the changes to the new repository.

## Installation

You can install the latest development version of batchr from
[GitHub](https://github.com/poissonconsulting/batchr) with:

``` r
# install.packages("remotes")
remotes::install_github("poissonconsulting/batchr")
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
