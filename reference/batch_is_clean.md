# Is Clean

Tests whether directory contains configuration file created by
[`batch_config()`](https://poissonconsulting.github.io/batchr/reference/batch_config.md).

## Usage

``` r
batch_is_clean(path, recurse = FALSE)
```

## Arguments

- path:

  A string of the path to the directory with the files for processing.

- recurse:

  A flag specifying whether to recurse into path's subdirectories.

## Value

A flag specifying whether the directory is clean.

## See also

[`batch_cleanup()`](https://poissonconsulting.github.io/batchr/reference/batch_cleanup.md)

## Examples

``` r
path <- tempdir()
batch_is_clean(path)
#> [1] TRUE
write.csv(mtcars, file.path(path, "file1.csv"))
batch_config(function(x) TRUE, path, regexp = "[.]csv$")
batch_is_clean(path)
#> [1] FALSE
batch_cleanup(path, force = TRUE, remaining = TRUE)
batch_is_clean(path)
#> [1] TRUE
unlink(file.path(path, "file1.csv"))
```
