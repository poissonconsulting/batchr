# Read Configuration File

Reads the values in the configuration file created by
[`batch_config()`](https://poissonconsulting.github.io/batchr/reference/batch_config.md).

## Usage

``` r
batch_config_read(path)
```

## Arguments

- path:

  A string of the path to the directory with the files for processing.

## Value

A named list of the configuration values.

## See also

[`batch_process()`](https://poissonconsulting.github.io/batchr/reference/batch_process.md)
and
[`batch_log_read()`](https://poissonconsulting.github.io/batchr/reference/batch_log_read.md)

## Examples

``` r
path <- tempdir()
write.csv(mtcars, file.path(path, "file1.csv"))
batch_config(function(x) TRUE, path, regexp = "[.]csv$")
batch_config_read(path)
#> $time
#> [1] "2026-06-24 08:21:19 UTC"
#> 
#> $regexp
#> [1] "[.]csv$"
#> 
#> $recurse
#> [1] FALSE
#> 
#> $fun
#> function (x) 
#> TRUE
#> <environment: 0x55724162bf20>
#> 
#> $dots
#> list()
#> 
batch_cleanup(path, force = TRUE, remaining = TRUE)
unlink(file.path(path, "file1.csv"))
```
