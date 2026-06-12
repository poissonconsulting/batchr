# Reconfigures Batch Processing File Set

Updates the regular expression and/or recurse argument that were
provided when a directory was configured (using
[`batch_config()`](https://poissonconsulting.github.io/batchr/reference/batch_config.md)).

## Usage

``` r
batch_reconfig_fileset(path, regexp = NULL, recurse = NULL)
```

## Arguments

- path:

  A string of the path to the directory with the files for processing.

- regexp:

  A string of a regular expression. Only non-hidden file names which
  match the regular expression will be batch processed.

- recurse:

  A flag specifying whether to recurse into path's subdirectories.

## Value

An invisible character vector of the paths to the files remaining to be
processed.

## Details

`batch_reconfig_fileset()` is useful for including or excluding
particular files.

It should be noted that
[`batch_reconfig_fun()`](https://poissonconsulting.github.io/batchr/reference/batch_reconfig_fun.md)
does not alter the configuration time.

In order to process previously failed files
[`batch_run()`](https://poissonconsulting.github.io/batchr/reference/batch_run.md)
should be called with `failed = NA` or `failed = TRUE`.

## See also

[`batch_process()`](https://poissonconsulting.github.io/batchr/reference/batch_process.md)
and
[`batch_config()`](https://poissonconsulting.github.io/batchr/reference/batch_config.md)

## Examples

``` r
path <- tempdir()
write.csv(mtcars, file.path(path, "file1.csv"))
batch_config(function(x) TRUE, path, regexp = "[.]csv$")
batch_config_read(path)
#> $time
#> [1] "2026-06-12 06:30:48 UTC"
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
#> <environment: 0x55f67dfeb970>
#> 
#> $dots
#> list()
#> 
batch_reconfig_fileset(path, regexp = "file\\d+[.]csv$")
batch_config_read(path)
#> $time
#> [1] "2026-06-12 06:30:48 UTC"
#> 
#> $regexp
#> [1] "file\\d+[.]csv$"
#> 
#> $recurse
#> [1] FALSE
#> 
#> $fun
#> function (x) 
#> TRUE
#> <environment: 0x55f67d41da78>
#> 
#> $dots
#> list()
#> 
batch_cleanup(path, force = TRUE, remaining = TRUE)
unlink(file.path(path, "file1.csv"))
```
