# Batch Completed?

Tests if there are any remaining files to process as listed by
[`batch_files_remaining()`](https://poissonconsulting.github.io/batchr/reference/batch_files_remaining.md).

## Usage

``` r
batch_completed(path, failed = FALSE)
```

## Arguments

- path:

  A string of the path to the directory with the files for processing.

- failed:

  A logical scalar specifying how to treat files that previously failed
  to process. If FALSE (the default) failed files are excluded, if NA
  they are included and if TRUE they are only included.

## Value

A flag specifying whether batch processing is complete.

## Details

By default, files that previously failed to process are excluded.

## See also

[`batch_process()`](https://poissonconsulting.github.io/batchr/reference/batch_process.md)

## Examples

``` r
path <- tempdir()
write.csv(mtcars, file.path(path, "file1.csv"))
batch_config(function(x) TRUE, path, regexp = "[.]csv$")
batch_completed(path)
#> [1] FALSE
batch_run(path, ask = FALSE)
batch_completed(path)
#> [1] TRUE
batch_cleanup(path)
unlink(file.path(path, "file1.csv"))
```
