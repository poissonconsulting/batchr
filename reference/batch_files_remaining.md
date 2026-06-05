# Batch Files

Gets the names of the files that are remaining to be processed by
[`batch_run()`](https://poissonconsulting.github.io/batchr/reference/batch_run.md).

## Usage

``` r
batch_files_remaining(path, failed = FALSE)
```

## Arguments

- path:

  A string of the path to the directory with the files for processing.

- failed:

  A logical scalar specifying how to treat files that previously failed
  to process. If FALSE (the default) failed files are excluded, if NA
  they are included and if TRUE they are only included.

## Value

A character vector of the names of the remaining files.

## Details

[`batch_completed()`](https://poissonconsulting.github.io/batchr/reference/batch_completed.md)
can be used to test if there are any files remaining.

## See also

[`batch_process()`](https://poissonconsulting.github.io/batchr/reference/batch_process.md)
and
[`batch_run()`](https://poissonconsulting.github.io/batchr/reference/batch_run.md)

## Examples

``` r
path <- tempdir()
write.csv(mtcars, file.path(path, "file1.csv"))
batch_config(function(x) TRUE, path, regexp = "[.]csv$")
batch_files_remaining(path)
#> [1] "file1.csv"
batch_run(path, ask = FALSE)
batch_files_remaining(path)
#> character(0)
batch_cleanup(path)
unlink(file.path(path, "file1.csv"))
```
