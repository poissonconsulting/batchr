# Batch Report

Outputs a report of the status of individual files to the console.

## Usage

``` r
batch_report(path)
```

## Arguments

- path:

  A string of the path to the directory with the files for processing.

## Value

An invisible NULL. The function is called for its side-effect of
outputting a report of the status of individual files to the console.

## See also

[`batch_file_status()`](https://poissonconsulting.github.io/batchr/reference/batch_file_status.md)

## Examples

``` r
path <- tempdir()
write.csv(mtcars, file.path(path, "file1.csv"))
batch_config(function(x) TRUE, path, regexp = "[.]csv$", )
batch_report(path)
#> ! file1.csv
#> Success: 0
#> Failure: 0
#> Remaining: 1
#> 
batch_run(path, ask = FALSE)
batch_report(path)
#> ✔ file1.csv [00:00:00.000]
#> Success: 1
#> Failure: 0
#> Remaining: 0
#> 
batch_cleanup(path)
unlink(file.path(path, "file1.csv"))
```
