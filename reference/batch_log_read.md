# Read Log File

Reads the values in the log file created by
[`batch_run()`](https://poissonconsulting.github.io/batchr/reference/batch_run.md).

## Usage

``` r
batch_log_read(path)
```

## Arguments

- path:

  A string of the path to the directory with the files for processing.

## Value

A tibble with four columns:

- type:

  A character vector indicating SUCCESS or FAILURE

- time:

  A hms vector of the file processing time

- file:

  A character vector of the file name

- error:

  A character vector of the error message (or NA if no error)

## See also

[`batch_process()`](https://poissonconsulting.github.io/batchr/reference/batch_process.md)
and
[`batch_config_read()`](https://poissonconsulting.github.io/batchr/reference/batch_config_read.md)

## Examples

``` r
path <- tempdir()
write.csv(mtcars, file.path(path, "file1.csv"))
batch_config(function(x) TRUE, path, regexp = "[.]csv$")
batch_log_read(path)
#> # A tibble: 0 × 4
#> # ℹ 4 variables: type <chr>, time <time>, file <chr>, message <chr>
batch_run(path, ask = FALSE)
batch_log_read(path)
#> # A tibble: 1 × 4
#>   type    time   file      message
#>   <chr>   <time> <chr>     <chr>  
#> 1 SUCCESS 00'00" file1.csv NA     
batch_cleanup(path)
unlink(file.path(path, "file1.csv"))
```
