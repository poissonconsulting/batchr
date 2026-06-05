# Batch File Status

Gets the current status (SUCCESS, FAILURE, REMAING) of each eligible
file in path.

## Usage

``` r
batch_file_status(path)
```

## Arguments

- path:

  A string of the path to the directory with the files for processing.

## Value

A tibble with four columns:

- type:

  A character vector indicating SUCCESS, FAILURE or REMAING

- time:

  A hms vector of the file processing time

- file:

  A character vector of the file name

- error:

  A character vector of the error message (or NA if no error)

## See also

[`batch_log_read()`](https://poissonconsulting.github.io/batchr/reference/batch_log_read.md)

## Examples

``` r
path <- tempdir()
write.csv(mtcars, file.path(path, "file1.csv"))
batch_config(function(x) TRUE, path, regexp = "[.]csv$")
batch_file_status(path)
#> # A tibble: 1 × 4
#>   type    time   file      message
#>   <chr>   <time> <chr>     <chr>  
#> 1 REMAING    NA  file1.csv NA     
batch_run(path, ask = FALSE)
batch_file_status(path)
#> # A tibble: 1 × 4
#>   type    time   file      message
#>   <chr>   <time> <chr>     <chr>  
#> 1 SUCCESS 00'00" file1.csv NA     
batch_cleanup(path)
unlink(file.path(path, "file1.csv"))
```
