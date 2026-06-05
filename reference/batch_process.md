# Batch File Processing

Performs batch processing of files in a directory using the
[`batch_config()`](https://poissonconsulting.github.io/batchr/reference/batch_config.md),
[`batch_run()`](https://poissonconsulting.github.io/batchr/reference/batch_run.md)
and
[`batch_cleanup()`](https://poissonconsulting.github.io/batchr/reference/batch_cleanup.md)
functions. For more control the user should call these three functions
instead.

## Usage

``` r
batch_process(
  fun,
  path,
  regexp = ".*",
  recurse = FALSE,
  progress = FALSE,
  force = TRUE,
  report = TRUE,
  seeds = NULL,
  options = furrr::furrr_options(),
  ask = getOption("batchr.ask", TRUE),
  ...
)
```

## Arguments

- fun:

  A function to process each of the files. `fun`'s first argument should
  be a string of the path to a single file. If processing is
  unsuccessful `fun` should return FALSE or throw an error (error
  messages are caught and automatically logged). If fun deletes or
  modifies the file then it is no longer considered for processing.

- path:

  A string of the path to the directory with the files for processing.

- regexp:

  A string of a regular expression. Only non-hidden file names which
  match the regular expression will be batch processed.

- recurse:

  A flag specifying whether to recurse into path's subdirectories.

- progress:

  A flag specifying whether to print a progress bar.

- force:

  A flag specifying whether to delete configuration and log files even
  if there are files remaining to be processed.

- report:

  A flag specifying whether to outputs a report of the status of
  individual files to the console.

- seeds:

  A named list of the L'Ecuyer-CMRG seed to use for each file. If `NULL`
  then `seeds` is `batch_seeds(files)`.

- options:

  The future specific options to use with the workers. seed must be
  `FALSE`.

- ask:

  A flag specifying whether to ask before starting to process the files.

- ...:

  Additional arguments passed to `fun`.

## Value

An invisible flag indicating whether all the files where successfully
processed.

## See also

[`batch_config()`](https://poissonconsulting.github.io/batchr/reference/batch_config.md),
[`batch_run()`](https://poissonconsulting.github.io/batchr/reference/batch_run.md)
and
[`batch_cleanup()`](https://poissonconsulting.github.io/batchr/reference/batch_cleanup.md)

## Examples

``` r
path <- tempdir()
write.csv(mtcars, file.path(path, "file1.csv"))
batch_process(function(x) TRUE, path, regexp = "[.]csv$", ask = FALSE)
#> ✔ file1.csv [00:00:00.000]
#> Success: 1
#> Failure: 0
#> Remaining: 0
#> 
unlink(file.path(path, "file1.csv"))
```
