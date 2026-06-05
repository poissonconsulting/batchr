# Runs Batch Processing

Starts (or restarts if previously stopped) processing the remaining
files specified by
[`batch_config()`](https://poissonconsulting.github.io/batchr/reference/batch_config.md).

## Usage

``` r
batch_run(
  path,
  failed = FALSE,
  progress = FALSE,
  files = NULL,
  seeds = NULL,
  options = furrr::furrr_options(),
  ask = getOption("batchr.ask", TRUE)
)
```

## Arguments

- path:

  A string of the path to the directory with the files for processing.

- failed:

  A logical scalar specifying how to treat files that previously failed
  to process. If FALSE (the default) failed files are excluded, if NA
  they are included and if TRUE they are only included.

- progress:

  A flag specifying whether to print a progress bar.

- files:

  A character vector of the remaining files to process. If `NULL` then
  `files` is `batch_files_remaining(path, failed)`.

- seeds:

  A named list of the L'Ecuyer-CMRG seed to use for each file. If `NULL`
  then `seeds` is `batch_seeds(files)`.

- options:

  The future specific options to use with the workers. seed must be
  `FALSE`.

- ask:

  A flag specifying whether to ask before starting to process the files.

## Value

An invisible named logical vector indicating for each file whether it
was successfully processed.

## Details

`batch_run()` logs all file processing attempts together with the the
type (SUCCESS or FAILURE), the system time in UTC, the file name and any
error messages. The hidden log file can be read using
[`batch_log_read()`](https://poissonconsulting.github.io/batchr/reference/batch_log_read.md).

[`batch_files_remaining()`](https://poissonconsulting.github.io/batchr/reference/batch_files_remaining.md)
provides a vector of the files that are remaining to be processed.

When processing is complete the hidden configuration file and hidden log
file can be deleted using
[`batch_cleanup()`](https://poissonconsulting.github.io/batchr/reference/batch_cleanup.md).

If a remaining file is removed or modified by a separate process,
`batch_run()` throws an error.

## See also

[`batch_process()`](https://poissonconsulting.github.io/batchr/reference/batch_process.md),
[`batch_config()`](https://poissonconsulting.github.io/batchr/reference/batch_config.md)
and
[`batch_cleanup()`](https://poissonconsulting.github.io/batchr/reference/batch_cleanup.md)

## Examples

``` r
path <- tempdir()
write.csv(mtcars, file.path(path, "file1.csv"))
batch_config(function(x) TRUE, path, regexp = "[.]csv$")
batch_run(path, ask = FALSE)
batch_cleanup(path)
unlink(file.path(path, "file1.csv"))
```
