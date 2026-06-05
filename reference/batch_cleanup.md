# Cleanup Batch Processing

Deletes configuration file created by
[`batch_config()`](https://poissonconsulting.github.io/batchr/reference/batch_config.md)
and log file created by
[`batch_run()`](https://poissonconsulting.github.io/batchr/reference/batch_run.md).

## Usage

``` r
batch_cleanup(
  path,
  force = FALSE,
  remaining = FALSE,
  failed = NA,
  recursive = FALSE,
  silent = FALSE
)
```

## Arguments

- path:

  A string of the path to the directory with the files for processing.

- force:

  A flag specifying whether to delete configuration and log files even
  if there are files remaining to be processed.

- remaining:

  A flag specifying whether to delete any files that are remaining to be
  processed (only applied when `force = TRUE`). Files that have been
  processed are never deleted.

- failed:

  A logical scalar specifying how to treat files that previously failed
  to process. If FALSE (the default) failed files are excluded, if NA
  they are included and if TRUE they are only included.

- recursive:

  A flag specifying whether to recurse into subdirectories when cleaning
  up. This is unrelated to the `recurse` option of
  [`batch_config()`](https://poissonconsulting.github.io/batchr/reference/batch_config.md)
  and is only expected to be used if the user has neglected to clean up
  multiple nested directories.

- silent:

  A flag specifying whether to suppress warnings (and messages).

## Value

A named logical vector indicating which directories were successfully
cleaned up.

## Details

The
[`batch_completed()`](https://poissonconsulting.github.io/batchr/reference/batch_completed.md)
function can be used to test if batch processing is complete.

## See also

[`batch_process()`](https://poissonconsulting.github.io/batchr/reference/batch_process.md)

## Examples

``` r
path <- tempdir()
write.csv(mtcars, file.path(path, "file1.csv"))
batch_config(function(x) TRUE, path, regexp = "[.]csv$")
batch_run(path, ask = FALSE)
batch_cleanup(path)
unlink(file.path(path, "file1.csv"))
```
