# Configure Batch Processing

Configures a directory for batch file processing by
[`batch_run()`](https://poissonconsulting.github.io/batchr/reference/batch_run.md).

## Usage

``` r
batch_config(fun, path, regexp = ".*", recurse = FALSE, ...)
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

- ...:

  Additional arguments passed to `fun`.

## Value

An invisible character vector of the paths to the files to that will be
processed when
[`batch_run()`](https://poissonconsulting.github.io/batchr/reference/batch_run.md)
is called.

## Details

`batch_config()` creates a hidden configuration file in path named
'.batchr.rds'.

The contents of the file can be read using
[`batch_config_read()`](https://poissonconsulting.github.io/batchr/reference/batch_config_read.md)
or updated using
[`batch_reconfig_fun()`](https://poissonconsulting.github.io/batchr/reference/batch_reconfig_fun.md).

Configuration is only possible if the directory does not already contain
a configuration file. If `recurse = TRUE` then the subdirectories must
also not contain configuration files.

The regexp must match at least one non-hidden file in the directory or
if `recurse = TRUE` in the directory or subdirectories. Hidden files are
excluded to prevent accidental modification of system files.

## See also

[`batch_process()`](https://poissonconsulting.github.io/batchr/reference/batch_process.md)
and
[`batch_run()`](https://poissonconsulting.github.io/batchr/reference/batch_run.md)

## Examples

``` r
path <- tempdir()
write.csv(mtcars, file.path(path, "file1.csv"))
batch_config(function(x) TRUE, path, regexp = "[.]csv$")
batch_run(path, ask = FALSE)
batch_cleanup(path)
unlink(file.path(path, "file1.csv"))
```
