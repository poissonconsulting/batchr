# Reconfigures Batch Processing Function

Updates the function and function arguments that were provided when a
directory was configured (using
[`batch_config()`](https://poissonconsulting.github.io/batchr/reference/batch_config.md)).

## Usage

``` r
batch_reconfig_fun(path, fun, ...)
```

## Arguments

- path:

  A string of the path to the directory with the files for processing.

- fun:

  A function to process each of the files. `fun`'s first argument should
  be a string of the path to a single file. If processing is
  unsuccessful `fun` should return FALSE or throw an error (error
  messages are caught and automatically logged). If fun deletes or
  modifies the file then it is no longer considered for processing.

- ...:

  Additional arguments passed to `fun`.

## Value

An invisible character vector of the paths to the files remaining to be
processed.

## Details

`batch_reconfig_fun()` is useful if a new version of the function is
required to successfully process some of the files.

It should be noted that `batch_reconfig_fun()` does not alter the
configuration time.

In order to process previously failed files
[`batch_run()`](https://poissonconsulting.github.io/batchr/reference/batch_run.md)
should be called with `failed = NA` or `failed = TRUE`.

## See also

[`batch_process()`](https://poissonconsulting.github.io/batchr/reference/batch_process.md)
and
[`batch_config()`](https://poissonconsulting.github.io/batchr/reference/batch_config.md)
