# L'Ecuyer-CMRG Seeds

Generates a named list of L'Ecuyer-CMRG seeds.

## Usage

``` r
batch_seeds(files = batch_files_remaining())
```

## Arguments

- files:

  A character vector of the names of the files.

## Value

A named list of the L'Ecuyer-CMRG seed for each file name.

## Examples

``` r
batch_seeds(c("a", "b"))
#> $a
#> [1]       10407     1258005 -1665023502  -363498263  1592400976  -155253397
#> [7]  -600925981
#> 
#> $b
#> [1]       10407 -1956927634 -1295442826  2121865102  -142604915  -317767264
#> [7]  -360586237
#> 
```
