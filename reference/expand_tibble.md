# Creates a `tibble` from All Combinations

Actually a wrapper for
[`expand.grid`](https://rdrr.io/r/base/expand.grid.html), but character
vectors will stay as characters.

## Usage

``` r
expand_tibble(...)
```

## Arguments

- ...:

  vectors, factors or a list containing these.

## Value

See [`expand.grid`](https://rdrr.io/r/base/expand.grid.html) but instead
of a [`data.frame`](https://rdrr.io/r/base/data.frame.html) a
[`tibble`](https://tibble.tidyverse.org/reference/tibble.html) is
returned.

## See also

[`expand.grid`](https://rdrr.io/r/base/expand.grid.html)

## Author

Marsel Scheer

## Examples

``` r
expand_tibble(fun = "rnorm", mean = 1:4, sd = 2:5)
#> # A tibble: 16 × 3
#>    fun    mean    sd
#>    <chr> <int> <int>
#>  1 rnorm     1     2
#>  2 rnorm     2     2
#>  3 rnorm     3     2
#>  4 rnorm     4     2
#>  5 rnorm     1     3
#>  6 rnorm     2     3
#>  7 rnorm     3     3
#>  8 rnorm     4     3
#>  9 rnorm     1     4
#> 10 rnorm     2     4
#> 11 rnorm     3     4
#> 12 rnorm     4     4
#> 13 rnorm     1     5
#> 14 rnorm     2     5
#> 15 rnorm     3     5
#> 16 rnorm     4     5
```
