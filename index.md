# simTool

An R-Package that facilitates simulation studies. It disengages the
researcher from administrative source code.

The *simTool* package is designed for statistical simulations that have
two components. One component generates the data and the other one
analyzes the data. The main aims of the *simTool* package are the
reduction of the administrative source code (mainly loops and management
code for the results) and a simple applicability of the package that
allows the user to quickly learn how to work with the *simTool* package.
Parallel computing is also supported. Finally, convenient functions are
provided to summarize the simulation results.

## Example

This small simulation (using 4 cores) illustrates how the confidence
interval based on the t-distribution performs on exponential distributed
random variables. The following lines generate exponential distributed
random variables of size 10, 50, 100, and 1000. Afterwards the *t.test*
using confidence levels 0.8, 0.9, 0.95 are applied. This is repeated
1000 times to estimate the coverage:

``` R
library(simTool)
dg <- expand_tibble(fun = "rexp", rate = 10, n = c(10L, 50L, 100L, 1000L))
pg <- expand_tibble(proc = "t.test", conf.level = c(0.8, 0.9, 0.95))
et <- eval_tibbles(dg, pg, 
  ncpus = 4,
  replications = 10^3,
  post_analyze = function(ttest) tibble::tibble(
    coverage = ttest$conf.int[1] <= 1 / 10 && 1 / 10 <= ttest$conf.int[2]),
  summary_fun = list(mean = mean)
)
et
#> # A tibble: 12 × 8
#>    fun    rate     n replications summary_fun proc   conf.level coverage
#>    <chr> <dbl> <int>        <int> <chr>       <chr>       <dbl>    <dbl>
#>  1 rexp     10    10            1 mean        t.test       0.8     0.754
#>  2 rexp     10    10            1 mean        t.test       0.9     0.855
#>  3 rexp     10    10            1 mean        t.test       0.95    0.905
#>  4 rexp     10    50            1 mean        t.test       0.8     0.808
#>  5 rexp     10    50            1 mean        t.test       0.9     0.905
#>  6 rexp     10    50            1 mean        t.test       0.95    0.945
#>  7 rexp     10   100            1 mean        t.test       0.8     0.792
#>  8 rexp     10   100            1 mean        t.test       0.9     0.895
#>  9 rexp     10   100            1 mean        t.test       0.95    0.936
#> 10 rexp     10  1000            1 mean        t.test       0.8     0.796
#> 11 rexp     10  1000            1 mean        t.test       0.9     0.897
#> 12 rexp     10  1000            1 mean        t.test       0.95    0.953
#> Number of data generating functions: 4
#> Number of analyzing procedures: 3
#> Number of replications: 1000
#> Estimated replications per hour: 1329200
#> Start of the simulation: 2025-04-10 18:09:19.481191
#> End of the simulation: 2025-04-10 18:09:22.189585
```

## Installation

You can install simTool from github with:

``` R
remotes::install_github("MarselScheer/simTool")
```

Or from CRAN with:

``` R
install.packages("simTool")
```

## Test suite

With tinytest you can rerun the test for simTool by calling

``` R
# install.packages("tinytest")
tinytest::test_package("simTool")
```
