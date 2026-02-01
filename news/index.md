# Changelog

## Version 1.1.9.9000

- placeholder for next development-cycle

## Version 1.1.9

CRAN release: 2026-01-24

- address deprecated .dots argument of group_by() due to upcoming dplyr
  version 1.2.0

## Version 1.1.8

CRAN release: 2025-04-09

- Upcoming R-version does not allow to apply attr() to primitives to
  assign values. Fix this by decorating primitives if they are used.

## Version 1.1.7

CRAN release: 2020-09-22

- During a replication data is generated once and then used by all data
  analyzing functions. This was already the case under the default
  setting where the generated data sets are stored by eval_tibbles() and
  is now also the case were the generated data is discarded by the
  eval_tibbles() function.

## Version 1.1.6

CRAN release: 2020-05-17

Misc:

- internal updated for dplyr 1.0.0 and vctrs 0.3.0
- internal correction based on lintr-findings

## Version 1.1.5

CRAN release: 2020-03-15

Misc:

- removing functions expandGrid() and evalGrid() (which were marked as
  deprecated in version 1.1.0) and the corresponding dependencies with
  reshape and plyr
- retrieve the RNG kind after parallel computation

## Version 1.1.4

CRAN release: 2019-09-14

Misc:

- adaptions to address the new release of tidyr 1.0.0

Note:

- due to <https://github.com/r-lib/vctrs/issues/530> reshaping the
  simulation results can be extremely slow. That costly reshaping will
  not happen if the parameter *simplify* is set to *FALSE* in
  *simTool::eval_tibbles*.

## Version 1.1.3

CRAN release: 2019-03-22

Misc:

- test cases update to address the new release of dplyr 0.8.0

## Version 1.1.2

CRAN release: 2019-02-02

Misc:

- using a workaround in examples and vignette to circumvent a bug
  introduced in purrr 0.3.0
  (<https://github.com/tidyverse/purrr/issues/629>)

## Version 1.1.1

CRAN release: 2019-01-13

Misc:

- .truth-functionality added, i.e. the parameters of the data generation
  (or alternatively a column of the data generating matrix with the name
  .truth) is passed to the the data analyzing functions, see the
  vignette for details
- Unnesting of the simulation results improved

## Version 1.1.0

CRAN release: 2018-03-26

Misc:

- Refactoring in order to remove the dependency on reshape and plyr
- The simulation itself is now a tibble instead of a list of lists

## Version 1.0.3

CRAN release: 2014-10-25

Misc:

- Adapted how libraries are loaded onto the cluster

## Version 1.0.2

CRAN release: 2014-10-15

New Features:

- The convenient function meanAndNormCI added

Misc:

- Two parameter renamed (post.proc to summary.fun and value.fun to
  convert.result.fun). Of course, renaming parameters is one of the
  worst things one can do. On the other hand, only very few users will
  be affected by these changes.

## Version 1.0.1

New Features:

- summarizing functions process also logical results (not only numeric)

Misc:

- HTML vignette (LaTeX not necessary anymore)
- additional documentation (static pdf in JSS style)

## Version 1.0

CRAN release: 2014-07-08

Initial release

- parallel computing via parallel and the ideas of L’Ecuyer 1999, 2002
  for random numbers
- fallback capability
- text progress bar
- estimation of the number of replications per hour
