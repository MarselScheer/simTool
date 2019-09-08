Version 1.1.4
=========================

Misc: 

* adaptions to address the new release of tidyr 1.0.0 

Note: 

* due to https://github.com/r-lib/vctrs/issues/530 reshaping the simulation results can be extremely slow. That costly reshaping will not happen if the parameter *simplify* is set to *FALSE* in *simTool::eval_tibbles*. 


Version 1.1.3
=========================

Misc: 

* test cases update to address the new release of dplyr 0.8.0 

Version 1.1.2
=========================

Misc: 

* using a workaround in examples and vignette to circumvent a bug introduced in purrr 0.3.0 (https://github.com/tidyverse/purrr/issues/629)


Version 1.1.1
=========================

Misc: 

* .truth-functionality added, i.e. the parameters of the data generation (or alternatively a column of the data generating matrix with the name .truth) is passed to the the data analyzing functions, see the vignette for details
* Unnesting of the simulation results improved



Version 1.1.0
=========================

Misc: 

* Refactoring in order to remove the dependency on reshape and plyr
* The simulation itself is now a tibble instead of a list of lists



Version 1.0.3
=========================

Misc:

* Adapted how libraries are loaded onto the cluster



Version 1.0.2
=========================

New Features:

* The convenient function meanAndNormCI added

Misc:

* Two parameter renamed (post.proc to summary.fun and value.fun to convert.result.fun). 
  Of course, renaming parameters is one of the worst things one can do. On the other
  hand, only very few users will be affected by these changes.


Version 1.0.1
=========================

New Features:

* summarizing functions process also logical results (not only numeric)

Misc:

* HTML vignette (LaTeX not necessary anymore)
* additional documentation  (static pdf in JSS style)


Version 1.0
=========================

Initial release

* parallel computing via parallel and the ideas of L'Ecuyer 1999, 2002 for random numbers
* fallback capability
* text progress bar
* estimation of the number of replications per hour
