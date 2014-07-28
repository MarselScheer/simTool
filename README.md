simTool
=======

An R-Package that facilitates simulation studies. It disengages the researcher from administrative source code.

The *simTool* package is designed for statistical simulations that
have two components. One component generates the data and the other one
analyzes the data. The main aims of the *simTool* package are the reduction
of the administrative source code (mainly loops and management code for the results) 
and a simple applicability
of the package that allows the user to quickly learn how to work with the
*simTool* package. Parallel computing is also supported. Finally, convenient
functions are provided to summarize the simulation results.

To install:

* For the latest version: devtools::install_github("MarselScheer/simTool", build_vignettes=FALSE)
  or devtools::install_github("MarselScheer/simTool") if LaTeX is available  
* For the CRAN version: install.packages("simTool")
  
