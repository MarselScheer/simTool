#' Conduct Simulation Studies with a Minimal Amount of Source Code
#'
#' The \code{simTool} package is designed for statistical simulations that
#' have two components. One component generates the data and the other one
#' analyzes the data. The main aims of the \code{simTool} package are the reduction
#' of the administrative source code (mainly loops and management code for the
#' results) and a simple applicability of the package that allows the user to
#' quickly learn how to work with the \code{simTool} package. Parallel computing is
#' also supported. Finally, convenient functions are provided to summarize the
#' simulation results.
#'
#' \tabular{ll}{ Package: \tab simTool\cr Type: \tab Package\cr Version: \tab
#' 1.0.3\cr Date: \tab 2013-02-21\cr License: \tab GPL-3\cr }
#' \code{evalGrids} is the workhorse. \code{as.data.frame} is function coercing the result object of evalGrids to
#' a data.frame. \code{expandGrid} is only a convenient function
#'
#' @name simTool-package
#' @aliases simTool-package simTool
#' @docType package
#' @author Marsel Scheer
#'
#' Maintainer: scheer@@freescience.de
#' @keywords utilities
#' @concept simulations 
#' @concept parallel 
#' @concept computing
#' @examples
#'
#' dg = expand_tibble(fun="rexp", n=c(10, 20), rate=1:2)
#' pg = expand_tibble(proc="summary")
#' eval_tibbles(dg, pg, replications=3)
#' eval_tibbles(dg, pg, replications=3, summary_fun = list(mean = mean))
#' eval_tibbles(dg, pg, replications=3, summary_fun = list(mean = mean, sd = sd))
#'
NULL
