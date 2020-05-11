#' Printing simulation results
#'
#'  Prints objects created by \code{eval_tibbles()}
#'
#'
#' @param x  object of class \code{eval_tibbles}
#' @param ... not used. only necessary to define the function consistently
#'  with respect to \code{print(x, ...)}
#' @author  Marsel Scheer
#' @export
print.eval_tibbles <- function(x, ...) {
  print(x$simulation)
  cat(sprintf("Number of data generating functions: %i\n", nrow(x$data_grid)))
  cat(sprintf("Number of analyzing procedures: %i\n", nrow(x$proc_grid)))
  cat(sprintf("Number of replications: %i\n", x$replications))
  cat(sprintf("Estimated replications per hour: %s\n", x$est_reps_per_hour))
  cat(sprintf("Start of the simulation: %s\n", x$start_time))
  cat(sprintf("End of the simulation: %s\n", x$end_time))
}
