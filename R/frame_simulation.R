#' @importFrom tidyr crossing
#' @importFrom purrr flatten
frame_simulation <- function(dg, pg, sim, summary_fun) {
  # if summary_fun is NULL, names(summary_fun) is NULL and crossing ignores it.
  ret <- tidyr::crossing(dg, replications = seq_along(sim[[1]]), summary_fun = names(summary_fun), pg)
  ret <- tibble::as_tibble(ret)

  flat_results <- flatten_results(sim)

  if (!is.null(summary_fun)) {
    flat_results <- purrr::flatten(flat_results)
  }
  ret$results <- flat_results
  ret
}
