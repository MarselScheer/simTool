#' @importFrom purrr flatten map
flatten_results <- function(sim) {
  sim <- purrr::flatten(sim)
  sim <- purrr::map(sim, ~ `[[`(., "results"))
  purrr::flatten(sim)
}
