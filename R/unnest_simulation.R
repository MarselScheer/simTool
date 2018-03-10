unnest_simulation <- function(e) {
  s <- try(tidyr::unnest(e$simulation), silent = TRUE)
  if (length(s) > 1 || class(s) != "try-error") {
    e$simulation <- s
  }
  e
}
