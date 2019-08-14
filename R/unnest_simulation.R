unnest_simulation <- function(e) {
  s <- try(tidyr::unnest(e$simulation), silent = TRUE)
  if (class(s)[[1]] != "try-error") {
    e$simulation <- s
    return(e)
  }
  s <- e$simulation
  s$.row_nmb <- base::seq_len(nrow(s))

  r <- with(NULL, dplyr::select(s, results, .row_nmb))
  r <- try(tidyr::unnest(r), silent = TRUE)

  if (class(r)[[1]] == "try-error") {
    return(e)
  }

  s <- with(NULL, dplyr::select(s, -results))
  s <- dplyr::inner_join(s, r, by = ".row_nmb")
  s <- with(NULL, dplyr::select(s, -.row_nmb))
  e$simulation <- s
  e
}
