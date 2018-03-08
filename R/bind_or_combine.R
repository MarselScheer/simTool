#'@importFrom dplyr bind_rows combine
#'@importFrom tibble as_tibble
#'@importFrom purrr map
bind_or_combine <- function(...)
{
  .list = list(...)
  if (is.matrix(.list[[1]])) 
  {
    .list <- 
      .list %>% 
      purrr::map(tibble::as_tibble)
  }
  if (is.null(names(.list[[1]])))
  {
    return(tibble::as_tibble(dplyr::combine(.list)))
  }
  do.call("bind_rows", .list)
}
