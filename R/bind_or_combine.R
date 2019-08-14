#' @importFrom dplyr bind_rows combine
#' @importFrom tibble as_tibble
#' @importFrom purrr map
bind_or_combine <- function(...) {
  .list <- list(...)
  
  if (is.matrix(.list[[1]])) {
    .list <- purrr::map(.list, purrr::partial(tibble::as_tibble, .name_repair = repair_col_names))
  }
  if (is.null(names(.list[[1]]))) {
    return(tibble::enframe(dplyr::combine(.list), name = NULL))
  }
  do.call("bind_rows", .list)
}

repair_col_names <- function(x) {
  names(x) <- x
  names(tibble::repair_names(x))
}