simple_unnesting <- function() {

  s <- expand_tibble(a = 1:2, b = 3, results = list(r1 = 1L, r2 = 2L))
  e <- list(simulation = s)
  expected_df <- s
  expected_df$results <- c(1, 1, 2, 2)
  expect_equivalent(
    list(simulation = expected_df),
    simTool:::unnest_simulation(e))

}
simple_unnesting()

unnesting_with_further_matrices_in_the_row <- function() {

  s <- simTool::expand_tibble(
    a = 1:2,
    df = list(matrix(1:6, 3, 2), matrix(1:8, 4, 2)),
    results = list(r1 = 1L, r2 = 2L))
  e <- list(simulation = s)
  expected_df <- s
  expected_df$results <- c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L)
  expect_equivalent(
    list(simulation = expected_df),
    simTool:::unnest_simulation(e)
  )

}
unnesting_with_further_matrices_in_the_row()

unnesting_with_further_matrices_in_the_row_and_result_itself_contains_data.frames <- function() {

  s <- simTool::expand_tibble(
    a = 1:2,
    df = list(matrix(1:6, 3, 2), matrix(1:8, 4, 2)),
    results = list(r1 = as.data.frame(matrix(1:4, 2, 2)),
      r2 = as.data.frame(matrix(1:9, 3, 3))))
  e <- list(simulation = s)

  expected_df <- s
  expected_df$id <- 1:nrow(expected_df)
  results <- purrr::map_dfr(split(expected_df, expected_df$id), function(sub) {
    ret <- sub$results[[1]]
    ret$id <- sub$id
    ret
  })
  expected_df <- dplyr::left_join(expected_df, results, by = "id")
  expected_df$results <- NULL
  expected_df$id <- NULL
  expected_df
  expect_equivalent(
    list(simulation = expected_df),
    simTool:::unnest_simulation(e))

}

unnesting_with_further_matrices_in_the_row_and_result_itself_contains_data.frames()


if_unnesting_is_not_possible_then_nothing_is_changed <- function() {
  x <- 1:2
  y <- 2:3
  s <- simTool::expand_tibble(a = 1:2, b = 2:3, results = list(lm(y ~ x)))
  e <- list(simulation = s)
  expect_identical(simTool:::unnest_simulation(e), e)
}
if_unnesting_is_not_possible_then_nothing_is_changed()
