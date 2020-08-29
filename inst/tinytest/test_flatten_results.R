extract_if_only_one_result_is_available <- function() {
  data_and_results <-
    list(
      list(list(data = "1. set", results = list(c(a = 1, b = 2)))),
      list(list(data = "2. set", results = list(c(a = 3, b = 4))))
    )
  expect_identical(
    simTool:::flatten_results(data_and_results),
    list(c(a = 1, b = 2), c(a = 3, b = 4))
  )
}
extract_if_only_one_result_is_available()

extract_if_more_than_one_result_is_available <- function() {
  data_and_results <-
    list(
      list(list(data = "1. set",
        results = list(c(a = 1, b = 2), 10, matrix(1, 2, 2)))),
      list(list(data = "2. set",
        results = list(c(a = 3, b = 4), 20, matrix(2, 2, 2))))
    )
  expect_identical(
    simTool:::flatten_results(data_and_results),
    list(
      c(a = 1, b = 2), 10, matrix(1, 2, 2),
      c(a = 3, b = 4), 20, matrix(2, 2, 2)
    )
  )
}
extract_if_more_than_one_result_is_available()
