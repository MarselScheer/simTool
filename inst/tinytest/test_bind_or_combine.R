unnamed_vector_combined_to_one_column <- function() {
  expect_identical(
    bind_or_combine(1:2, 3:5),
    tibble::enframe(x = 1:5, name = NULL))
}
unnamed_vector_combined_to_one_column()

named_vectors_combined_to_multiple_columns <- function() {
  expect_identical(
    bind_or_combine(c(min = 1, max = 1), c(min = 2, max = 3), c(a = 1)),
    tibble::tibble(min = c(1, 2, NA), max = c(1, 3, NA), a = c(NA, NA, 1))
  )
}
named_vectors_combined_to_multiple_columns()

repair_colnames <- function() {
  expect_identical(
    repair_col_names(c("", "", "")),
    paste0("V", 1:3)
  )
  expect_identical(
    repair_col_names(c("V1", "d", "")),
    c("V1", "d", "V2")
  )
}
repair_colnames()

matrix_is_converted_to_tibble <- function() {
  expect_identical(
    bind_or_combine(matrix(1:6, 2, 3)),
    tibble::tibble(V1 = 1:2, V2 = 3:4, V3 = 5:6)
  )
}
matrix_is_converted_to_tibble()

matrix_is_converted_to_tibble_Colnames_preserved <- function() {
  expect_identical(
    bind_or_combine(structure(1:6, .Dim = 3:2,
      .Dimnames = list(NULL, letters[1:2]))),
    tibble::tibble(a = 1:3, b = 4:6)
  )
}
matrix_is_converted_to_tibble_Colnames_preserved()
