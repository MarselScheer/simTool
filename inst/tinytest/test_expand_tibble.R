cross_product_of_complicated_objects <- function() { # nolint
  tmp <- expand_tibble(
    desc = letters[1:2],
    mod = list(
      lm(Sepal.Length ~ Sepal.Width, data = iris),
      lm(Sepal.Length ~ Petal.Width, data = iris)
    )
  )

  er <- dplyr::tibble(
    desc = c("a", "b", "a", "b"),
    mod = list(
      lm(Sepal.Length ~ Sepal.Width, data = iris),
      lm(Sepal.Length ~ Sepal.Width, data = iris),
      lm(Sepal.Length ~ Petal.Width, data = iris),
      lm(Sepal.Length ~ Petal.Width, data = iris)
    )
  )
  expect_equal(tmp$desc, er$desc)
  expect_equal(
    tmp$mod,
    er$mod,
    info = "expand_tibble also works with model fits")
}
cross_product_of_complicated_objects()
