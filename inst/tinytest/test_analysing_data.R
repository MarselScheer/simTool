can_set_get_and_call_analyser <- function() {
  f <- function(arg) arg^2
  da <- Data_analyser$new(analyser_fun = f)
  expect_equal(da$get_analyser(), f)
  expect_equal(da$analyser(1), 1)
  expect_equal(da$analyser(2), 4)
}
can_set_get_and_call_analyser()

can_post_process_after_analyser <- function() {
  f <- function(arg) arg^2
  da <- Data_analyser$new(
    analyser_fun = f,
    post_fun = function(x) data.frame(t(x)))

  expect_equal(da$analyser(1), data.frame(t.x. = 1))
  expect_equal(da$analyser(1:2), data.frame(X1 = 1, X2 = 4))
}
can_post_process_after_analyser()
