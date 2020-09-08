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

data_analyser_creates_default_shortname <- function() {

  dg <- Data_analyser$new(analyser_fun = runif)
  sn <- strsplit(x = dg$get_short_name(), split = "_")[[1]]
  expect_equal("runif", sn[1])
  expect_true(difftime(
    time1 = Sys.Date(),
    time2 = as.Date(sn[2], format = "%s"),
    units = "days") < 1)

}
data_analyser_creates_default_shortname()

data_analyser_stores_shortname <- function() {

  short_name <- "abcd"
  dg <- Data_analyser$new(
    analyser_fun = runif,
    short_name = short_name)
  expect_equal(dg$get_short_name(), short_name)

}
data_analyser_stores_shortname()
