f <- function(arg) arg^2
da <- Data_analyser$new(analyser_fun = f)
#-# can set and get the analyser function
expect_equal(da$get_analyser(), f)

#-# can call analyser function with argument(s)
expect_equal(da$analyser(1), 1)
expect_equal(da$analyser(2), 4)


f <- function(arg) arg^2
da <- Data_analyser$new(
  analyser_fun = f,
  post_fun = function(x) data.frame(t(x)))

#-# can call analyser function with argument(s)
expect_equal(da$post_fun(da$analyser(1)), data.frame(t.x. = 1))
expect_equal(da$post_fun(da$analyser(1:2)), data.frame(X1 = 1, X2 = 4))
