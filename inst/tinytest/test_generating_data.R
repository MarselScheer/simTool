datagen_instance_can_store_data_generating_function <- function() { # nolint
  f <- function() return(42)
  dg <- Data_generator$new(generator_fun = f)
  expect_equal(dg$get_generator(), f)
  expect_equal(dg$generator(), 42)
}
datagen_instance_can_store_data_generating_function()
