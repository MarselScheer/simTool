datagen_instance_can_store_data_generating_function <- function() { # nolint
  f <- function() return(42)
  dg <- Data_generator$new(generator_fun = f)
  expect_equal(dg$get_generator(), f)
  expect_equal(dg$generator(), 42)
}
datagen_instance_can_store_data_generating_function()

datagen_creates_default_shortname <- function() {

  dg <- Data_generator$new(generator_fun = runif)
  sn <- strsplit(x = dg$get_short_name(), split = "_")[[1]]
  expect_equal("runif", sn[1])
  expect_true(difftime(
    time1 = Sys.Date(),
    time2 = as.Date(sn[2], format = "%s"),
    units = "days") < 1)

}
datagen_creates_default_shortname()

datagen_stores_shortname <- function() {

  short_name <- "abcd"
  dg <- Data_generator$new(
    generator_fun = runif,
    short_name = short_name)
  expect_equal(dg$get_short_name(), short_name)

}
datagen_stores_shortname()
