call_generator_once_then_analyser <- function() {
  OFFSET <- 0
  dg1 <- Data_generator$new(generator_fun = function() {
    OFFSET <<- OFFSET + 1
    return(OFFSET + 0:2)
    })
  da1 <- Data_analyser$new(analyser_fun = function(data) min(data))
  da2 <- Data_analyser$new(analyser_fun = function(data) max(data))
  su1 <- Simulation_unit$new(
    generator = list(dg1 = dg1),
    analysers = list(da1 = da1, da2 = da2),
    replications = 2
  )
  su1$run_evaluation()
  expect_equivalent(
    current = su1$get_results()[["result"]],
    # first rep dg1 -> da1/2, then second rep dg1 -> da1/2
    target = list(1, 3, 2, 4)
  )
}
call_generator_once_then_analyser()

simulation_unit_stores_results <- function() {
  dg1 <- Data_generator$new(generator_fun = function() return(1:3))
  da1 <- Data_analyser$new(analyser_fun = function(data) min(data))
  da2 <- Data_analyser$new(analyser_fun = function(data) max(data))
  su1 <- Simulation_unit$new(
    generator = list(dg1 = dg1),
    analysers = list(da1 = da1, da2 = da2),
    replications = 1
  )
  out <- su1$run_evaluation()
  expect_equal(
    current = out,
    target = su1$get_results())
}
simulation_unit_stores_results()

post_process_after_analysing <- function() {
  dg1 <- Data_generator$new(generator_fun = function() return(1:3))
  da1 <- Data_analyser$new(analyser_fun = function(data) min(data),
    post_fun = function(x) data.frame(out = x))
  da2 <- Data_analyser$new(analyser_fun = function(data) max(data),
    post_fun = function(x) data.frame(out = x))
  su1 <- Simulation_unit$new(
    generator = list(dg1 = dg1),
    analysers = list(da1 = da1, da2 = da2),
    replications = 1
  )
  su1$run_evaluation()
  expect_equivalent(
    current = su1$get_results()[["result"]],
    target = list(
        data.frame(out = 1),
        data.frame(out = 3)
    )
  )
}
post_process_after_analysing()

aggregate_results_wo_post_process <- function() {
  dg1 <- Data_generator$new(generator_fun = function() 1:3)
  da1 <- Data_analyser$new(analyser_fun = function(data) min(data))
  da2 <- Data_analyser$new(analyser_fun = function(data) max(data))
  su1 <- Simulation_unit$new(
    generator = list(dg1 = dg1),
    analysers = list(da1 = da1, da2 = da2),
    replications = 2,
    aggregate_funs = list(mean = mean, sd = sd)
  )
  su1$run_evaluation()
  expect_equal(
    current = su1$get_results()[["result_mean"]],
    target = c(1, 3)
  )
  expect_equal(
    current = su1$get_results()[["result_sd"]],
    target = c(0, 0)
  )
}
aggregate_results_wo_post_process()

aggregate_results_w_post_process <- function() { # nolint
  dg1 <- Data_generator$new(generator_fun = function() 1:4)
  da1 <- Data_analyser$new(
    analyser_fun = function(data) c(min(data), max(data)),
    post_fun = function(v) data.frame(min = v[1], max = v[2]))
  da2 <- Data_analyser$new(
    analyser_fun = function(data) c(sum(data), prod(data)),
    post_fun = function(v) data.frame(sum = v[1], prod = v[2]))
  su1 <- Simulation_unit$new(
    generator = list(dg1 = dg1),
    analysers = list(da1 = da1, da2 = da2),
    replications = 2,
    aggregate_funs = list(mean = mean, sd = sd)
  )
  su1$run_evaluation()
  expect_equal(
    current = su1$get_results()[["min_mean"]],
    target = c(1, NA)
  )
  expect_equal(
    current = su1$get_results()[["max_mean"]],
    target = c(4, NA)
  )
  expect_equal(
    current = su1$get_results()[["sum_mean"]],
    target = c(NA, 10)
  )
  expect_equal(
    current = su1$get_results()[["prod_mean"]],
    target = c(NA, 24)
  )
  expect_equal(
    current = su1$get_results()[["min_sd"]],
    target = c(0, NA)
  )
  expect_equal(
    current = su1$get_results()[["max_sd"]],
    target = c(0, NA)
  )
  expect_equal(
    current = su1$get_results()[["sum_sd"]],
    target = c(NA, 0)
  )
  expect_equal(
    current = su1$get_results()[["prod_sd"]],
    target = c(NA, 0)
  )

}
aggregate_results_w_post_process()
