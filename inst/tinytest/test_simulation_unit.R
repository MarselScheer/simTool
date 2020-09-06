call_generator_and_analyser <- function() {
  dg1 <- Data_generator$new(generator_fun = function() return(1:3))
  da1 <- Data_analyser$new(analyser_fun = function(data) min(data))
  da2 <- Data_analyser$new(analyser_fun = function(data) max(data))
  su1 <- Simulation_unit$new(
    generator = list(dg1 = dg1),
    analysers = list(da1 = da1, da2 = da2),
    replications = 2
  )
  expect_equivalent(
    current = su1$run_evaluation(),
    target = data.table::data.table(
      generator = "dg1",
      analyser = c("da1", "da2", "da1", "da2"),
      replication = c(1, 1, 2, 2),
      result = list(1, 3, 1, 3)
    )
  )
  expect_equivalent(
    current = su1$run_evaluation(),
    target = su1$get_results())
}
call_generator_and_analyser()

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
  expect_equivalent(
    current = su1$run_evaluation(),
    target = data.table::data.table(
      generator = "dg1",
      analyser = c("da1", "da2"),
      replication = c(1, 1),
      result = list(
        data.frame(out = 1),
        data.frame(out = 3))
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
  expect_equivalent(
    current = su1$run_evaluation(),
    target = data.table::data.table(
      generator = "dg1",
      analyser = c("da1", "da2"),
      result_mean = c(1, 3),
      result_sd = c(0, 0),
      key = c("generator", "analyser")
    )
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
  expect_equivalent(
    current = su1$run_evaluation(),
    target = data.table::data.table(
      generator = "dg1",
      analyser = c("da1", "da2"),
      min_mean = c(1, NA),
      max_mean = c(4, NA),
      sum_mean = c(NA, 10),
      prod_mean = c(NA, 24),
      min_sd = c(0, NA),
      max_sd = c(0, NA),
      sum_sd = c(NA, 0),
      prod_sd = c(NA, 0),
      key = c("generator", "analyser")
    )
  )
}
aggregate_results_w_post_process()
