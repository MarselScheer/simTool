create_a_tibble_containing_the_results_for_every_replication <- function() { # nolint
  dg <- expand_tibble(fun = c("Gen A", "Gen B"), para = 1:2)
  pg <- expand_tibble(proc = c("Ana A", "Ana B", "Ana C"))
  sim <-
    list(
      list(
        list(data = "1. set",
          results = list(c(res = "Gen A 1 Ana A"),
            c(res = "Gen A 1 Ana B"),
            c(res = "Gen A 1 Ana C"))),
        list(data = "2. set",
          results = list(c(res = "Gen A 1 Ana A"),
            c(res = "Gen A 1 Ana B"),
            c(res = "Gen A 1 Ana C")))
      ),

      list(
        list(data = "1. set",
          results = list(c(res = "Gen B 1 Ana A"),
            c(res = "Gen B 1 Ana B"),
            c(res = "Gen B 1 Ana C"))),
        list(data = "2. set",
          results = list(c(res = "Gen B 1 Ana A"),
            c(res = "Gen B 1 Ana B"),
            c(res = "Gen B 1 Ana C")))
      ),

      list(
        list(data = "1. set",
          results = list(c(res = "Gen A 2 Ana A"),
            c(res = "Gen A 2 Ana B"),
            c(res = "Gen A 2 Ana C"))),
        list(data = "2. set",
          results = list(c(res = "Gen A 2 Ana A"),
            c(res = "Gen A 2 Ana B"),
            c(res = "Gen A 2 Ana C")))
      ),

      list(
        list(data = "1. set",
          results = list(c(res = "Gen B 2 Ana A"),
            c(res = "Gen B 2 Ana B"),
            c(res = "Gen B 2 Ana C"))),
        list(data = "2. set",
          results = list(c(res = "Gen B 2 Ana A"),
            c(res = "Gen B 2 Ana B"),
            c(res = "Gen B 2 Ana C")))
      )
    )

  fs <- simTool:::frame_simulation(dg, pg, sim, NULL)
  fs <- tidyr::unnest(fs, cols = c("results"))
  fs <- tidyr::unite(fs, "expected", c("fun", "para", "proc"), sep = " ")

  expect_equivalent(fs$results, fs$expected)
}
create_a_tibble_containing_the_results_for_every_replication()

tibble_with_results_summarized_by_one_summary_function <- function() { # nolint
  dg <- expand_tibble(fun = c("Gen A", "Gen B"), para = 1:2)
  pg <- expand_tibble(proc = c("Ana A", "Ana B", "Ana C"))
  sim <-
    list(
      list(
        list(
          results =
            list(mean = list(c(res = "Gen A 1 Ana A"),
              c(res = "Gen A 1 Ana B"),
              c(res = "Gen A 1 Ana C")))
        )
      ),

      list(
        list(
          results =
            list(mean = list(c(res = "Gen B 1 Ana A"),
              c(res = "Gen B 1 Ana B"),
              c(res = "Gen B 1 Ana C")))
        )
      ),

      list(
        list(
          results =
            list(mean = list(c(res = "Gen A 2 Ana A"),
              c(res = "Gen A 2 Ana B"),
              c(res = "Gen A 2 Ana C")))
        )
      ),

      list(
        list(
          results =
            list(mean = list(c(res = "Gen B 2 Ana A"),
              c(res = "Gen B 2 Ana B"),
              c(res = "Gen B 2 Ana C")))
        )
      )
    )

  fs <- simTool:::frame_simulation(dg, pg, sim, mean)
  fs <- tidyr::unnest(fs, cols = c("results"))
  fs <- tidyr::unite(fs, "expected", c("fun", "para", "proc"), sep = " ")

  expect_equivalent(fs$results, fs$expected)
}
tibble_with_results_summarized_by_one_summary_function()
