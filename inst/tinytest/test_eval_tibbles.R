truth_parameter_of_data_analyzer_can_access_matrix_of_data_generator <- function() {

  gen_data1 <- function(df) {
    df[[1]][, 1]
  }
  gen_data2 <- function(df) {
    df[[1]][, 2]
  }
  dg <- expand_tibble(
    fun = c("gen_data1", "gen_data2"),
    df = list(
      matrix(1:6, 3, 2),
      matrix(1:8, 4, 2)
    )
  )
  f <- function(data, .truth) {
    .truth$df[[1]]
  }
  pg <- expand_tibble(proc = "f")
  eg <- eval_tibbles(dg, pg, rep = 2, envir = environment(), simplify = FALSE)
  expect_true(
    all(
      sapply(
        1:8,
        function(i) all(eg$simulation$df[[i]] == eg$simulation$results[[i]])
      )
    ),
    info = "data analyzer can use explicit .truth-parameter"
  )


  dg <- expand_tibble(
    fun = c("gen_data1", "gen_data2"),
    df = list(
      matrix(1:6, 3, 2),
      matrix(1:8, 4, 2)
    ),
    xyz = NA
  )
  f <- function(data, .truth) {
    .truth$df[[1]]
  }
  pg <- expand_tibble(proc = "f")
  eg <- eval_tibbles(dg, pg, rep = 2, envir = environment(), simplify = FALSE)
  expect_true(
    all(
      sapply(
        1:8,
        function(i) all(eg$simulation$df[[i]] == eg$simulation$results[[i]])
      )
    ),
    info = "NA in data analyzer is not part of .truth"
  )


  f <- function(data, .truth) {
    0
  }
  post_ana <- function(result, .truth) {
    .truth$df[[1]]
  }

  pg <- expand_tibble(proc = "f")
  eg <- eval_tibbles(dg, pg,
    rep = 2, envir = environment(), simplify = FALSE,
    post_analyze = post_ana
  )
  expect_true(
    all(
      sapply(
        1:8,
        function(i) all(eg$simulation$df[[i]] == eg$simulation$results[[i]])
      )
    ),
    info = "post analyzer can use explicit .truth-parameter"
  )

}
truth_parameter_of_data_analyzer_can_access_matrix_of_data_generator()

################################################################

truth_column_passed_to_analyzers <- function() {

  gen_data1 <- function(df) {
    df[[1]][, 1]
  }
  gen_data2 <- function(df) {
    df[[1]][, 2]
  }
  dg <- expand_tibble(
    fun = c("gen_data1", "gen_data2"),
    df = list(
      matrix(1:6, 3, 2),
      matrix(1:8, 4, 2)
    )
  )
  dg$.truth <- 1:4
  f <- function(data, .truth) {
    .truth
  }
  pg <- expand_tibble(proc = "f")
  eg <- eval_tibbles(dg, pg, rep = 2, envir = environment(), simplify = TRUE)
  expect_identical(eg$simulation$results, c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
    info = "truth-column passed to data analyzer")


  f <- function(data, .truth) {
    0
  }
  post_ana <- function(result, .truth) {
    .truth
  }
  pg <- expand_tibble(proc = c("f"))
  eg <- eval_tibbles(dg, pg,
    rep = 2, envir = environment(), simplify = TRUE,
    post_analyze = post_ana
  )
  expect_identical(eg$simulation$results, c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
    info = "truth-column passed to post analyzer")

}
truth_column_passed_to_analyzers()

##############################################################

mixture_of_data_analyzer_with_and_without_truth_parameter <- function() {

  gen_data1 <- function(df) {
    df[[1]][, 1]
  }
  gen_data2 <- function(df) {
    df[[1]][, 2]
  }
  dg <- expand_tibble(
    fun = c("gen_data1", "gen_data2"),
    df = list(
      matrix(1:6, 3, 2),
      matrix(1:8, 4, 2)
    )
  )
  dg$.truth <- 1:4
  f <- function(data, .truth) {
    0
  }
  pg <- expand_tibble(proc = c("f", "min"))
  eg <- eval_tibbles(dg, pg, rep = 2, envir = environment(), simplify = TRUE)
  expect_identical(eg$simulation$results,
    c(0, 1, 0, 1, 0, 4, 0, 4, 0, 1, 0, 1, 0, 5, 0, 5))

}
mixture_of_data_analyzer_with_and_without_truth_parameter()


###########################################################

error_with_respect_to_truth_column <- function() {

  gen_data1 <- function(df) {
    df[[1]][, 1]
  }
  gen_data2 <- function(df) {
    df[[1]][, 2]
  }
  dg <- expand_tibble(
    fun = c("gen_data1", "gen_data2"),
    df = list(
      matrix(1:6, 3, 2),
      matrix(1:8, 4, 2)
    )
  )
  dg$.truth <- 1:4
  f <- function(data, .truth) {
    0
  }
  pg <- expand_tibble(proc = c("f"), .truth = 1)
  expect_error(eval_tibbles(dg, pg, rep = 2, envir = environment(),
    simplify = TRUE),
    "\\.truth.*not allowed")

  dg$a <- 4:7
  names(dg)[4] <- ".truth"
  pg <- expand_tibble(proc = c("f"))
  expect_error(eval_tibbles(dg, pg, rep = 2, envir = environment(),
    simplify = TRUE),
    "only one column with name '.truth' allowed")

}
error_with_respect_to_truth_column()


#############################################################


results_and_data_is_stored <- function() {

  rng <- function(data, ...) {
    ret <- range(data)
    names(ret) <- c("min", "max")
    ret
  }
  gen_data1 <- function(df) {
    df[[1]][, 1]
  }
  gen_data2 <- function(df) {
    df[[1]][, 2]
  }
  dg <- expand_tibble(
    fun = c("gen_data1", "gen_data2"),
    df = list(
      matrix(1:6, 3, 2),
      matrix(1:8, 4, 2)
    )
  )
  pg <- expand_tibble(proc = "rng")
  eg <- eval_tibbles(dg, pg, rep = 2, envir = environment(), simplify = FALSE)
  expected_df <- structure(list(
    fun = c("gen_data1", "gen_data1", "gen_data2", "gen_data2",
      "gen_data1", "gen_data1", "gen_data2", "gen_data2"),
    df = list(
      structure(1:6, .Dim = c(3L, 2L)),
      structure(1:6, .Dim = c(3L, 2L)), structure(1:6, .Dim = c(3L, 2L)),
      structure(1:6, .Dim = c(3L, 2L)), structure(1:8, .Dim = c(4L, 2L)),
      structure(1:8, .Dim = c(4L, 2L)), structure(1:8, .Dim = c(4L, 2L)),
      structure(1:8, .Dim = c(4L, 2L))
    ),
    replications = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
    proc = c("rng", "rng", "rng", "rng", "rng", "rng", "rng", "rng"),
    results = list(
      structure(c(1L, 3L), .Names = c("min", "max")),
      structure(c(1L, 3L), .Names = c("min", "max")),
      structure(c(4L, 6L), .Names = c("min", "max")),
      structure(c(4L, 6L), .Names = c("min", "max")),
      structure(c(1L, 4L), .Names = c("min", "max")),
      structure(c(1L, 4L), .Names = c("min", "max")),
      structure(c(5L, 8L), .Names = c("min", "max")),
      structure(c(5L, 8L), .Names = c("min", "max"))
    )
  ),
  .Names = c("fun", "df", "replications", "proc", "results"),
  row.names = c(NA, -8L),
  class = c("tbl_df", "tbl", "data.frame")
  )
  for (col in colnames(eg$simulation)) {
    expect_identical(eg$simulation[[col]], expected_df[[col]],
      info = "results are stored in element simulation")
  }

  expect_identical(eg$generated_data,
    list(1:3, 1:3, 4:6, 4:6, 1:4, 1:4, 5:8, 5:8),
    info = "generated data is stored")

  eg <- eval_tibbles(dg, pg, discard_generated_data = TRUE, envir = environment())
  expect_false(all(grepl("generated_data", names(eg))),
    info = "data is discarded")


  cl <- parallel::makeCluster(rep("localhost", 2), type = "PSOCK")
  eg <- eval_tibbles(dg, pg, rep = 2, envir = environment(),
    simplify = FALSE, cluster = cl)
  expect_identical(eg$generated_data,
    list(1:3, 1:3, 4:6, 4:6, 1:4, 1:4, 5:8, 5:8),
    info = "generated data stored if cluster is used")
  eg <- eval_tibbles(dg, pg,
    rep = 2, envir = environment(), simplify = FALSE, cluster = cl,
    discard_generated_data = TRUE
  )
  expect_false(all(grepl("generated_data", names(eg))),
    info = "generated data is discarded if cluster is used")

  parallel::stopCluster(cl)
}
results_and_data_is_stored()


#################################################################
results_stored_in_simulation <- function() {

  gen_mat <- function(df) {
    df[[1]][, 1]
  }
  mat_mult <- function(mat_a, mat_b) {
    mat_a %*% mat_b[[1]]
  }
  dg <- expand_tibble(
    fun = c("gen_mat"),
    df = list(
      matrix(1:4, 2, 2),
      matrix(5:8, 2, 2)
    )
  )
  pg <- expand_tibble(
    proc = "mat_mult",
    mat_b = list(matrix(1:4, 2, 2), matrix(5:8, 2, 2))
  )
  eg <- eval_tibbles(dg, pg, rep = 1, envir = environment(), simplify = FALSE)
  expected_df <- structure(
    list(
      fun = c("gen_mat", "gen_mat", "gen_mat", "gen_mat"),
      df = list(
        structure(1:4, .Dim = c(2L, 2L)), structure(1:4, .Dim = c(2L, 2L)),
        structure(5:8, .Dim = c(2L, 2L)), structure(5:8, .Dim = c(2L, 2L))
      ),
      replications = c(1L, 1L, 1L, 1L),
      proc = c("mat_mult", "mat_mult", "mat_mult", "mat_mult"),
      mat_b = list(
        structure(1:4, .Dim = c(2L, 2L)),
        structure(5:8, .Dim = c(2L, 2L)), structure(1:4, .Dim = c(2L, 2L)),
        structure(5:8, .Dim = c(2L, 2L))
      ),
      results = list(
        structure(c(5, 11), .Dim = 1:2), structure(c(17, 23), .Dim = 1:2),
        structure(c(17, 39), .Dim = 1:2), structure(c(61, 83), .Dim = 1:2)
      )
    ),
    .Names = c("fun", "df", "replications", "proc", "mat_b", "results"),
    row.names = c(NA, -4L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  for (col in colnames(eg$simulation)) {
    expect_identical(eg$simulation[[col]], expected_df[[col]])
  }

}
results_stored_in_simulation()

##################################################################

grids_preserved <- function() {

  rng <- function(data, ...) {
    ret <- range(data)
    names(ret) <- c("min", "max")
    ret
  }
  dg <- expand_tibble(fun = "seq_len", length.out = 1:3)
  pg <- expand_tibble(proc = "rng")
  eg <- eval_tibbles(dg, pg, rep = 2, envir = environment(), simplify = FALSE)
  expected_df <- structure(list(
    fun = c(
      "seq_len", "seq_len", "seq_len", "seq_len",
      "seq_len", "seq_len"
    ), length.out = c(1L, 1L, 2L, 2L, 3L, 3L),
    replications = c(1L, 2L, 1L, 2L, 1L, 2L), proc = c(
      "rng",
      "rng", "rng", "rng", "rng", "rng"
    ), results = list(
      structure(c(
        1L,
        1L
      ), .Names = c("min", "max")), structure(c(1L, 1L), .Names = c(
        "min",
        "max"
      )), structure(1:2, .Names = c("min", "max")), structure(1:2, .Names = c(
        "min",
        "max"
      )), structure(c(1L, 3L), .Names = c("min", "max")),
      structure(c(1L, 3L), .Names = c("min", "max"))
    )
  ), .Names = c(
    "fun",
    "length.out", "replications", "proc", "results"
  ), row.names = c(
    NA,
    -6L
  ), class = c("tbl_df", "tbl", "data.frame"))
  expect_true(all(eg$data_grid == dg),
    info = "data grid preserved")
  expect_true(all(eg$proc_grid == pg),
    info = "proc grid preserved")

  for (col in colnames(eg$simulation)) {
    expect_identical(eg$simulation[[col]], expected_df[[col]],
      info = "one analyzing function. Results stored in simulation")
  }

}
grids_preserved()
##################################################################

simplify_the_simulation_results <- function() {

  rng <- function(data, ...) {
    ret <- range(data)
    names(ret) <- c("min", "max")
    ret
  }
  dg <- expand_tibble(fun = "seq_len", length.out = 1:3)
  pg <- expand_tibble(proc = "rng")
  eg <- eval_tibbles(dg, pg, rep = 2, envir = environment(), simplify = TRUE)
  expected_df <- structure(list(fun = c(
    "seq_len", "seq_len", "seq_len", "seq_len",
    "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len",
    "seq_len", "seq_len"
  ), length.out = c(
    1L, 1L, 1L, 1L, 2L, 2L,
    2L, 2L, 3L, 3L, 3L, 3L
  ), replications = c(
    1L, 1L, 2L, 2L, 1L,
    1L, 2L, 2L, 1L, 1L, 2L, 2L
  ), proc = c(
    "rng", "rng", "rng", "rng",
    "rng", "rng", "rng", "rng", "rng", "rng", "rng", "rng"
  ), results = c(
    1L,
    1L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 3L, 1L, 3L
  )), row.names = c(
    NA,
    -12L
  ), class = c("tbl_df", "tbl", "data.frame"), .Names = c(
    "fun",
    "length.out", "replications", "proc", "results"
  ))
  for (col in colnames(eg$simulation)) {
    expect_equivalent(eg$simulation[[col]], expected_df[[col]])
  }

}
simplify_the_simulation_results()


##################################################################

post_analyze_function_works <- function() {

  rng <- function(data, ...) {
    ret <- range(data)
    names(ret) <- c("min", "max")
    ret
  }
  dg <- expand_tibble(fun = "seq_len", length.out = 1:3)
  pg <- expand_tibble(proc = "rng")
  eg <- eval_tibbles(dg, pg,
    envir = environment(), simplify = TRUE,
    post_analyze = purrr::compose(tibble::as_tibble, t)
  )
  expected_df <- structure(list(
    fun = c("seq_len", "seq_len", "seq_len"), length.out = 1:3,
    replications = c(1L, 1L, 1L), proc = c("rng", "rng", "rng"),
    min = c(1L, 1L, 1L), max = 1:3
  ), row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"),
  .Names = c(
    "fun",
    "length.out", "replications", "proc", "min", "max"
  ))
  for (col in colnames(eg$simulation)) {
    expect_identical(eg$simulation[[col]], expected_df[[col]])
  }

}
post_analyze_function_works()

##################################################################

three_analyzing_functions <- function() {

  rng <- function(data, ...) {
    ret <- range(data)
    names(ret) <- c("min", "max")
    ret
  }
  dg <- expand_tibble(fun = "seq_len", length.out = 1:3)
  pg <- expand_tibble(proc = c("rng", "median", "length"))
  eg <- eval_tibbles(dg, pg, rep = 2, envir = environment(), simplify = FALSE)
  expected_df <- structure(list(fun = c(
    "seq_len", "seq_len", "seq_len", "seq_len",
    "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len",
    "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len",
    "seq_len", "seq_len"
  ), length.out = c(
    1L, 1L, 1L, 1L, 1L, 1L,
    2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L
  ), replications = c(
    1L,
    1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L,
    2L
  ), proc = c(
    "rng", "median", "length", "rng", "median", "length",
    "rng", "median", "length", "rng", "median", "length", "rng",
    "median", "length", "rng", "median", "length"
  ), results = list(
    structure(c(1L, 1L), .Names = c("min", "max")), 1L, 1L, structure(c(
      1L,
      1L
    ), .Names = c("min", "max")), 1L, 1L, structure(1:2, .Names = c(
      "min",
      "max"
    )), 1.5, 2L, structure(1:2, .Names = c("min", "max")),
    1.5, 2L, structure(c(1L, 3L), .Names = c("min", "max")),
    2L, 3L, structure(c(1L, 3L), .Names = c("min", "max")), 2L,
    3L
  )), .Names = c(
    "fun", "length.out", "replications", "proc",
    "results"
  ), row.names = c(NA, -18L), class = c(
    "tbl_df", "tbl",
    "data.frame"
  ))
  for (col in colnames(eg$simulation)) {
    expect_identical(eg$simulation[[col]], expected_df[[col]])
  }

}
three_analyzing_functions()

##################################################################

error_if_summary_function_is_not_a_list <- function() {

  dg <- expand_tibble(fun = "runif", n = 10)
  pg <- expand_tibble(proc = "length")
  expect_error(eval_tibbles(dg, pg, rep = 2, envir = environment(),
    summary_fun = c(mean), simplify = FALSE),
    "must be NULL or a named list")

}
error_if_summary_function_is_not_a_list()

error_if_summary_function_is_not_a_named_list <- function() {

  dg <- expand_tibble(fun = "runif", n = 10)
  pg <- expand_tibble(proc = "length")
  expect_error(eval_tibbles(dg, pg, rep = 2, envir = environment(),
    summary_fun = list(mean), simplify = FALSE),
    "must be NULL or a named list")

}
error_if_summary_function_is_not_a_named_list()



##################################################################

three_analyzing_functions_and_one_summary_function <- function() {

  rng <- function(data, ...) {
    ret <- range(data)
    names(ret) <- c("min", "max")
    ret
  }
  dg <- expand_tibble(fun = "seq_len", length.out = 1:3)
  pg <- expand_tibble(proc = c("rng", "median", "length"))
  eg <- eval_tibbles(dg, pg, rep = 2, envir = environment(),
    summary_fun = list(mean = mean),
    simplify = FALSE)
  expected_df <- structure(list(fun = c(
    "seq_len", "seq_len", "seq_len", "seq_len",
    "seq_len", "seq_len", "seq_len", "seq_len", "seq_len"
  ), length.out = c(
    1L,
    1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L
  ), replications = c(
    1L, 1L, 1L,
    1L, 1L, 1L, 1L, 1L, 1L
  ), summary_fun = c(
    "mean", "mean", "mean",
    "mean", "mean", "mean", "mean", "mean", "mean"
  ), proc = c(
    "rng",
    "median", "length", "rng", "median", "length", "rng", "median",
    "length"
  ), results = list(structure(list(min = 1, max = 1), .Names = c(
    "min",
    "max"
  ), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
  structure(list(value = 1), .Names = "value", row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame")), structure(list(
    value = 1
  ), .Names = "value", row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )), structure(list(min = 1, max = 2), .Names = c(
    "min",
    "max"
  ), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
  structure(list(value = 1.5), .Names = "value", row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame")), structure(list(
    value = 2
  ), .Names = "value", row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )), structure(list(min = 1, max = 3), .Names = c(
    "min",
    "max"
  ), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
  structure(list(value = 2), .Names = "value", row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame")), structure(list(
    value = 3
  ), .Names = "value", row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )))), .Names = c(
    "fun", "length.out", "replications",
    "summary_fun", "proc", "results"
  ), row.names = c(NA, -9L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  ))
  for (col in colnames(eg$simulation)) {
    expect_identical(eg$simulation[[col]], expected_df[[col]])
  }

}
three_analyzing_functions_and_one_summary_function()


##################################################################

three_analyzing_functions_and_three_summary_function <- function() {

  rng <- function(data, ...) {
    ret <- range(data)
    names(ret) <- c("min", "max")
    ret
  }
  dg <- expand_tibble(fun = "seq_len", length.out = 1:3)
  pg <- expand_tibble(proc = c("rng", "median", "length"))
  eg <- eval_tibbles(dg, pg, rep = 4, envir = environment(),
    summary_fun = list(mean = mean, sum = sum, prod = prod),
    simplify = FALSE)
  expected_df <- structure(list(fun = c(
    "seq_len", "seq_len", "seq_len", "seq_len",
    "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len",
    "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len",
    "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len",
    "seq_len", "seq_len", "seq_len", "seq_len", "seq_len"
  ), length.out = c(
    1L,
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
    2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L
  ), replications = c(
    1L,
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
  ), summary_fun = c(
    "mean",
    "mean", "mean", "prod", "prod", "prod", "sum", "sum", "sum",
    "mean", "mean", "mean", "prod", "prod", "prod", "sum", "sum",
    "sum", "mean", "mean", "mean", "prod", "prod", "prod", "sum",
    "sum", "sum"
  ), proc = c(
    "rng", "median", "length", "rng", "median",
    "length", "rng", "median", "length", "rng", "median", "length",
    "rng", "median", "length", "rng", "median", "length", "rng",
    "median", "length", "rng", "median", "length", "rng", "median",
    "length"
  ), results = list(structure(list(min = 1, max = 1), .Names = c(
    "min",
    "max"
  ), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
  structure(list(value = 1), .Names = "value", row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame")), structure(list(
    value = 1
  ), .Names = "value", row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )), structure(list(min = 1, max = 1), .Names = c(
    "min",
    "max"
  ), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
  structure(list(value = 1), .Names = "value", row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame")), structure(list(
    value = 1
  ), .Names = "value", row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )), structure(list(min = 4L, max = 4L), .Names = c(
    "min",
    "max"
  ), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
  structure(list(value = 4L), .Names = "value", row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame")), structure(list(
    value = 4L
  ), .Names = "value", row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )), structure(list(min = 1, max = 2), .Names = c(
    "min",
    "max"
  ), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
  structure(list(value = 1.5), .Names = "value", row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame")), structure(list(
    value = 2
  ), .Names = "value", row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )), structure(list(min = 1, max = 16), .Names = c(
    "min",
    "max"
  ), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
  structure(list(value = 5.0625), .Names = "value", row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame")), structure(list(
    value = 16
  ), .Names = "value", row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )), structure(list(min = 4L, max = 8L), .Names = c(
    "min",
    "max"
  ), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
  structure(list(value = 6), .Names = "value", row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame")), structure(list(
    value = 8L
  ), .Names = "value", row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )), structure(list(min = 1, max = 3), .Names = c(
    "min",
    "max"
  ), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
  structure(list(value = 2), .Names = "value", row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame")), structure(list(
    value = 3
  ), .Names = "value", row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )), structure(list(min = 1, max = 81), .Names = c(
    "min",
    "max"
  ), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
  structure(list(value = 16), .Names = "value", row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame")), structure(list(
    value = 81
  ), .Names = "value", row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )), structure(list(min = 4L, max = 12L), .Names = c(
    "min",
    "max"
  ), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
  structure(list(value = 8L), .Names = "value", row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame")), structure(list(
    value = 12L
  ), .Names = "value", row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )))), .Names = c(
    "fun", "length.out", "replications",
    "summary_fun", "proc", "results"
  ), row.names = c(NA, -27L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  ))
  for (col in colnames(eg$simulation)) {
    expect_identical(eg$simulation[[col]], expected_df[[col]])
  }

}
three_analyzing_functions_and_three_summary_function()



##################################################################

three_analyzing_functions_and_one_summary_function_over_2_cpus <- function() {

  rng <- function(data, ...) {
    ret <- range(data)
    names(ret) <- c("min", "max")
    ret
  }
  dg <- expand_tibble(fun = "seq_len", length.out = 1:3)
  pg <- expand_tibble(proc = c("rng", "median", "length"))
  eg <- eval_tibbles(dg, pg, rep = 20, envir = environment(),
    summary_fun = list(mean = mean), ncpus = 2,
    simplify = FALSE)
  expected_df <- structure(list(fun = c(
    "seq_len", "seq_len", "seq_len", "seq_len",
    "seq_len", "seq_len", "seq_len", "seq_len", "seq_len"
  ), length.out = c(
    1L,
    1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L
  ), replications = c(
    1L, 1L, 1L,
    1L, 1L, 1L, 1L, 1L, 1L
  ), summary_fun = c(
    "mean", "mean", "mean",
    "mean", "mean", "mean", "mean", "mean", "mean"
  ), proc = c(
    "rng",
    "median", "length", "rng", "median", "length", "rng", "median",
    "length"
  ), results = list(structure(list(min = 1, max = 1), .Names = c(
    "min",
    "max"
  ), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
  structure(list(value = 1), .Names = "value", row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame")), structure(list(
    value = 1
  ), .Names = "value", row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )), structure(list(min = 1, max = 2), .Names = c(
    "min",
    "max"
  ), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
  structure(list(value = 1.5), .Names = "value", row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame")), structure(list(
    value = 2
  ), .Names = "value", row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )), structure(list(min = 1, max = 3), .Names = c(
    "min",
    "max"
  ), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
  structure(list(value = 2), .Names = "value", row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame")), structure(list(
    value = 3
  ), .Names = "value", row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )))), .Names = c(
    "fun", "length.out", "replications",
    "summary_fun", "proc", "results"
  ), row.names = c(NA, -9L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  ))
  for (col in colnames(eg$simulation)) {
    expect_identical(eg$simulation[[col]], expected_df[[col]])
  }

}
three_analyzing_functions_and_one_summary_function_over_2_cpus()

##################################################################

one_group_for_summary_fun <- function() {

  shift <- -1
  gen_data <- function() {
    shift <<- shift + 1
    tibble::tibble(group = letters[1:3], b = 1:3 + shift)
  }
  dg <- expand_tibble(fun = c("gen_data"))
  pg <- expand_tibble(proc = "identity")
  eg <- eval_tibbles(dg, pg,
    rep = 3, envir = environment(), summary_fun = list(mean = mean, sum = sum),
    group_for_summary = "group", simplify = FALSE
  )
  expected_df <- structure(
    list(
      fun = c("gen_data", "gen_data"),
      replications = c(1L, 1L),
      summary_fun = c("mean", "sum"),
      proc = c("identity", "identity"),
      results = structure(
        list(
          mean = structure(list(group = c("a", "b", "c"), b = c(2, 3, 4)),
            class = c("tbl_df", "tbl", "data.frame"),
            .Names = c("group", "b"), row.names = c(NA, -3L)
          ),
          sum = structure(list(group = c("a", "b", "c"), b = c(6, 9, 12)),
            class = c("tbl_df", "tbl", "data.frame"), .Names = c("group", "b"),
            row.names = c(NA, -3L)
          )
        ),
        .Names = c("mean", "sum")
      )
    ),
    .Names = c("fun", "replications", "summary_fun", "proc", "results"),
    row.names = c(NA, -2L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  for (col in colnames(eg$simulation)) {
    expect_identical(eg$simulation[[col]], expected_df[[col]])
  }

}
one_group_for_summary_fun()

##################################################################

two_groups_for_summary_fun <- function() {

  shift <- -1
  gen_data <- function() {
    shift <<- shift + 1
    tibble::tibble(group1 = letters[1:3], group2 = letters[4:6], b = 1:3 + shift)
  }
  dg <- expand_tibble(fun = c("gen_data"))
  pg <- expand_tibble(proc = "identity")
  eg <- eval_tibbles(dg, pg,
    rep = 3, envir = environment(), summary_fun = list(mean = mean, sum = sum),
    group_for_summary = c("group1", "group2"), simplify = FALSE
  )
  expected_df <-
    tibble::tibble(
      fun = "gen_data",
      replications = 1L,
      summary_fun = c("mean", "sum"),
      proc = "identity",
      results = list(
        mean = dplyr::group_by(
          tibble::tibble(group1 = letters[1:3],
            group2 = letters[4:6],
            b = c(2.0, 3.0, 4.0)),
          group1, group2),
        sum = dplyr::group_by(
          tibble::tibble(group1 = letters[1:3],
            group2 = letters[4:6],
            b = c(6.0, 9.0, 12.0)),
          group1, group2)
      )
    )
  for (col in colnames(eg$simulation)) {
    expect_equivalent(eg$simulation[[col]], expected_df[[col]])
  }

}
two_groups_for_summary_fun()


##################################################################

variables_uploaded_to_cluster <- function() {

  ret_global_var <- function(dummy) {
    paste(get("globalVar", envir = globalenv()),
      "executed on cluster",
      sep = ", ")
  }
  dg <- expand_tibble(fun = "seq_len", length.out = 1:3)
  pg <- expand_tibble(proc = "ret_global_var")
  assign("globalVar", "uploaded to cluster", envir = .GlobalEnv)
  eg <- eval_tibbles(dg, pg, rep = 10, envir = environment(),
    ncpus = 2, cluster_global_objects = c("globalVar"),
    simplify = FALSE)
  expect_identical(unique(unlist(eg$simulation$results)),
    "uploaded to cluster, executed on cluster")

  err <- try(eval_tibbles(dg, pg, rep = 10, envir = environment(),
    ncpus = 2, simplify = FALSE), silent = TRUE)
  expect_equal(class(err), "try-error")
  expect_true(grepl("'globalVar' not found", err))

}
variables_uploaded_to_cluster()

##################################################################

library_boot_loaded_on_the_cluster. <- function() {

  pg <- expand_tibble(proc = c("mean"))
  fetch_other_pkgs <- function(dummy) {
    names(sessionInfo()[["otherPkgs"]])
  }
  cl <- parallel::makeCluster(rep("localhost", 2), type = "PSOCK")
  dg <- expand_tibble(fun = "seq_len", length.out = 1:3)
  pg <- expand_tibble(proc = "fetch_other_pkgs")
  eg <- eval_tibbles(dg, pg, rep = 2, envir = environment(),
    cluster = cl, simplify = FALSE)
  expect_true(is.null(unique(unlist(eg$simulation$results))),
    info = "no libs loaded on cluster")

  eg <- eval_tibbles(dg, pg,
    rep = 2, envir = environment(),
    cluster = cl,
    cluster_libraries = c("boot"), simplify = FALSE
  )
  parallel::stopCluster(cl)
  expect_equal(unique(unlist(eg$simulation$results)), "boot")

}
library_boot_loaded_on_the_cluster.()

##################################################################

warning_if_cluster_and_ncpus_are_specified_and_that_the_cluster_cl_is_not_stopped <- function() {

  fetch_other_pkgs <- function(dummy) {
    names(sessionInfo()[["otherPkgs"]])
  }
  cl <- parallel::makeCluster(rep("localhost", 2), type = "PSOCK")
  dg <- expand_tibble(fun = "seq_len", length.out = 1:3)
  pg <- expand_tibble(proc = "fetch_other_pkgs")
  expect_warning(
    eval_tibbles(dg, pg,
      rep = 2, envir = environment(),
      ncpus = 2,
      cluster = cl,
      cluster_libraries = c("boot"), simplify = FALSE
    ),
    "Ignore argument ncpus"
  )

  # just repeat the call. If the cluster would have been stopped
  # an error would occur
  expect_warning(
    eval_tibbles(dg, pg,
      rep = 2, envir = environment(),
      ncpus = 2,
      cluster = cl,
      cluster_libraries = c("boot"), simplify = FALSE
    ),
    "Ignore argument ncpus"
  )
  parallel::stopCluster(cl)

}
warning_if_cluster_and_ncpus_are_specified_and_that_the_cluster_cl_is_not_stopped()




##################################################################

results_are_mapped_correctly_to_data_generator_analyzer_constellations <- function() {

  gen_data1 <- function(p) {
    2 * p
  }
  gen_data2 <- function(p) {
    3 * p
  }
  gen_data3 <- function(p) {
    5 * p
  }
  dg <- expand_tibble(
    fun = c("gen_data1", "gen_data2", "gen_data3"),
    p = c(7, 11, 13)
  )
  ana1 <- function(data, m) {
    data * m * 29
  }
  ana2 <- function(data, m) {
    data * m * 31
  }
  ana3 <- function(data, m) {
    data * m * 37
  }
  pg <- expand_tibble(
    proc = c("ana1", "ana2", "ana3"),
    m = c(17, 19, 23)
  )
  eg <- eval_tibbles(dg, pg, rep = 2, envir = environment(), simplify = TRUE)
  result <- 1:162
  cnt <- 0
  for (p in c(7, 11, 13)) {
    for (g in c(2, 3, 5)) {
      for (rep in 1:2) {
        for (m in c(17, 19, 23)) {
          for (a in c(29, 31, 37)) {
            cnt <- cnt + 1
            result[cnt] <- g * p * m * a
          }
        }
      }
    }
  }
  expect_equal(eg$simulation$results, result)

}
results_are_mapped_correctly_to_data_generator_analyzer_constellations()


##################################################

data_is_generated_once_and_used_for_all_analyzing_functions <- function() {
  dg <- expand_tibble(
    fun = c("runif"),
    n = 10
  )


  f1 <- function(data) {
    data
  }
  f2 <- function(data) {
    data
  }

  pg <- expand_tibble(proc = c("f1", "f2"))
  eg1 <- eval_tibbles(dg, pg, rep = 1, envir = environment(), simplify = FALSE)
  eg2 <- eval_tibbles(dg, pg, rep = 1, envir = environment(), simplify = FALSE,
    discard_generated_data = TRUE)

  expect_true(
    all(
      eg1$simulation$results[[1]] == eg1$simulation$results[[2]]
    )
  )
  expect_true(
    all(
      eg2$simulation$results[[1]] == eg2$simulation$results[[2]]
    )
  )
}
data_is_generated_once_and_used_for_all_analyzing_functions()
