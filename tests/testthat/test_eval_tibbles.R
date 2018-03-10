rng = function(data, ...) {
  ret = range(data)
  names(ret) = c("min", "max")
  ret
}

genData1 = function(df)
{
  df[[1]][,1]
}

genData2 = function(df)
{
  df[[1]][,2]
}

dg <- expand_tibble(
  fun = c("genData1", "genData2"),
  df = list(matrix(1:6, 3,2),
            matrix(1:8, 4,2)))
pg <- expandGrid(proc = "rng")
eg <- eval_tibbles(dg, pg, rep = 2, envir = environment())

expected_df = structure(list(
  fun = c("genData1", "genData1", "genData2", "genData2", "genData1", "genData1", "genData2", "genData2"), 
  df = list(structure(1:6, .Dim = c(3L, 2L)), 
            structure(1:6, .Dim = c(3L, 2L)), structure(1:6, .Dim = c(3L, 2L)), 
            structure(1:6, .Dim = c(3L, 2L)), structure(1:8, .Dim = c(4L, 2L)),
            structure(1:8, .Dim = c(4L, 2L)), structure(1:8, .Dim = c(4L, 2L)),
            structure(1:8, .Dim = c(4L, 2L))),
  replications = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L), proc = c("rng", "rng", "rng", "rng", "rng", "rng", "rng", "rng"), 
  results = list(structure(c(1L, 3L), .Names = c("min", "max")), 
                 structure(c(1L, 3L), .Names = c("min", "max")), 
                 structure(c(4L, 6L), .Names = c("min", "max")), 
                 structure(c(4L, 6L), .Names = c("min", "max")), 
                 structure(c(1L, 4L), .Names = c("min", "max")),
                 structure(c(1L, 4L), .Names = c("min", "max")), 
                 structure(c(5L, 8L), .Names = c("min", "max")),
                 structure(c(5L, 8L), .Names = c("min", "max")))), 
  .Names = c("fun", "df", "replications", "proc", "results"), 
  row.names = c(NA, -8L), 
  class = c("tbl_df", "tbl", "data.frame"))


test_that("Tibbles for data generating functions can be used. Results were created and stored in simulation",{
  expect_identical(eg$simulation, expected_df)
})

##################################################################



genMat = function(df)
{
  df[[1]][,1]
}

mat_mult = function(A, B)
{
  A %*% B[[1]]
}

dg <- expand_tibble(
  fun = c("genMat"),
  df = list(matrix(1:4, 2,2),
            matrix(5:8, 2,2)))

pg <- expand_tibble(
  proc = "mat_mult", 
  B = list(matrix(1:4, 2,2), matrix(5:8, 2,2)))

eg <- eval_tibbles(dg, pg, rep = 1, envir = environment())

expected_df <- structure(
  list(fun = c("genMat", "genMat", "genMat", "genMat"), 
       df = list(structure(1:4, .Dim = c(2L, 2L)), structure(1:4, .Dim = c(2L, 2L)), 
                 structure(5:8, .Dim = c(2L, 2L)), structure(5:8, .Dim = c(2L, 2L))), 
       replications = c(1L, 1L, 1L, 1L), proc = c("mat_mult", "mat_mult", "mat_mult", "mat_mult"),
       B = list(structure(1:4, .Dim = c(2L, 2L)), 
                structure(5:8, .Dim = c(2L, 2L)), structure(1:4, .Dim = c(2L, 2L)), 
                structure(5:8, .Dim = c(2L, 2L))), 
       results = list(structure(c(5, 11), .Dim = 1:2), structure(c(17, 23), .Dim = 1:2),
                      structure(c(17, 39), .Dim = 1:2), structure(c(61, 83), .Dim = 1:2))), 
  .Names = c("fun", "df", "replications", "proc", "B", "results"), 
  row.names = c(NA, -4L), 
  class = c("tbl_df", "tbl", "data.frame"))


test_that("Tibbles for data generating and data analyzing functions can be used. Results were created and stored in simulation",{
  expect_identical(eg$simulation, expected_df)
})

##################################################################

dg <- expandGrid(fun = "seq_len", length.out = 1:3)
pg <- expandGrid(proc = "rng")
eg <- eval_tibbles(dg, pg, rep = 2, envir = environment())

expected_df = structure(list(fun = c("seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len"), 
                             length.out = c(1L, 1L, 2L, 2L, 3L, 3L), 
               replications = c(1L, 2L, 1L, 2L, 1L, 2L), 
               proc = c("rng", "rng", "rng", "rng", "rng", "rng"), 
               results = list(structure(c(1L, 1L), .Names = c("min", "max")), 
                              structure(c(1L, 1L), .Names = c("min", "max")), structure(1:2, .Names = c("min", "max")), 
                              structure(1:2, .Names = c("min", "max")), structure(c(1L, 3L), .Names = c("min", "max")), 
                              structure(c(1L, 3L), .Names = c("min", "max")))), .Names = c("fun", "length.out", "replications", "proc", "results"), 
               out.attrs = structure(list(dim = structure(c(1L, 3L), .Names = c("fun", "length.out")),
                                          dimnames = structure(list(fun = "fun=seq_len", length.out = c("length.out=1","length.out=2", "length.out=3")), .Names = c("fun", "length.out"))), 
                                     .Names = c("dim", "dimnames")), row.names = c(NA, -6L), 
               class = c("tbl_df", "tbl", "data.frame"))


test_that("Data grid that was used is preserved in the object returned by evalGrids",
          expect_true(all(eg$data_grid == dg)))

test_that("Proc grid that was used is preserved in the object returned by evalGrids",
          expect_true(all(eg$proc_grid == pg)))

test_that("One analyzing function. Results were created and stored in simulation",{
  expect_identical(eg$simulation, expected_df)
})
##################################################################

dg <- expandGrid(fun = "seq_len", length.out = 1:3)
pg <- expandGrid(proc = c("rng", "median", "length"))
eg <- eval_tibbles(dg, pg,rep = 2, envir = environment())

expected_df = structure(
  list(fun = c("seq_len", "seq_len", "seq_len", "seq_len",
               "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len",
               "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len",
               "seq_len", "seq_len"),
       length.out = c(1L, 1L, 1L, 1L, 1L, 1L,
                      2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L),
       replications = c(1L,
                        1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L,
                        2L),
       proc = c("rng", "median", "length", "rng", "median", "length",

                "rng", "median", "length", "rng", "median", "length", "rng",
                "median", "length", "rng", "median", "length"),
       results = list(
         structure(c(1L, 1L), .Names = c("min", "max")), 1L, 1L,
         structure(c(1L, 1L), .Names = c("min", "max")), 1L, 1L,
         structure(1:2, .Names = c("min", "max")), 1.5, 2L,
         structure(1:2, .Names = c("min", "max")), 1.5, 2L,
         structure(c(1L, 3L), .Names = c("min", "max")), 2L, 3L,
         structure(c(1L, 3L), .Names = c("min", "max")), 2L, 3L)),
  .Names = c("fun", "length.out", "replications", "proc", "results"),
  out.attrs = structure(
    list(dim = structure(c(1L, 3L), .Names = c("fun", "length.out")),
         dimnames = structure(list(fun = "fun=seq_len",
                                   length.out = c("length.out=1", "length.out=2", "length.out=3")),
                              .Names = c("fun", "length.out"))), .Names = c("dim", "dimnames")),
  row.names = c(NA, -18L),
  class = c("tbl_df", "tbl", "data.frame"))

test_that("Three analyzing functions. Results were created and stored in simulation",{
  expect_identical(eg$simulation, expected_df)
})

##################################################################

test_that("Error if summary function is not a list",{
  expect_error(eval_tibbles(dg, pg,rep = 2, envir = environment(), summary_fun = c(mean)), "must be NULL or a named list")
})

test_that("Error if summary function is not a named list",{
  expect_error(eval_tibbles(dg, pg,rep = 2, envir = environment(), summary_fun = list(mean)), "must be NULL or a named list")
})


##################################################################

eg <- eval_tibbles(dg, pg,rep = 2, envir = environment(), summary_fun = list(mean = mean))

expected_df <- structure(
  list(fun = c("seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len"), 
       length.out = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L), replications = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
       summary_fun = rep("mean", 9),
       proc = c("rng", "median", "length", "rng", "median", "length", "rng", "median", "length"), 
       results = list(
         structure(list(min = 1, max = 1), .Names = c("min", "max"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(value = 1), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(value = 1), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(min = 1, max = 2), .Names = c("min", "max"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(value = 1.5), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(value = 2), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(min = 1, max = 3), .Names = c("min", "max"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(value = 2), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(value = 3), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")))), 
  .Names = c("fun", "length.out", "replications", "summary_fun", "proc", "results"), 
  out.attrs = structure(
    list(dim = structure(c(1L, 3L), .Names = c("fun", "length.out")), 
         dimnames = structure(list(fun = "fun=seq_len",length.out = c("length.out=1", "length.out=2", "length.out=3")), .Names = c("fun", "length.out"))), 
    .Names = c("dim", "dimnames")), row.names = c(NA, -9L), class = c("tbl_df", "tbl", "data.frame"))

test_that("Three analyzing functions and one summary function. Results were created and stored in simulation",{
  expect_identical(eg$simulation, expected_df)
})


##################################################################

eg <- eval_tibbles(dg, pg,rep = 4, envir = environment(), summary_fun = list(mean = mean, sum = sum, prod = prod))

expected_df <- structure(
  list(fun = c("seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", 
               "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", 
               "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len"), 
       length.out = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L), 
       replications = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
       summary_fun = c("mean", "mean", "mean", "prod", "prod", "prod", "sum", "sum", "sum", "mean", "mean", "mean", 
                       "prod", "prod", "prod", "sum", "sum", "sum", "mean", "mean", "mean", "prod", "prod", "prod", 
                       "sum", "sum", "sum"), 
       proc = c("rng", "median", "length", "rng", "median", "length", "rng", "median", "length", "rng", "median", "length", 
                "rng", "median", "length", "rng", "median", "length", "rng", "median", "length", "rng", "median", "length", 
                "rng", "median", "length"), 
       results = list(
         structure(list(min = 1, max = 1), .Names = c("min", "max"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(value = 1), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(value = 1), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(min = 1, max = 1), .Names = c("min", "max"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(value = 1), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(value = 1), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(min = 4L, max = 4L), .Names = c("min", "max"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(value = 4L), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(value = 4L), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(min = 1, max = 2), .Names = c("min", "max"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(value = 1.5), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(value = 2), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(min = 1, max = 16), .Names = c("min", "max"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(value = 5.0625), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(value = 16), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(min = 4L, max = 8L), .Names = c("min", "max"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(value = 6), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(value = 8L), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(min = 1, max = 3), .Names = c("min", "max"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(value = 2), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(value = 3), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(min = 1, max = 81), .Names = c("min", "max"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(value = 16), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(value = 81), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(min = 4L, max = 12L), .Names = c("min", "max"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(value = 8L), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")), 
         structure(list(value = 12L), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")))),
  .Names = c("fun", "length.out", "replications", "summary_fun", "proc", "results"), 
  out.attrs = structure(list(dim = structure(c(1L, 3L), .Names = c("fun", "length.out")), 
                             dimnames = structure(list(fun = "fun=seq_len", length.out = c("length.out=1", "length.out=2", "length.out=3")),
                                                  .Names = c("fun", "length.out"))), .Names = c("dim", "dimnames")), row.names = c(NA, -27L), 
  class = c("tbl_df", "tbl", "data.frame"))

test_that("Three analyzing functions and three summary function. Results were created and stored in simulation",{
  expect_identical(eg$simulation, expected_df)
})



##################################################################

eg <- eval_tibbles(dg, pg,rep = 20, envir = environment(), summary_fun = list(mean = mean), ncpus = 2)

expected_df <- structure(
  list(fun = c("seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len", "seq_len"),
       length.out = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L), replications = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
       summary_fun = rep("mean", 9),
       proc = c("rng", "median", "length", "rng", "median", "length", "rng", "median", "length"),
       results = list(
         structure(list(min = 1, max = 1), .Names = c("min", "max"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(value = 1), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(value = 1), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(min = 1, max = 2), .Names = c("min", "max"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(value = 1.5), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(value = 2), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(min = 1, max = 3), .Names = c("min", "max"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(value = 2), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")),
         structure(list(value = 3), .Names = "value", row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")))),
  .Names = c("fun", "length.out", "replications", "summary_fun", "proc", "results"),
  out.attrs = structure(
    list(dim = structure(c(1L, 3L), .Names = c("fun", "length.out")),
         dimnames = structure(list(fun = "fun=seq_len",length.out = c("length.out=1", "length.out=2", "length.out=3")), .Names = c("fun", "length.out"))),
    .Names = c("dim", "dimnames")), row.names = c(NA, -9L), class = c("tbl_df", "tbl", "data.frame"))

test_that("Three analyzing functions and one summary function over 2 cpus. Results were created and stored in simulation",{
  expect_identical(eg$simulation, expected_df)
})

##################################################################

shift = -1
gen_data <- function()
{
  shift <<- shift + 1
  tibble(group = letters[1:3], b = 1:3 + shift)
}

dg <- expand_tibble(fun = c("gen_data"))
pg <- expandGrid(proc = "identity")
eg <- eval_tibbles(dg, pg, rep = 3, envir = environment(), summary_fun = list(mean = mean, sum = sum), 
                   group_for_summary = "group")

expected_df <- structure(
  list(fun = c("gen_data", "gen_data"), replications = c(1L, 1L), summary_fun = c("mean", "sum"), 
       proc = c("identity", "identity"), 
       results = structure(
         list(mean = structure(list(group = c("a", "b", "c"), b = c(2, 3, 4)), 
                               class = c("tbl_df", "tbl", "data.frame"), 
                               .Names = c("group", "b"), row.names = c(NA, -3L)), 
              sum = structure(list(group = c("a", "b", "c"), b = c(6, 9, 12)), 
                              class = c("tbl_df", "tbl", "data.frame"), .Names = c("group", "b"), 
                              row.names = c(NA, -3L))), 
         .Names = c("mean", "sum"))), .Names = c("fun", "replications", "summary_fun", "proc", "results"), 
  row.names = c(NA, -2L), 
  class = c("tbl_df", "tbl", "data.frame"))

test_that("Grouping with one group for summary_fun. Results were created and stored in simulation",{
  expect_identical(eg$simulation, expected_df)
})

##################################################################

ret_global_var = function(dummy)
{
  paste(get("globalVar", envir = globalenv()), "executed on cluster", sep = ", ")
}
dg <- expandGrid(fun = "seq_len", length.out = 1:3)
pg <- expandGrid(proc = "ret_global_var")
assign("globalVar", "uploaded to cluster", envir = .GlobalEnv)
eg <- eval_tibbles(dg, pg, rep = 10, envir = environment(), ncpus = 2, cluster_global_objects = c("globalVar"))

test_that("Variable gets uploaded to the cluster.",{
  expect_identical(unique(unlist(eg$simulation$results)), "uploaded to cluster, executed on cluster")
})

test_that("Error is variable is not uploaded to cluster",{
  err = try(eval_tibbles(dg, pg, rep = 10, envir = environment(), ncpus = 2), silent = TRUE)
  expect_equal(class(err), "try-error")
  expect_true(grepl("'globalVar' not found", err))
})

##################################################################
fetch_other_pkgs = function(dummy)
{
  names(sessionInfo()$otherPkgs)
}
dg <- expandGrid(fun = "seq_len", length.out = 1:3)
pg <- expandGrid(proc = "fetch_other_pkgs")
eg <- eval_tibbles(dg, pg, rep = 2, envir = environment(), ncpus = 2)

test_that("No libraries loaded on the cluster.",{
  expect_true(is.null(unique(unlist(eg$simulation$results))))
})

eg <- eval_tibbles(dg, pg, rep = 2, envir = environment(), ncpus = 2, cluster_libraries = c("survival"))
test_that("No libraries loaded on the cluster.",{
  expect_equal(unique(unlist(eg$simulation$results)), "survival")
})

