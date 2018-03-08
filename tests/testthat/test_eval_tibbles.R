# genData1 = function(df)
# {
#   df[[1]][,1]
# }
# 
# genData2 = function(df)
# {
#   df[[1]][,2]
# }
# 
# dg <- expand_tibble(
#   fun = c("genData1", "genData2"),
#   df = list(data.frame(1:3, 4:6),
#              data.frame(1:4, 5:8)))
# pg <- expandGrid(proc = "rng")
# eg <- eval_tibbles(dg, pg, rep = 2, envir = environment())


##################################################################

rng = function(data, ...) {
  ret = range(data)
  names(ret) = c("min", "max")
  ret
}

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
          expect_true(all(eg$dataGrid == dg)))

test_that("Proc grid that was used is preserved in the object returned by evalGrids",
          expect_true(all(eg$procGrid == pg)))

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

ret_global_var = function(dummy)
{
  paste(get("globalVar", envir = globalenv()), "executed on cluster", sep = ", ")
}
dg <- expandGrid(fun = "seq_len", length.out = 1:3)
pg <- expandGrid(proc = "ret_global_var")
assign("globalVar", "uploaded to cluster", envir = .GlobalEnv)
eg <- eval_tibbles(dg, pg, rep = 10, envir = environment(), ncpus = 2, clusterGlobalObjects = c("globalVar"))

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

eg <- eval_tibbles(dg, pg, rep = 2, envir = environment(), ncpus = 2, clusterLibraries = c("survival"))
test_that("No libraries loaded on the cluster.",{
  expect_equal(unique(unlist(eg$simulation$results)), "survival")
})

