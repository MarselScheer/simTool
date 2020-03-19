data_grid_to_fun <- function(data_grid, envir) {
  lapply(seq_len(nrow(data_grid)), function(i) {
    fp <- extract_fun_and_parameter(data_grid[i, , drop = FALSE], envir)
    ret <- function() do.call(fp$fun, fp$para)
    attr(ret, ".truth") <- attr(fp$fun, ".truth")
    ret
  })
}
