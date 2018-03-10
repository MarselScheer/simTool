data_grid_to_fun <- function(data_grid, envir) {
  lapply(1:nrow(data_grid), function(i) {
    fp <- extract_fun_and_parameter(data_grid[i, , drop = FALSE], envir)
    function() do.call(fp$fun, fp$para)
  })
}
