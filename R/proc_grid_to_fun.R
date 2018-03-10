proc_grid_to_fun <- function(proc_grid, envir) {
  lapply(1:nrow(proc_grid), function(i) {
    fp <- extract_fun_and_parameter(proc_grid[i, , drop = FALSE], envir)
    function(x) {
      fp$para[[length(fp$para) + 1]] <- x
      do.call(fp$fun, fp$para)
    }
  })
}
