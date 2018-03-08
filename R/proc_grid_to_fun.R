proc_grid_to_fun = function(procGrid, envir)
{
  lapply(1:nrow(procGrid), function(i){
    fp = extract_fun_and_parameter(procGrid[i,,drop = FALSE], envir)
    function(x) {fp$para[[length(fp$para) + 1]] = x; do.call(fp$fun, fp$para)}
  })
}
