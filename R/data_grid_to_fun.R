data_grid_to_fun = function(dataGrid, envir)
{
  lapply(1:nrow(dataGrid), function(i)
  {
    fp = extract_fun_and_parameter(dataGrid[i,,drop = FALSE], envir)
    function() do.call(fp$fun, fp$para)
  })
}
