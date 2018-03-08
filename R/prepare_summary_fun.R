prepare_summary_fun = function(summary_fun)
{
  if (!is.null(summary_fun))
  {      
    if (!is.list(summary_fun) || is.null(names(summary_fun)))
    {
      stop("summary_fun must be NULL or a named list, like list(mean = mean, sd = sd)")
    }
    order_idx = order(names(summary_fun))
    summary_fun = summary_fun[order_idx]
  }
}