#' @importFrom methods formalArgs
proc_grid_to_fun <- function(proc_grid, envir) {
  lapply(seq_len(nrow(proc_grid)), function(i) {
    fp <- extract_fun_and_parameter(proc_grid[i, , drop = FALSE], envir)
    # extending the parameter list should be outside, otherwise
    # the parameter list will be extended every time the data analyzing
    # function is called, i.e. for every replication!
    fp$para[[length(fp$para) + 1]] <- ""

    if (is.element(".truth", methods::formalArgs(fp$fun))) {
      # extending parameter list outside the function call, see above
      fp$para$.truth <- ""
      return(function(x, .truth) {
        fp$para$.truth <- .truth
        fp$para[[length(fp$para) - 1]] <- x
        do.call(fp$fun, fp$para)
      })
    }
    return(function(x, .truth) {
      # fp$fun has no argument for .groundTruth, but function signature must
      # be the same
      fp$para[[length(fp$para)]] <- x
      do.call(fp$fun, fp$para)
    })
  })
}
