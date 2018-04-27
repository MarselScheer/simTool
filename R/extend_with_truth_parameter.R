#' @importFrom methods formalArgs
extend_with_truth_parameter <- function(f) {
  if (is.element(".truth", methods::formalArgs(f))) {
    return(f)
  }
  function(x, .truth) f(x)
}
