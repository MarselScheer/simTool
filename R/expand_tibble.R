#' Creates a \code{tibble} from All Combinations
#'
#'  Actually a wrapper for \code{\link{expand.grid}}, but
#'  character vectors will stay as characters.
#'
#'
#' @param \dots  vectors, factors or a list containing these.
#' @return See \code{\link{expand.grid}} but instead of a \code{\link{data.frame}}
#'  a \code{\link{tibble}} is returned.
#' @author  Marsel Scheer
#' @seealso  \code{\link{expand.grid}}
#' @examples
#'
#' expand_tibble(fun="rnorm", mean=1:4, sd=2:5)
#'
#' @importFrom tibble as_tibble
#' @export
expand_tibble <-
  function(...) {
    tibble::as_tibble(expand.grid(..., stringsAsFactors = FALSE))
  }
