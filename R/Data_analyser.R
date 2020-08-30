#' @export
Data_analyser <- R6::R6Class("Data_analyser",
  public = list(
    initialize = function(analyser_fun, post_fun = NULL) {
      if (is.null(post_fun)) {
        private$analyser_fun = analyser_fun
      } else {
        private$analyser_fun = function(x) post_fun(analyser_fun(x))
      }
    },
    get_analyser = function() {
      return(private$analyser_fun)
    },
    analyser = function(analyser_input) {
      f <- self$get_analyser()
      return(f(analyser_input))
    }
  ),
  private = list(
    analyser_fun = NULL
  )
)
