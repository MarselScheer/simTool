#' R6 Class used to define how to analyse a data set
#' @export
Data_analyser <- R6::R6Class("Data_analyser",
  public = list(
    ##' @description
    ##' Creates a new Data_analyser object
    ##' @param analyser_fun function that analyse a data set
    ##' @param post_fun function post-processes the output of
    ##' \code{analyser_fun}
    ##' @return no explicit return object
    initialize = function(analyser_fun, post_fun = NULL) {
      if (is.null(post_fun)) {
        private$analyser_fun = analyser_fun
      } else {
        private$analyser_fun = function(x) post_fun(analyser_fun(x))
      }
    },
    ##' @description
    ##' getter for the analysing and post-processing function
    ##' @return function that analyses the data set already combined
    ##' with the post-processing function if it was defined
    get_analyser = function() {
      return(private$analyser_fun)
    },
    ##' @description
    ##' calls the analysing function and if defined also the post-processing
    ##' @param analyser_input data set to be analysed
    ##' @return the analysed data set after post-processing
    analyser = function(analyser_input) {
      f <- self$get_analyser()
      return(f(analyser_input))
    }
  ),
  private = list(
    analyser_fun = NULL
  )
)
