#' R6 Class used to define how to analyse a data set
#' @export
Data_analyser <- R6::R6Class("Data_analyser",
  public = list(
    ##' @description
    ##' Creates a new Data_analyser object
    ##' @param analyser_fun function that analyse a data set
    ##' @param post_fun function post-processes the output of
    ##' \code{analyser_fun}
    ##' @param short_name string naming the data analyser. If
    ##' not provided a name is generated that consists of the
    ##' argument passed to \code{analyser_fun} and an integer
    ##' which are the seconds passed since 1970.
    ##' @return no explicit return object
    initialize = function(analyser_fun, post_fun = NULL, short_name = NULL) {
      if (is.null(post_fun)) {
        private$analyser_fun = analyser_fun
      } else {
        private$analyser_fun = function(x) post_fun(analyser_fun(x))
      }

      if (is.null(short_name)) {
        short_name <- paste0(
          deparse(substitute(analyser_fun)),
          "_",
          format(x = Sys.time(), format = "%s")
          )
      }
      private$short_name <- short_name
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
    },
    ##' @description
    ##' getter for the short_name
    ##' @return short_name for the data analyser
    get_short_name = function() {
      return(private$short_name)
    }
  ),
  private = list(
    analyser_fun = NULL,
    short_name = NULL
  )
)
