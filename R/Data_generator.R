#' R6 Class used to define how data is generated
#' @export
Data_generator <- R6::R6Class("Data_generator",
  public = list(
    ##' @description
    ##' Creates a new Data_generator
    ##' @param generator_fun function that generates the data
    ##' @param short_name string naming the data generator. If
    ##' not provided a name is generated that consists of the
    ##' argument passed to \code{generator_fun} and an integer
    ##' which are the seconds passed since 1970.
    ##' @return no explicit return-value
    initialize = function(generator_fun, short_name = NULL) {
      private$generator_fun = generator_fun
      if (is.null(short_name)) {
        short_name <- paste0(
          deparse(substitute(generator_fun)),
          "_",
          format(x = Sys.time(), format = "%s")
          )
      }
      private$short_name <- short_name
    },
    ##' @description
    ##' getter for the data generating function
    ##' @return function that generates the data
    get_generator = function() {
      return(private$generator_fun)
    },
    ##' @description
    ##' calls the data generating function
    ##' @return a generated data set
    generator = function() {
      f <- self$get_generator()
      return(f())
    },
    ##' @description
    ##' getter for the short_name
    ##' @return short_name for the data generator
    get_short_name = function() {
      return(private$short_name)
    }
  ),
  private = list(
    generator_fun = NULL,
    short_name = NULL
  )
)
