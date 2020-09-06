#' R6 Class used to define how data is generated
#' @export
Data_generator <- R6::R6Class("Data_generator",
  public = list(
    ##' @description
    ##' Creates a new Data_generator
    ##' @param generator_fun function that generates the data
    ##' @return no explicit return-value
    initialize = function(generator_fun) {
      private$generator_fun = generator_fun
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
    }
  ),
  private = list(
    generator_fun = NULL
  )
)
