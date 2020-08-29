#' @export
Data_generator <- R6::R6Class("Data_generator",
  public = list(
    initialize = function(generator_fun) {
      private$generator_fun = generator_fun
    },
    get_generator = function() {
      return(private$generator_fun)
    },
    generator = function() {
      f <- self$get_generator()
      return(f())
    }
  ),
  private = list(
    generator_fun = NULL
  )
)
