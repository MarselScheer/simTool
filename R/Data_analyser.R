#' @export
Data_analyser <- R6::R6Class("Data_analyser",
  public = list(
    initialize = function(analyser_fun, post_fun = NULL) {
      private$analyser_fun = analyser_fun

      # NOTE: deliberately do not provide a function that
      #       calls the post_fun because we would have to
      #       check if post_fun is NULL and then call
      #       maybe return the object if post_fun was not
      #       set by the user. It is simply faster to not
      #       apply the post_fun than returning the input
      #       without modification.
      self$post_fun = post_fun
    },
    get_analyser = function() {
      return(private$analyser_fun)
    },
    analyser = function(analyser_input) {
      f <- self$get_analyser()
      return(f(analyser_input))
    },
    post_fun = NULL
  ),
  private = list(
    analyser_fun = NULL
  )
)
