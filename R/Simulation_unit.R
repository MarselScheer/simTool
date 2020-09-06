#' R6 Class used to define which Data_generator is
#' used with which Data_analyser
#'
#' @import data.table
#' @export
Simulation_unit <- R6::R6Class("Simulation_unit",
  public = list(
    ##' @description
    ##' Creates a new Simulation_unit object.
    ##' @param generator a named list with one element containing
    ##' a \link{Data_generator}
    ##' @param analysers a named list where every element must be
    ##' a \link{Data_analyser} that is able to process the output
    ##' Data_generator passed to \code{generator}
    ##' @param replications number of replications to be performed
    ##' @param aggregate_funs (optional) a named list of functions
    ##' where every function aggregates the replications to one
    ##' value. Of course the aggregation happens stratified for
    ##' every analyser.
    ##' @return A new Simulation_unit object
    initialize = function(generator, analysers, replications,
                          aggregate_funs = NULL) {
      private$generator <- generator
      private$analysers <- analysers
      private$replications <- replications
      private$aggregate_funs <- aggregate_funs
    },
    ##' @description
    ##' Generates a data set using the Data_generator and
    ##' pass it to every Data_analyser. This is repeated
    ##' as often as defined during constructing the object.
    ##' Furhtermore, the replications are aggregated
    ##' according to the definition during construction of the
    ##' object
    ##' @return a data.table with column
    ##' \describe{
    ##'   \item{generator}{name for the generator
    ##'    defined during construction of the object}
    ##'   \item{analyser}{name for the analyser
    ##'    defined during construction of the object}
    ##'   \item{replication}{number of the replication. Note this
    ##'   column is missing if aggregation was defined}
    ##'   \item{result}{output of the corresponding analyser or
    ##'    the aggregated output over all replications if
    ##'   aggregation was defined. In the case of aggregation
    ##'   it is possible that also the column name result was
    ##'   substitute by the corresponding output of the analyser}
    ##' }
    run_evaluation = function() {
      reps <- seq_len(length.out = private$replications)
      ret <- data.table::data.table(
        generator = names(private$generator),
        analyser = names(private$analysers),
        replication = as.numeric(gl(
          n = private$replications,
          k = length(private$analysers))))

      analysers <- lapply(
        X = private$analysers,
        FUN = function(instance) instance[["analyser"]])
      gen <- private$generator[[1]]$generator

      results <- lapply(reps, function(rep) {
        d <- gen()
        lapply(analysers, function(f) {
          f(analyser_input = d)
        })
      })
      ret[["result"]] <- purrr::flatten(.x = results)
      names(ret[["results"]]) <- NULL
      ret <- ret[, c("generator", "analyser", "replication", "result")]
      ret <- private$aggregate(result_frame = ret)
      return(ret)
    }
  ),
  private = list(
    generator = NULL,
    analysers = NULL,
    replications = NULL,
    aggregate_funs = NULL,
    ## @description
    ## aggregates the raw replication according to the user
    ## defined aggregation function
    ## @param result_frame results to be aggregated
    ## @return if no aggregation is defined \code{result_frame} is returned
    ## Otherwise see \link{run_evaluation}, where column replication was
    ## dropped.
    aggregate = function(result_frame) {
      if (is.null(private$aggregate_funs)) {
        return(result_frame)
      }
      sub <- result_frame[replication == 1,]
      if (all(sapply(sub$result, function(x) is.vector(x) && length(x) == 1))) {
        result_frame[, result := unlist(result)]
      } else {
        result_frame[, id := .I]
        # remove names otherwise rbindlist
        # would use the names for the id
        results <- result_frame$result
        names(results) <- NULL
        results <- data.table::rbindlist(
          l = results,
          idcol = "id",
          fill = TRUE)
        result_frame[, result := NULL]
        result_frame <- merge(x = result_frame, y = results, by = "id")
        result_frame[, id := NULL]
      }
      cols <- setdiff(names(result_frame), c("generator", "analyser", "replication"))
      ret <- data.table::dcast(
        data = result_frame,
        formula =  generator + analyser ~ .,
        fun.aggregate = private$aggregate_funs,
        value.var = cols)
      ret
    }
  )
)
