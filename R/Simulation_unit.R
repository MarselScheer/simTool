#' @import data.table
#' @export
Simulation_unit <- R6::R6Class("Eval_suite",
  public = list(
    initialize = function(generator, analysers, replications,
                          aggregate_funs = NULL) {
      private$generator <- generator
      private$analysers <- analysers
      private$replications <- replications
      private$aggregate_funs <- aggregate_funs
    },
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
      ret <- ret[, c("generator", "analyser", "replication", "result")]
      ret <- private$post_processing(raw_results = ret)
      ret <- private$aggregate(result_frame = ret)
      return(ret)
    }
  ),
  private = list(
    generator = NULL,
    analysers = NULL,
    replications = NULL,
    aggregate_funs = NULL,
    aggregate = function(result_frame) {
      if (is.null(private$aggregate_funs)) {
        return(result_frame)
      }
      sub <- result_frame[replication == 1,]
      if (all(sapply(sub$result, function(x) is.vector(x) && length(x) == 1))) {
        result_frame[, result := unlist(result)]
      } else {
        result_frame[, id := .I]
        results <- data.table::rbindlist(
          l = result_frame$result,
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
    },
    post_processing = function(raw_results) {
      ret <- lapply(
        X = names(private$analysers),
        FUN = function(analyser_name) {
          analyser_instance <- private$analysers[[analyser_name]]
          post_fun <- analyser_instance$post_fun
          sub <- raw_results[analyser == analyser_name]
          if (is.null(post_fun)) {
            return(sub)
          }
          sub[["result"]] <- lapply(sub[["result"]], post_fun)
          return(sub)
        })
      ret <- data.table::rbindlist(l = ret)
    }
  )
)
