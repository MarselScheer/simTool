define_simulation <- function(pf, discard_generated_data, cluster,
                              replications, summary_fun, group_for_summary,
                              post_analyze) {
  function(fc) {
    truth <- attr(fc, ".truth")
    with_out_data <- function(dummy) {
      list(data = NULL, results = lapply(pf, function(f) {
        post_analyze(f(fc(), truth), truth)
      }))
    }

    with_data <- function(dummy) {
      data <- fc()
      list(data = data, results = lapply(pf, function(f) {
        post_analyze(f(data, truth), truth)
      }))
    }

    if (discard_generated_data) {
      if (!is.null(cluster)) {
        ret <- parallel::parLapply(cluster, 1:replications, with_out_data)
      } else {
        ret <- lapply(1:replications, with_out_data)
      }
    } else {
      if (!is.null(cluster)) {
        ret <- parallel::parLapply(cluster, 1:replications, with_data)
      } else {
        ret <- lapply(1:replications, with_data)
      }
    }

    if (!is.null(summary_fun)) {
      res <- purrr::map(ret, ~ `[[`(., "results"))
      # combine all results for the i-th function in procGrid
      res <- lapply(
        seq_along(res[[1]]),
        function(i) purrr::map(res, ~ `[[`(., i))
      )
      res <- purrr::map(res, ~ do.call("bind_or_combine", .))
      if (!is.null(summary_fun) && !is.null(group_for_summary)) {
        class(group_for_summary) <- "list"
        res <- lapply(summary_fun, function(f) {
          purrr::map(res, ~ dplyr::summarize_all(
            dplyr::group_by(., .dots = group_for_summary), f
          ))
        })
      } else {
        res <- lapply(summary_fun, function(f) {
          purrr::map(res, ~ dplyr::summarize_all(., f))
        })
      }

      ret <- list(list(results = res))
    }
    ret
  }
}
