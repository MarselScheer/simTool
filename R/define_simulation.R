define_simulation <- function(pf, discardGeneratedData, cluster, replications, summary.fun, post_analyze) {
  function(fc){
    withOutData = function(dummy){
      list(data=NULL, results=lapply(pf, function(f){post_analyze(f(fc()))}))
    }
    
    withData = function(dummy){
      data = fc()
      list(data=data, results=lapply(pf, function(f){post_analyze(f(data))}))      
    }
    
    if (discardGeneratedData){
      if (!is.null(cluster)){
        ret = parallel::parLapply(cluster, 1:replications, withOutData)
      } else {
        ret = lapply(1:replications, withOutData)
      }
    } else {
      if (!is.null(cluster)){
        ret = parallel::parLapply(cluster, 1:replications, withData)
      } else {
        ret = lapply(1:replications, withData)
      }
    }
    
    if (!is.null(summary.fun)){
      res <- purrr::map(ret, ~`[[`(., "results")) 
      # combine all results for the i-th function in procGrid
      res <- lapply(seq_along(res[[1]]), function(i) purrr::map(res, ~`[[`(., i)))
      res <- purrr::map(res, ~do.call("bind_or_combine", .))
      
      res <- lapply(summary.fun, function(f) purrr::map(res, ~dplyr::summarize_all(., f)))
      ret = list(list(results=res))
    }
    ret
  }
}
