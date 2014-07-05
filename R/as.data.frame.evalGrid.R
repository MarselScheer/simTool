#' Converts an 'evalGrid' object into a data.frame
#'
#'  Converts the results contained in the 
#'  object returned by \code{\link{evalGrids}} 
#'  into a \code{data.frame}. If the results can not
#'  be coerced into a data.frame automatically, the
#'  user can provide a function. Furthermore, functions
#'  to summarize the results over the replications
#'  the user may provide a function.
#'
#'  If necessary, more details than the description above 
#'
#'@param x  an object returned by 
#'  \code{\link{evalGrids}}
#'@param \dots  serves only to force the user to use the
#'  fullnames of following two parameters.
#'@param value.fun  A functions that converts the result
#'  object into a data.frame
#'@param post.proc  Functions to summarize the results over
#'  the replications, e.g. mean, sd.
#'@param progress if \code{TRUE} a progress bar is shown in the console.
#'@return  A \code{data.frame} with the parameter constellations
#'  for the data generation and evaluation and the results
#'  (probably summarized) in \code{data.frame} manner.
#'@author  Marsel Scheer
#'@seealso  \code{\link{evalGrids}}
#'@examples
#'
#'genRegData <- function(){
#'  data.frame(
#'      x = 1:10,
#'      y = rnorm(10, mean=1:10))
#'}
#'# the first parameter is ALWAYS used
#'# for the generated data
#'
#'LM <- function(data, formula){
#'  lm(formula=as.formula(formula), data=data)
#'}
#'eg <- evalGrids(
#'  expandGrid(fun="genRegData"),
#'  expandGrid(proc="LM", formula=c("y ~ x", "y ~ x + I(x^2)")),
#'  replications=5)
#'
#'lm2df = function(lm.object) {
#'  ret = coef(summary.lm(lm.object))[, 1:2]
#'  data.frame(covariable = rownames(ret), ret, check.names=FALSE)
#'}
#'as.data.frame(eg, value.fun=lm2df, progress=TRUE)
#'as.data.frame(eg, value.fun=lm2df, post.proc=c(mean, sd), progress=TRUE)
#'@export
as.data.frame.evalGrid <-
  function(x, ..., value.fun = identity, post.proc=NULL, progress=FALSE) {
    
    postFun = NULL
    if (!is.null(post.proc)){      
      if (length(post.proc) == 1) {
        postFun = post.proc
      } else {
        postFun = do.call(funstofun, as.list(match.call()$post.proc[-1]))    
      }
    }

    .progress="none"
    if (progress)
      .progress="text"    
    
    with(x, {
      ddply(expandGrid(i=1:nrow(dataGrid), j=1:nrow(procGrid)), .(i, j), function(row){
        i=row[1,1]
        j=row[1,2]  

        ret = ldply(simulation[[i]], function(rep) value.fun(rep$results[[j]]))
        if (nrow(ret) > 0)
          ret = cbind(replication=gl(length(simulation[[i]]), nrow(ret)/length(simulation[[i]])), ret)
        
        
        if (nrow(ret) > 0){
          if (!is.null(postFun)){
            ret$replication=NULL
            idx = which(sapply(1:ncol(ret), function(i) all(is.numeric(ret[,i]))))
            mdf = melt(ret, measure.vars=idx)
            ret = cast(mdf, ... ~ variable, postFun)
          }
        } else {
          ret = data.frame(.evalGridComment="Results missing")
        }
        if (!is.null(x$post.proc))
          ret$replication=NULL
        
        suppressWarnings(cbind(dataGrid[i,,drop=FALSE], procGrid[j,,drop=FALSE], ret))
      }, .progress=.progress)
    })
  }
