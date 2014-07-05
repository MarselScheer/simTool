#' Generates source code for the simulation
#'
#'  Generates hard coded source code from the information
#'  provided by \code{dataGrid} and \code{procGrid}.
#'
#'  If necessary, more details than the description above 
#'
#'@param dataGrid  see \code{\link{evalGrids}}
#'@param procGrid  see \code{\link{evalGrids}}
#'@param replications  see \code{\link{evalGrids}}
#'@param discardGeneratedData  see \code{\link{evalGrids}}
#'@return  see \code{\link{evalGrids}}
#'@author  Marsel Scheer
#'@seealso  \code{\link{evalGrids}}
#'@examples
#'\dontrun{hardCode(expandGrid(fun="runif", n=1:3),expandGrid(proc=c("rng", "median")),rep=3)}
#'@export
hardCode <- function(dataGrid, procGrid, replications=1, discardGeneratedData=TRUE){
  results=hardCodeOneLine(procGrid, "r", TRUE)
  s = sapply(1:nrow(dataGrid), function(i){
    data=hardCodeOneLine(dataGrid[i,], "d")    
    rName = paste("r", 1:length(results), sep="")
    rName = paste(paste(rName, rName, sep="="), collapse=", ")
    lName = paste("l", i, sep="")
    if (discardGeneratedData)
      tmp = " = list(data=NULL, results=list("
    else
      tmp = " = list(data=d1, results=list("
    c("\n\n", data, "\n", paste(results, "\n"),
      paste(lName, tmp, rName, ")", ")")
    )
  })
  
  lName = paste("l", 1:nrow(dataGrid), sep="")
  lName = paste(paste(lName, "=", lName), collapse=", ")
  
  ret = c("simulation <- replicate(", replications,  ", {", 
          s, "\n\n", paste("list(", lName, ")"),
          "})")
  cat(ret)
  ret
}


# 
# dg = expandGrid(fun="rexp", n=c(10,100,1000), rate=1/(1:6))
# pg = expandGrid(proc=c("mean", "sd", "median"))
# hc = hardCode2(dg, pg)
# write(hc, file="/tmp/out")
