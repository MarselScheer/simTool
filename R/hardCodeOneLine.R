#' Helperfunction for \code{\link{hardCode}}
#'
#'  A concise (1-5 lines) description of what the function does. 
#'
#'  If necessary, more details than the description above 
#'
#'@param dataGrid  Describe \code{dataGrid} here
#'@param var  Describe \code{var} here
#'@param dataArgument  Describe \code{dataArgument} here
#'@param silent  Describe \code{silent} here
#'@return  ~Describe the value returned  If it is a LIST, use 
#'\item{comp1 }{Description of 'comp1'}  \item{comp2 }{Description of
#''comp2'}  ...
#'@note  further notes
#'@author  who you are
#'@seealso  objects to See Also as \code{\link{help}}, ~
#'@references  ~put references to the literature/web site here ~
#'@keywords ~kwd1 ~kwd2
#'@examples
#'
#'##---- Should be DIRECTLY executable !! ----
#'##-- ==>  Define data, use random,
#'##--  or do  help(data=index)  for the standard data sets.
#'
#'## The function is currently defined as
#'function (dataGrid, var, dataArgument = FALSE, silent = TRUE) 
#'{
#'    s = sapply(1:nrow(dataGrid), function(i) {
#'        para = as.list(dataGrid[i, -c(1, which(is.na(dataGrid[i, 
#'            ]))), drop = FALSE])
#'        str = as.character(call(dataGrid[i, 1], para))
#'        str[2] = substr(str[2], start = 6, stop = nchar(str[2]))
#'        tmp = "("
#'        if (dataArgument) {
#'            if (nchar(str[2]) == 1) {
#'                tmp = "(d1"
#'            }
#'            else {
#'                tmp = "(d1, "
#'            }
#'        }
#'        str[2] = paste(tmp, str[2], sep = "")
#'        paste(str, collapse = "")
#'    })
#'    hc = paste(paste(var, 1:length(s), sep = ""), "<-", s)
#'    if (!silent) 
#'        cat(hc, "\n")
#'    hc
#'  }
#'
hardCodeOneLine <- function(dataGrid, var, dataArgument=FALSE, silent=TRUE){
  s = sapply(1:nrow(dataGrid), function(i){
    para = as.list(dataGrid[i,-c(1, which(is.na(dataGrid[i,]))), drop=FALSE])
    str = as.character(call(dataGrid[i,1], para))      
    
    # delete "list(" at the beginning
    str[2] = substr(str[2], start=6, stop=nchar(str[2]))
    tmp = "("
    if(dataArgument){
      if (nchar(str[2]) == 1){
        tmp = "(d1"
      } else {
        tmp = "(d1, "      
      }
    }
    str[2] = paste(tmp, str[2], sep="")
    paste(str, collapse="")
  })
  
  hc = paste(paste(var, 1:length(s), sep=""), "<-", s)
  if (!silent)
    cat(hc, "\n")
  
  hc
}



# hardCode <- function(dataGrid, var, dataArgument=FALSE, silent=FALSE){
#   s = sapply(1:nrow(dataGrid), function(i){
#     para = as.list(dataGrid[i,-c(1, which(is.na(dataGrid[i,]))), drop=FALSE])
#     str = as.character(call(dataGrid[i,1], para))      
#     # delete "list(" at the beginning
#     str[2] = substr(str[2], start=6, stop=nchar(str[2]))
#     tmp = "("
#     if(dataArgument){
#       if (nchar(str[2]) == 1){
#         tmp = "(x "
#       } else {
#         tmp = "(x"      
#       }
#     }
#     str[2] = paste(tmp, str[2], sep="")
#     paste(str, collapse="")
#   })
#   if (dataArgument)
#     fc = paste("function(x)", s)
#   else
#     fc = paste("function()", s)
#   
#   tempF = tempfile()  
#   hc = paste(paste(var, "="), "list(", paste(fc, collapse=",\n"), ")")
#   if (!silent)
#     cat(hc, "\n")
#   
#   write(hc, file=tempF)
#   source(tempF)
# }
