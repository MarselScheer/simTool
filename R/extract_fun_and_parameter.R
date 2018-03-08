extract_fun_and_parameter = function(row, envir)
{
  list(
    fun = get(unlist(row[1,1]), envir = envir),
    para = as.list(row[1,-c(1, which(is.na(row[1,]))), drop = FALSE])
  )
}
