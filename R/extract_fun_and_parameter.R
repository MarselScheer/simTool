extract_fun_and_parameter <- function(row, envir) {
  idx_na <- which(is.na(row))
  idx_gt <- which(names(row) == ".truth")

  copy_primitives <- function(fun) {
    # upcoming R-version will not support attr(fun, "xyz") <- sth if fun is
    # a builtin function like mean, min, max, ...
    # so we wrap it to have a non-primitive function
    if (!is.primitive(fun)) {
       return(fun)
    }
    return(function(...) fun(...))
  }
  ret <- list(
    fun = copy_primitives(fun=get(unlist(row[1, 1]), envir = envir)),
    para = as.list(row[1, -c(1, idx_na, idx_gt), drop = FALSE])
  )

  if (length(idx_gt) == 0 && length(idx_na) == 0) {
    gt <- as.list(row[1, , drop = FALSE])
  } else if (length(idx_gt) == 0) {
    gt <- as.list(row[1, -idx_na, drop = FALSE])
  } else if (length(idx_gt) == 1) {
    gt <- row[1, idx_gt, drop = TRUE]
  } else {
    stop("only one column with name '.truth' allowed")
  }
  attr(ret$fun, ".truth") <- gt
  ret
}
