extract_fun_and_parameter <- function(row, envir) {
  idx_na <- which(is.na(row))
  idx_gt <- which(names(row) == ".truth")

  ret <- list(
    fun = get(unlist(row[1, 1]), envir = envir),
    para = as.list(row[1, -c(1, idx_na, idx_gt), drop = FALSE])
  )

  if (length(idx_gt) == 0) {
    gt <- ret$para
  } else if (length(idx_gt) == 1) {
    gt <- row[1, idx_gt, drop = TRUE]
  } else {
    stop("only one column with name '.truth' allowed")
  }
  attr(ret$fun, ".truth") <- gt
  ret
}
