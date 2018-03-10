progress_bar <- function(df) {
  if (!interactive()) {
    return(function() {

    })
  }
  count <- 0
  pb <- utils::txtProgressBar(max = length(df), style = 3)
  utils::setTxtProgressBar(pb, 0)
  function() {
    count <<- count + 1
    utils::setTxtProgressBar(pb, count)
  }
}
