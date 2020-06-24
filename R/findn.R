find.n <- function(dat, fast) {
  if(nrow(dat) * 0.5 > 200) {
    n <- 200
  } else {
    n <- round(nrow(dat) * 0.5)
  }
  n
}
