dummy <- function(dat) {
  newdat <- data.frame(a = rep(0, nrow(dat)))
  for(i in 1:ncol(dat)) {
    if(is.numeric(dat[, i])) {
      newdat <- cbind(newdat, dat[, i])
      colnames(newdat)[ncol(newdat)] <- colnames(dat)[i]
    } else {
      dum <- data.frame(stats::model.matrix( ~ dat[, i] - 1)[, -1])
      colnames(dum) <- paste(colnames(dat[i]), ".", 1:ncol(dum), sep = "")
      newdat <- cbind(newdat, dum)
    }
  }
  newdat[, -1]
}

