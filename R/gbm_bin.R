
#------------------------------------------------------------------------------
#                           GBM binary hjn function
#------------------------------------------------------------------------------

gbm.bin.hjn <- function(x = x, y = y, cross = cross, fast = fast, loss =loss) {

  y <- as.numeric(as.factor(y)) - 1
  dat <- as.data.frame(cbind(y, x))

  # initialize list
  results <- list()

  #------------------------------------------------------------------------------
  #                           GBM binary resub function
  #------------------------------------------------------------------------------

  gbm.bin.opt.resub <- function(params, dat, loss){
    pr <- NULL

    try(pr <- gbm::gbm(y ~ ., distribution = "bernoulli",
                       n.trees = round(params[1]),
                       interaction.depth = round(params[2]),
                       n.minobsinnode = round(params[3]),
                       shrinkage = params[4],
                       data = dat))
    pred <- gbm::predict.gbm(pr, newdata = dat, type="response",
                             n.trees = round(params[1]))

    if(is.null(pr)){
      l <- 1
    } else {
      l <- loss.bin(pred = pred, true_y = dat$y, loss = loss)
    }
    1 - l
  }


  #------------------------------------------------------------------------------
  #                           GBM binary CV functions
  #------------------------------------------------------------------------------

  gbm.bin.cv <- function(x, y, cross, tr, id, nmin, shr, loss) {
    dat <- cbind(y, x)
    yval <- rep(0, nrow(x))
    xvs <- rep(1:cross, length = nrow(x))
    xvs <- sample(xvs)
    cv.acc <- rep(0, cross)
    for(i in 1:cross) {
      train <- dat[xvs != i, ]
      test <- dat[xvs == i, ]
      gbm.t <- gbm::gbm(y ~ ., distribution = "bernoulli",
                        interaction.depth = id, n.trees = tr,
                        shrinkage = shr, data = train, n.minobsinnode = nmin)
      yval[xvs == i] <- gbm::predict.gbm(gbm.t, newdata = test, type="response",
                                         n.trees = tr)
    }
    l <- loss.bin(pred = yval, true_y = dat$y, loss = loss)
    l
  }

  gbm.bin.opt.cv <- function(params, cross, dat, loss) {
    pr <- NULL
    try(pr <- gbm.bin.cv(dat[, -1], dat[, 1],
                         cross = cross, tr = round(params[1]), id = round(params[2]),
                         nmin = round(params[3]), shr = params[4], loss = loss))
    if(!is.null(pr)){
      l <- 1 - pr
    } else {
      l <- 1
    }
    l
  }



  #------------------------------------------------------------------------------
  #                           GBM binary fast functions
  #------------------------------------------------------------------------------

  gbm.bin.pred.fast <- function(x, y, n, tr, id, nmin, shr, loss) {
    dat <- cbind(y, x)
    dat2 <- dat[sample(nrow(dat)), ]
    train <- dat2[c(1:n), ]
    test <- dat2[-c(1:n), ]
    gbm.t <- gbm::gbm(y ~ ., distribution = "bernoulli",
                      interaction.depth = id, n.trees = tr,
                      shrinkage = shr, data = train, n.minobsinnode = nmin)
    pred <- gbm::predict.gbm(gbm.t, newdata = test, type="response",
                             n.trees = tr)
    l <- loss.bin(pred = pred, true_y = test$y, loss = loss)
    l
  }

  gbm.bin.opt.fast <- function(params, n, dat, loss){
    pr <- NULL
    try(pr <- gbm.bin.pred.fast(dat[, -1], dat[, 1], n = n, tr = round(params[1]),
                                id = round(params[2]),
                                nmin = round(params[3]), shr = params[4], loss = loss))
    if(!is.null(pr)){
      l <- 1 - pr
    } else {
      l <- 1
    }
    l
  }

  # setup fitness function based on user inputs
  if(is.null(cross) & !fast) {
    fit <- function(x) {gbm.bin.opt.resub(x, dat, loss)}
  } else if (fast > 0) {
    if(fast > 1) {
      n <- fast
    } else if(fast < 1) {
      n <- round(fast * nrow(dat))
    } else {
      n <- find.n(dat, fast)
    }
    fit <- function(x) {gbm.bin.opt.fast(x, n, dat, loss)}
    results$n <- n
  } else if(!is.null(cross)) {
    if(cross >= 2) {
      fit <- function(x) {gbm.bin.opt.cv(x, cross, dat, loss)}
    } else {
      stop("Invalid number of folds for cross-validation. Use integer > 1.")
    }

    results$nfold <- cross
  } else {
    warning("Invalid option for fast. Default for fast used in computations.")
    n <- find.n(dat, fast)
    fit <- function(x) {gbm.bin.opt.fast(x, n, dat, loss)}
    results$n <- n
  }

  hjn.obj <- optimx::hjn(par = c(500, 5, 8, 0.09), fn = fit,
                         lower = c(50, 1, 5, 0.001),
                         upper = c(3000, 15, 12, 0.1))

  results$n.trees <- as.integer(round(hjn.obj$par[1]))
  results$interaction.depth <- as.integer(round(hjn.obj$par[2]))
  results$n.minobsinnode <- as.integer(round(hjn.obj$par[3]))
  results$shrinkage <- as.numeric(hjn.obj$par[4])
  results$loss <- as.numeric(1.0 - hjn.obj$value)
  results$model <- gbm::gbm(y ~ ., distribution = "bernoulli", data = dat,
                            n.trees = results$n.trees,
                            interaction.depth = results$interaction.depth,
                            n.minobsinnode = results$n.minobsinnode,
                            shrinkage = results$shrinkage)

  results
}


