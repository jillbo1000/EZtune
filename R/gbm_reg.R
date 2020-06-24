
#------------------------------------------------------------------------------
#                           GBM regression hjn function
#------------------------------------------------------------------------------

gbm.reg.hjn <- function(x = x, y = y, cross = cross, fast = fast, loss = loss) {

  dat <- as.data.frame(cbind(y, x))

  # initialize list
  results <- list()

  #------------------------------------------------------------------------------
  #                           GBM regression resub function
  #------------------------------------------------------------------------------

  # Function for regular speed
  gbm.reg.opt.resub <- function(params, dat, loss){
    pr <- NULL
    try(pr <- gbm::gbm(y ~ ., distribution = "gaussian",
                       n.trees = round(params[1]),
                       interaction.depth = round(params[2]),
                       n.minobsinnode = round(params[3]),
                       shrinkage = params[4],
                       data = dat))
    if(!is.null(pr)){
      pred <- gbm::predict.gbm(pr, newdata = dat, type = "response",
                               n.trees = round(params[1]))
      l <- loss.reg(pred = pred, true_y = dat$y, loss = loss)
    } else {
      l <- 1e+150
    }
    l
  }


  #------------------------------------------------------------------------------
  #                           GBM regression CV function
  #------------------------------------------------------------------------------

  gbm.reg.cv <- function(x, y, cross, tr, id, nmin, shr, loss) {
    dat <- cbind(y, x)
    yval <- rep(0, nrow(x))
    xvs <- rep(1:cross, length = nrow(x))
    xvs <- sample(xvs)
    cv.acc <- rep(0, cross)
    for(i in 1:cross) {
      train <- dat[xvs != i, ]
      test <- dat[xvs == i, ]
      gbm.t <- gbm::gbm(y ~ ., distribution = "gaussian",
                        n.trees = tr, interaction.depth = id,
                        n.minobsinnode = nmin, shrinkage = shr, data = train)
      yval[xvs == i] <- gbm::predict.gbm(gbm.t, newdata = test, type="response",
                                            n.trees = round(tr))
    }
    l <- loss.reg(pred = yval, true_y = dat$y, loss = loss)
    l
  }

  gbm.reg.opt.cv <- function(params, dat, cross, loss) {
    pr <- NULL
    try(pr <- gbm.reg.cv(dat[, -1], dat[, 1],
                         cross = cross, tr = round(params[1]),
                         id = round(params[2]), nmin = round(params[3]),
                         shr = params[4], loss = loss))
    if(!is.null(pr)){
      l <- pr
    } else {
      l <- 1e+150
    }
    l
  }



  #------------------------------------------------------------------------------
  #                           GBM regression fast functions
  #------------------------------------------------------------------------------

  gbm.reg.pred.fast <- function(x, y, n, tr, id, nmin, shr, loss) {
    dat <- cbind(y, x)
    dat2 <- dat[sample(nrow(dat)), ]
    train <- dat2[c(1:n), ]
    test <- dat2[-c(1:n), ]
    gbm.t <- gbm::gbm(y ~ ., distribution = "gaussian",
                      n.trees = tr, interaction.depth = id,
                      n.minobsinnode = nmin, shrinkage = shr, data = train)
    pred <- stats::predict(gbm.t, newdata = test, type="response",
                           n.trees = round(tr))
    loss.reg(pred = pred, true_y = test$y, loss = loss)
  }

  gbm.reg.opt.fast <- function(params, dat, n, loss){
    pr <- NULL
    try(pr <- gbm.reg.pred.fast(dat[, -1], dat[, 1], n = n, tr = round(params[1]),
                                id = round(params[2]), nmin = round(params[3]),
                                shr = params[4], loss))
    if(!is.null(pr)){
      l <- pr
    } else {
      l <- 1e+150
    }
    l
  }

  # setup fitness function based on user inputs
  if(is.null(cross) & !fast) {
    fit <- function(x) {gbm.reg.opt.resub(x, dat, loss)}
  } else if (fast > 0) {
    if(fast > 1) {
      n <- fast
    } else if(fast < 1) {
      n <- round(fast * nrow(dat))
    } else {
      n <- find.n(dat, fast)
    }
    fit <- function(x) {gbm.reg.opt.fast(x, dat, n, loss)}
    results$n <- n
  } else if(!is.null(cross)) {
    if(cross >= 2) {
      fit <- function(x) {gbm.reg.opt.cv(x, dat, cross, loss)}
    } else {
      stop("Invalid number of folds for cross-validation. Use integer > 1.")
    }

    results$nfold <- cross
  } else {
    warning("Invalid option for fast. Default for fast used in computations.")
    n <- find.n(dat, fast)
    fit <- function(x) {gbm.reg.opt.fast(x, n)}
    results$n <- n
  }

  hjn.obj <- optimx::hjn(par = c(2000, 8, 6, 0.01), fn = fit,
                         lower = c(50, 1, 5, 0.001),
                         upper = c(5000, 15, 10, 0.1))

  results$n.trees <- as.integer(round(hjn.obj$par[1]))
  results$interaction.depth <- as.integer(round(hjn.obj$par[2]))
  results$n.minobsinnode <- as.integer(round(hjn.obj$par[3]))
  results$shrinkage <- as.numeric(hjn.obj$par[4])
  results$loss <- as.numeric(hjn.obj$value)
  results$model <- gbm::gbm(y ~ ., distribution = "gaussian", data = dat,
                            n.trees = results$n.trees,
                            interaction.depth = results$interaction.depth,
                            n.minobsinnode = results$n.minobsinnode,
                            shrinkage = results$shrinkage)

  results
}


