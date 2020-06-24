
#------------------------------------------------------------------------------
#                           EN regression hjn function
#------------------------------------------------------------------------------

en.reg.hjn <- function(x = x, y = y, cross = cross, fast = fast, loss = loss) {

  dat <- cbind(y, x)
  x <- as.matrix(dummy(x))

  # initialize list
  results <- list()

  #------------------------------------------------------------------------------
  #                           EN regression resub function
  #------------------------------------------------------------------------------

  # Function for regular speed
  en.reg.opt.resub <- function(params, x, y, loss){
    pr <- NULL
    try(pr <- glmnet::glmnet(x, y, family = "gaussian", alpha = params[1],
                             lambda = exp(params[2])))
    if(!is.null(pr)){
      pred <- stats::predict(pr, newx = x, type = "response")
      l <- loss.reg(pred = pred, true_y = y, loss = loss)
    } else {
      l <- 1e+150
    }
    l
  }


  #------------------------------------------------------------------------------
  #                           EN regression CV function
  #------------------------------------------------------------------------------

  en.reg.cv <- function(x, y, cross, alpha, lambda, loss) {
    yval <- rep(0, nrow(x))
    xvs <- rep(1:cross, length = nrow(x))
    xvs <- sample(xvs)
    cv.acc <- rep(0, cross)
    for(i in 1:cross) {
      train.x <- x[xvs != i, ]
      train.y <- y[xvs != i]
      test.x <- x[xvs == i, ]
      test.y <- y[xvs == i]
      en.t <- glmnet::glmnet(train.x, train.y, distribution = "gaussian", alpha = alpha,
                             lambda = exp(lambda))
      yval[xvs == i] <- stats::predict(en.t, newx = test.x, type="response")
    }
    l <- loss.reg(pred = yval, true_y = y, loss = loss)
    l
  }

  en.reg.opt.cv <- function(params, x, y, cross, loss) {
    pr <- NULL
    try(pr <- en.reg.cv(x = x, y = y, cross = cross, alpha = params[1],
                         lambda = params[2], loss = loss))
    if(!is.null(pr)){
      l <- pr
    } else {
      l <- 1e+150
    }
    l
  }



  #------------------------------------------------------------------------------
  #                           EN regression fast functions
  #------------------------------------------------------------------------------

  en.reg.pred.fast <- function(x, y, n, alpha, lambda, loss) {
    dat <- cbind(y, x)
    dat2 <- dat[sample(nrow(dat)), ]
    train <- dat2[c(1:n), ]
    test <- dat2[-c(1:n), ]
    en.t <- glmnet::glmnet(train[, -1], train[, 1], family = "gaussian",
                           alpha = alpha, lambda = exp(lambda))
    pred <- stats::predict(en.t, newx = test[, -1], type="response")
    loss.reg(pred = pred, true_y = test[, 1], loss = loss)
  }

  en.reg.opt.fast <- function(params, x, y, n, loss){
    pr <- NULL
    try(pr <- en.reg.pred.fast(x = x, y = y, n = n, alpha = params[1],
                                lambda = params[2], loss))
    if(!is.null(pr)){
      l <- pr
    } else {
      l <- 1e+150
    }
    l
  }

  # setup fitness function based on user inputs
  if(is.null(cross) & !fast) {
    fit <- function(p) {en.reg.opt.resub(p, x, y, loss)}
  } else if (fast > 0) {
    if(fast > 1) {
      n <- fast
    } else if(fast < 1) {
      n <- round(fast * nrow(dat))
    } else {
      n <- find.n(dat, fast)
    }
    fit <- function(p) {en.reg.opt.fast(p, x, y, n, loss)}
    results$n <- n
  } else if(!is.null(cross)) {
    if(cross >= 2) {
      fit <- function(p) {en.reg.opt.cv(p, x, y, cross, loss)}
    } else {
      stop("Invalid number of folds for cross-validation. Use integer > 1.")
    }

    results$nfold <- cross
  } else {
    warning("Invalid option for fast. Default for fast used in computations.")
    n <- find.n(dat, fast)
    fit <- function(p) {en.reg.opt.fast(p, x, y, n, loss)}
    results$n <- n
  }

  lmin <- log(min(glmnet::cv.glmnet(x, y, family = "gaussian", alpha = 1)$lambda))
  lmax <- log(max(glmnet::cv.glmnet(x, y, family = "gaussian", alpha = 0)$lambda)) / 2
  lstart <- lmin + (lmax - lmin) / 3

  hjn.obj <- optimx::hjn(par = c(0.75, lstart), fn = fit, lower = c(0, lmin),
                         upper = c(1, lmax))

  results$alpha <- hjn.obj$par[1]
  results$lambda <- exp(hjn.obj$par[2])
  results$loss <- as.numeric(hjn.obj$value)
  results$model <- glmnet::glmnet(x, y, family = "gaussian",
                                  alpha = results$alpha,
                                  lambda = results$lambda)

  results
}


