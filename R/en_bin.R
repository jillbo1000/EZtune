
#------------------------------------------------------------------------------
#                           EN binary hjn function
#------------------------------------------------------------------------------

en.bin.hjn <- function(x = x, y = y, cross = cross, fast = fast, loss =loss) {

  y <- as.numeric(as.factor(y)) - 1
  x <- as.matrix(dummy(x))
  dat <- cbind(y, x)

  # initialize list
  results <- list()

  #------------------------------------------------------------------------------
  #                           EN binary resub function
  #------------------------------------------------------------------------------

  en.bin.opt.resub <- function(params, x, y, loss){
    pr <- NULL

    try(pr <- glmnet::glmnet(x, y, family = "binomial",
                       alpha = params[1], lambda = exp(params[2])))

    pred <- as.numeric(stats::predict(pr, newx = x, type = "class"))

    if(is.null(pr)){
      l <- 1
    } else {
      l <- loss.bin(pred = as.numeric(pred), true_y = y, loss = loss)
    }

    1 - l
  }


  #------------------------------------------------------------------------------
  #                           EN binary CV functions
  #------------------------------------------------------------------------------

  en.bin.cv <- function(x, y, cross, alpha, lambda, loss) {
    yval <- rep(0, nrow(x))
    xvs <- rep(1:cross, length = nrow(x))
    xvs <- sample(xvs)
    for(i in 1:cross) {
      train.x <- x[xvs != i, ]
      train.y <- y[xvs != i]
      test.x <- x[xvs == i, ]
      test.y <- y[xvs == i]
      en.t <- glmnet::glmnet(train.x, train.y, family = "binomial",
                             alpha = alpha, lambda = exp(lambda))
      yval[xvs == i] <- as.numeric(stats::predict(en.t, newx = test.x,
                                                  type = "class"))
    }

    l <- loss.bin(pred = yval, true_y = y, loss = loss)
    l
  }

  en.bin.opt.cv <- function(params, cross, x, y, loss) {
    pr <- NULL
    try(pr <- en.bin.cv(x = x, y = y, cross = cross, alpha = params[1],
                        lambda = params[2], loss = loss))
    if(!is.null(pr)){
      l <- 1 - pr
    } else {
      l <- 1
    }
    l
  }


  #------------------------------------------------------------------------------
  #                           EN binary fast functions
  #------------------------------------------------------------------------------

  en.bin.pred.fast <- function(x, y, n, alpha, lambda, loss) {
    dat <- cbind(y, x)
    dat2 <- dat[sample(nrow(dat)), ]
    train <- dat2[c(1:n), ]
    test <- dat2[-c(1:n), ]
    en.t <- glmnet::glmnet(train[, -1], train[, 1], family = "binomial",
                           alpha = alpha, lambda = exp(lambda))
    pred <- as.numeric(stats::predict(en.t, newx = test[, -1], type = "class"))

    l <- loss.bin(pred = pred, true_y = test[, 1], loss = loss)
    l
  }

  en.bin.opt.fast <- function(params, n, x, y, loss){
    pr <- NULL
    try(pr <- en.bin.pred.fast(x = x, y = y, n = n, alpha = params[1],
                               lambda = params[2], loss = loss))
    if(!is.null(pr)){
      l <- 1 - pr
    } else {
      l <- 1
    }
    l
  }

  # setup fitness function based on user inputs
  if(is.null(cross) & !fast) {
    fit <- function(p) {en.bin.opt.resub(p, x, y, loss)}
  } else if (fast > 0) {
    if(fast > 1) {
      n <- fast
    } else if(fast < 1) {
      n <- round(fast * nrow(dat))
    } else {
      n <- find.n(dat, fast)
    }
    fit <- function(p) {en.bin.opt.fast(p, n, x, y, loss)}
    results$n <- n
  } else if(!is.null(cross)) {
    if(cross >= 2) {
      fit <- function(p) {en.bin.opt.cv(p, cross, x, y, loss)}
    } else {
      stop("Invalid number of folds for cross-validation. Use integer > 1.")
    }
    results$nfold <- cross
  } else {
    warning("Invalid option for fast. Default for fast used in computations.")
    n <- find.n(dat, fast)
    fit <- function(p) {en.bin.opt.fast(p, n, x, y, loss)}
    results$n <- n
  }

  lmin <- log(min(glmnet::cv.glmnet(as.matrix(x), y, family = "binomial", alpha = 1)$lambda))
  lmax <- log(max(glmnet::cv.glmnet(as.matrix(x), y, family = "binomial", alpha = 0)$lambda)) / 2
  lstart <- lmin + (lmax - lmin) / 3

  hjn.obj <- optimx::hjn(par = c(0.75, lstart), fn = fit, lower = c(0, lmin),
                         upper = c(1, lmax))

  results$alpha <- hjn.obj$par[1]
  results$lambda <- exp(hjn.obj$par[2])
  results$loss <- as.numeric(1.0 - hjn.obj$value)
  results$model <- glmnet::glmnet(x, y, family = "binomial",
                                  alpha = results$alpha,
                                  lambda = results$lambda)

  results
}


