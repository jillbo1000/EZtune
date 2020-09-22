#' @import utils
utils::globalVariables(c("dat"))

#------------------------------------------------------------------------------
#                           SVM regression ga function
#------------------------------------------------------------------------------

svm.reg.ga <- function(x = x, y = y, cross = cross, fast = fast, loss = loss) {

  dat <- as.data.frame(cbind(y, x))

  # initialize list
  results <- list()

  #------------------------------------------------------------------------------
  #                           SVM regression resub function
  #------------------------------------------------------------------------------

  # Function for regular speed
  svm.reg.opt.resub <- function(params, dat, loss) {
    pr <- NULL

    try(pr <- e1071::svm(y ~ ., data = dat, cost = params[1],
                         gamma = 2^params[2], epsilon = params[3]))
    if(!is.null(pr)){
      l <- loss.reg(pred = pr$fitted, true_y = dat$y, loss = loss)
    } else {
      l <- 1e+150
    }
    l
  }


  #------------------------------------------------------------------------------
  #                           SVM regression CV function
  #------------------------------------------------------------------------------

  svm.reg.opt.cv <- function(params, dat, cross, loss) {
    pr <- NULL

    if(loss == "mse") {
      try(pr <- e1071::svm(y ~ ., data = dat, cost = params[1],
                           gamma = 2^params[2], epsilon = params[3],
                           cross = cross))
      l <- pr$tot.MSE[1, 1]
    } else if(loss == "mae") {
      try(pr <- cv.pred.svm.reg(dat = dat, params = params, cross = cross))
      l <- loss.reg(pred = pr, true_y = dat$y, loss = "mae")
    } else {
      l <- 1e+150
    }

    l
  }


  #------------------------------------------------------------------------------
  #                           SVM regression fast functions
  #------------------------------------------------------------------------------

  svm.reg.pred.fast <- function(x, y, n, cost, gamma, epsilon, loss) {
    dat <- cbind(y, x)
    dat2 <- dat[sample(nrow(dat)), ]
    train <- dat2[c(1:n), ]
    test <- dat2[-c(1:n), ]
    svm.t <- e1071::svm(y ~ ., data = train, cost = cost, gamma = gamma,
                        epsilon = epsilon)
    pred <- stats::predict(svm.t, newdata = test[, -1])
    loss.reg(pred = pred, true_y = test$y, loss = loss)
  }

  svm.reg.opt.fast <- function(params, dat, n, loss){
    pr <- NULL
    try(pr <- svm.reg.pred.fast(dat[, -1], dat[, 1], n = n, cost = params[1],
                                gamma = 2^params[2], epsilon = params[3],
                                loss = loss))
    if(!is.null(pr)){
      l <- pr
    } else {
      l <- 1e+150
    }
    l
  }

  # setup fitness function based on user inputs
  if(is.null(cross) & !fast) {
    fit <- function(p) {-1 * svm.reg.opt.resub(p, dat, loss)}
  } else if (fast > 0) {
    if(fast > 1) {
      n <- fast
    } else if(fast < 1) {
      n <- round(fast * nrow(dat))
    } else {
      n <- find.n(dat, fast)
    }
    fit <- function(p) {-1 * svm.reg.opt.fast(p, dat, n, loss)}
    results$n <- n
  } else if(!is.null(cross)) {
    if(cross >= 2) {
      fit <- function(p) {-1 * svm.reg.opt.cv(p, dat, cross, loss)}
    } else {
      stop("Invalid number of folds for cross-validation. Use integer > 1.")
    }

    results$nfold <- cross
  } else {
    warning("Invalid option for fast. Default for fast used in computations.")
    n <- find.n(dat, fast)
    fit <- function(p) {-1 * svm.reg.opt.fast(p, dat, n, loss)}
    results$n <- n
  }

  ga.obj <- GA::ga(type = "real-valued", fitness = fit, parallel = 2,
                   maxiter = 10, run = 5, lower = c(1, -10, 0),
                   upper = c(1042, 5, 0.5))

  results$cost <- as.numeric(ga.obj@solution[1, 1])
  results$gamma <- as.numeric(2^ga.obj@solution[1, 2])
  results$epsilon <- as.numeric(ga.obj@solution[1, 3])
  results$loss <- as.numeric(-1 * ga.obj@fitnessValue)
  results$model <- e1071::svm(y ~ ., data = dat, cost = results$cost,
                              gamma = results$gamma, epsilon = results$epsilon)

  results
}
