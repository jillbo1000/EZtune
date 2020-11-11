
#------------------------------------------------------------------------------
#                           SVM binary ga function
#------------------------------------------------------------------------------

svm.bin.ga <- function(x = x, y = y, cross = NULL, fast = FALSE, loss = loss) {

  dat <- as.data.frame(cbind(y, x))

  #------------------------------------------------------------------------------
  #                           SVM binary CV function
  #------------------------------------------------------------------------------

  svm.bin.opt.cv <- function(params, cross, loss) {
    dat <- dat

    if(loss == "class") {
      pr <- NULL
      try(pr <- e1071::svm(as.factor(y) ~ ., data = dat, cost = params[1],
                           gamma = 2^params[2], cross = cross, fitted = TRUE))
      l <- 0.01 * pr$tot.accuracy
    } else if (loss == "auc") {
      pr <- NULL
      try(pr <- cv.pred.svm(dat, params, cross))
      l <- loss.bin(pred = pr, true_y = dat$y, loss = "auc")
    } else {
      stop("invalid optimization criterion - choose 'default', 'class', or 'auc' for loss")
    }

    if(is.null(pr)) l <- 0

    l
  }


  #------------------------------------------------------------------------------
  #                           SVM binary fast functions
  #------------------------------------------------------------------------------

  svm.bin.pred.fast <- function(x, y, n, cost, gamma) {
    dat <- cbind(y, x)
    dat2 <- dat[sample(nrow(dat)), ]
    train <- dat2[c(1:n), ]
    test <- dat2[-c(1:n), ]
    svm.t <- e1071::svm(as.factor(y) ~ ., data = train, cost = cost,
                        gamma = gamma, probability = TRUE)
    pr <- stats::predict(svm.t, newdata = test, probability = TRUE)
    pred <- attr(pr, "probabilities")[, colnames(attr(pr, "probabilities")) == "1"]
    data.frame(cbind(pred, y = test$y))
  }

  svm.bin.opt.fast <- function(dat = dat, params = params, n = n, loss = loss) {
    pr <- NULL
    try(pr <- svm.bin.pred.fast(dat[, -1], dat[, 1], n = n, cost = params[1],
                                gamma = 2^params[2]))
    if(!is.null(pr)) {
      l <- loss.bin(pred = pr$pred, true_y = pr$y, loss = loss)
    } else {
      l <- 0
    }
    l
  }


  #------------------------------------------------------------------------------
  #                           SVM binary resub function
  #------------------------------------------------------------------------------

  # Function for regular speed
  svm.bin.opt.resub <- function(params, loss){
    pr <- NULL
    try(pr <- e1071::svm(as.factor(y) ~ ., data = dat, cost = params[1],
                         gamma = 2^params[2], probability = TRUE))
    if(!is.null(pr)){
      pr1 <- stats::predict(pr, newdata = dat, probability = TRUE)
      pred <- attr(pr1, "probabilities")[, 1]
      l <- loss.bin(pred = pred, true_y = dat$y, loss = loss)
    } else {
      l <- 0
    }
    l
  }
  # initialize list
  results <- list()

  # setup fitness function based on user inputs
  if(is.null(cross) & !fast) {
    fit <- function(x) {svm.bin.opt.resub(x, loss)}
  } else if (fast > 0) {
    if(fast > 1) {
      n <- fast
    } else if(fast < 1) {
      n <- round(fast * nrow(dat))
    } else {
      n <- find.n(dat, fast)
    }
    fit <- function(x) {svm.bin.opt.fast(dat, x, n, loss)}
    results$n <- n
  } else if(!is.null(cross)) {
    if(cross >= 2) {
      fit <- function(x) {svm.bin.opt.cv(x, cross, loss)}
    } else {
      stop("Invalid number of folds for cross-validation. Use integer > 1.")
    }
    results$nfold <- cross
  } else {
    warning("Invalid option for fast. Default for fast used in computations.")
    n <- find.n(dat, fast)
    fit <- function(x) {svm.bin.opt.fast(dat, x, n, loss)}
    results$n <- n
  }

  ga.obj <- GA::ga(type = "real-valued", fitness = fit, parallel = 2,
                   maxiter = 10, run = 5, lower = c(1, -10), upper = c(1042, 5))

  results$cost <- as.numeric(ga.obj@solution[1, 1])
  results$gamma <- as.numeric(2^ga.obj@solution[1, 2])
  results$loss <- as.numeric(ga.obj@fitnessValue)
  results$model <- e1071::svm(as.factor(y) ~ ., data = dat,
                              cost = results$cost, gamma = results$gamma)

  results
}

