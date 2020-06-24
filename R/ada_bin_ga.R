
#------------------------------------------------------------------------------
#                           ADA binary ga function
#------------------------------------------------------------------------------

ada.bin.ga <- function(x = x, y = y, cross = cross, fast = fast, loss = loss) {

  dat <- as.data.frame(cbind(y, x))

  #------------------------------------------------------------------------------
  #                           ADA binary resub function
  #------------------------------------------------------------------------------

  # Function for regular speed
  ada.bin.opt.resub <- function(params, dat, loss){
    pr <- NULL
    try(pr <- ada::ada(as.factor(y) ~ ., loss = "exponential",
                       nu = params[1], iter = round(params[2]),
                       data = dat,
                       control = rpart::rpart.control(maxdepth = round(params[3]))))
    if(!is.null(pr)){
      pred <- stats::predict(pr, newdata = dat, type = "prob")[,2]
      l <- loss.bin(pred = pred, true_y = dat$y, loss = loss)
    } else {
      l <- 0
    }
    l
  }


  #------------------------------------------------------------------------------
  #                           ADA binary CV functions
  #------------------------------------------------------------------------------

  ada.bin.cv <- function(x, y, cross, nu, iter, maxd, loss) {
    dat <- cbind(y, x)
    yval <- rep(0, nrow(x))
    xvs <- rep(1:cross, length = nrow(x))
    xvs <- sample(xvs)
    cv.acc <- rep(0, cross)
    for(i in 1:cross) {
      train <- dat[xvs != i, ]
      test <- dat[xvs == i, ]
      ada.t <- ada::ada(as.factor(y) ~ ., loss = "exponential",
                        nu = nu, iter = iter, data = train,
                        control = rpart::rpart.control(maxdepth = maxd))
      yval[xvs == i] <- stats::predict(ada.t, newdata = test, type = "prob")[,2]
    }
    l <- loss.bin(pred = yval, true_y = dat$y, loss = loss)
  }

  ada.bin.opt.cv <- function(params, cross, dat, loss) {
    pr <- NULL
    try(pr <- ada.bin.cv(dat[, -1], dat[, 1], cross = cross,
                         nu = round(params[1]), iter = round(params[2]),
                         maxd = round(params[3]), loss = loss))
    if(!is.null(pr)){
      l <- pr
    } else {
      l <- 1
    }
    l
  }


  #------------------------------------------------------------------------------
  #                           ADA binary fast functions
  #------------------------------------------------------------------------------

  ada.bin.pred.fast <- function(x, y, n, nu, iter, maxd, loss) {
    dat <- cbind(y, x)
    dat2 <- dat[sample(nrow(dat)), ]
    train <- dat2[c(1:n), ]
    test <- dat2[-c(1:n), ]
    ada.t <- ada::ada(as.factor(y) ~ ., loss = "exponential",
                      nu = nu, iter = iter, data = train,
                      control = rpart::rpart.control(maxdepth = maxd))
    pred <- stats::predict(ada.t, newdata = test, type = "prob")[,2]
    loss.bin(pred = pred, true_y = test$y, loss = loss)
  }

  ada.bin.opt.fast <- function(params, n, dat, loss){
    pr <- NULL
    try(pr <- ada.bin.pred.fast(dat[, -1], dat[, 1], n = n,
                                nu = round(params[1]), iter = round(params[2]),
                                maxd = round(params[3]), loss = loss))
    if(!is.null(pr)){
      l <- pr
    } else {
      l <- 0
    }
    l
  }

  # initialize list
  results <- list()

  # setup fitness function based on user inputs
  if(is.null(cross) & !fast) {
    fit <- function(x) {ada.bin.opt.resub(x, dat, loss)}
  } else if (fast > 0) {
    if(fast > 1) {
      n <- fast
    } else if(fast < 1) {
      n <- round(fast * nrow(dat))
    } else {
      n <- find.n(dat, fast)
    }
    fit <- function(x) {ada.bin.opt.fast(x, n, dat, loss)}
    results$n <- n
  } else if(!is.null(cross)) {
    if(cross >= 2) {
      fit <- function(x) {ada.bin.opt.cv(x, cross, dat, loss)}
    } else {
      stop("Invalid number of folds for cross-validation. Use integer > 1.")
    }

    results$nfold <- cross
  } else {
    warning("Invalid option for fast. Default for fast used in computations.")
    n <- find.n(dat, fast)
    fit <- function(x) {ada.bin.opt.fast(x, n, dat, loss)}
    results$n <- n
  }

  ga.obj <- GA::ga(type = "real-valued", fitness = fit, parallel = 2,
                   maxiter = 10, run = 5, lower = c(0.01, 50, 1),
                   upper = c(1, 300, 20))

  results$nu <- as.numeric(ga.obj@solution[1, 1])
  results$iter <- as.integer(round(ga.obj@solution[1, 2]))
  results$maxdepth <- as.integer(round(ga.obj@solution[1, 3]))
  results$loss <- as.numeric(ga.obj@fitnessValue)
  results$model <- ada::ada(as.factor(y) ~ ., loss = "exponential",
                            nu = results$nu, iter = results$iter,
                            data = dat,
                            control = rpart::rpart.control(maxdepth = results$maxdepth))

  results
}

