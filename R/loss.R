
#------------------------------------------------------------------------------
#                       loss function for binary data
#------------------------------------------------------------------------------


loss.bin <- function(pred, true_y, loss) {
  if(loss == "class") {
    mean(round(pred) == true_y)
  } else if (loss == "auc") {
    predA <- ROCR::prediction(pred, true_y)
    unlist(ROCR::performance(predA, "auc")@y.values)
  } else if (loss == "deviance") {
    stop("deviance only valid for elastic net - choose 'class' or 'auc' for loss")
  } else {
    stop("invalid optimization criterion - choose 'default', 'class', or 'auc' for loss")
  }
}


#------------------------------------------------------------------------------
#                       loss function for continuous data
#------------------------------------------------------------------------------


loss.reg <- function(pred, true_y, loss) {
  if(loss == "mse") {
    mean((pred - true_y)^2)
  } else if (loss == "mae") {
    mean(abs(pred - true_y))
  } else {
    stop("invalid optimization criterion - choose 'default', 'mse', or 'mae' for loss")
  }
}


#------------------------------------------------------------------------------
#                               cross-validation
#------------------------------------------------------------------------------


cv.pred.svm <- function(dat, params, cross) {

  yval <- rep(0, nrow(dat))
  xvs <- rep(1:cross, length = nrow(dat))
  xvs <- sample(xvs)
  for(i in 1:cross) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    svm.t <- e1071::svm(as.factor(y) ~ ., data = train, cost = params[1],
                        gamma = 2^params[2], probability = TRUE)
    pr <- stats::predict(svm.t, newdata = test[, -1], probability = TRUE)
    yval[xvs == i] <- attr(pr, "probabilities")[, colnames(attr(pr, "probabilities")) == "1"]
  }
  yval
}

cv.pred.svm.reg <- function(dat, params, cross) {

  yval <- rep(0, nrow(dat))
  xvs <- rep(1:cross, length = nrow(dat))
  xvs <- sample(xvs)
  for(i in 1:cross) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    svm.t <- e1071::svm(y ~ ., data = train, cost = params[1],
                        gamma = 2^params[2], epsilon = params[3])
    yval[xvs == i] <- stats::predict(svm.t, newdata = test[, -1])
  }
  yval
}

cv.pred.gbm <- function(dat, params, cross) {

  yval <- rep(0, nrow(dat))
  xvs <- rep(1:cross, length = nrow(dat))
  xvs <- sample(xvs)
  for(i in 1:cross) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    gbm.t <- gbm::gbm(y ~ ., distribution = "bernoulli", data = train,
                      n.trees = round(params[1]), interaction.depth = round(params[2]),
                      n.minobsinnode = round(params[3]), shrinkage = params[4])
    yval[xvs == i] <- gbm::predict.gbm(gbm.t, newdata = test, type="response",
                                             n.trees = round(params[1]))
  }
  yval
}

cv.pred.gbm.reg <- function(dat, params, cross) {

  yval <- rep(0, nrow(dat))
  xvs <- rep(1:cross, length = nrow(dat))
  xvs <- sample(xvs)
  for(i in 1:cross) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    gbm.t <- gbm::gbm(y ~ ., distribution = "gaussian", data = train,
                      n.trees = round(params[1]), interaction.depth = round(params[2]),
                      n.minobsinnode = round(params[3]), shrinkage = params[4])
    yval[xvs == i] <- gbm::predict.gbm(gbm.t, newdata = test, type="response",
                                       n.trees = round(params[1]))
  }
  yval
}

cv.pred.ada <- function(dat, params, cross) {

  yval <- rep(0, nrow(dat))
  xvs <- rep(1:cross, length = nrow(dat))
  xvs <- sample(xvs)
  for(i in 1:cross) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    gbm.t <- ada::ada(as.factor(y) ~ ., loss = "exponential",
                      nu = params[1], iter = round(params[2]),
                      data = train,
                      control = rpart::rpart.control(maxdepth = round(params[3])))
    yval[xvs == i] <- stats::predict(gbm.t, newdata = test, type = "prob")[,2]
  }
  yval
}

cv.pred.en <- function(x, y, params, cross) {

  yval <- rep(0, nrow(x))
  xvs <- rep(1:cross, length = nrow(x))
  xvs <- sample(xvs)
  for(i in 1:cross) {
    train.x <- x[xvs != i, ]
    train.y <- y[xvs != i]
    test.x <- x[xvs == i, ]
    test.y <- y[xvs == i]
    en.t <- glmnet::glmnet(train.x, train.y, family = "binomial",
                           alpha = params[1], lambda = exp(params[2]))
    yval[xvs == i] <- as.numeric(stats::predict(en.t, newx = test.x,
                                                type = "class"))
  }
  yval
}


cv.pred.en.reg <- function(x, y, params, cross) {

  yval <- rep(0, nrow(x))
  xvs <- rep(1:cross, length = nrow(x))
  xvs <- sample(xvs)
  for(i in 1:cross) {
    train.x <- x[xvs != i, ]
    train.y <- y[xvs != i]
    test.x <- x[xvs == i, ]
    test.y <- y[xvs == i]
    en.t <- glmnet::glmnet(train.x, train.y, family = "gaussian",
                           alpha = params[1], lambda = exp(params[2]))
    yval[xvs == i] <- stats::predict(en.t, newx = test.x, type="response")
  }
  yval
}

