
#------------------------------------------------------------------------------
#                           ADA binary cv accuracy
#------------------------------------------------------------------------------

ada.bin.cv <- function(x, y, model, cross = 10) {

  y <- as.numeric(as.factor(y)) - 1
  dat <- as.data.frame(cbind(y, x))
  yval <- rep(0, nrow(x))
  xvs <- rep(1:cross, length = nrow(x))
  xvs <- sample(xvs)
  cv.acc <- rep(0, cross)
  for(i in 1:cross) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    t1 <- rbind(test, train)
    ada.t <- ada::ada(as.factor(y) ~ ., loss = "exponential",
                      nu = model$nu, iter = model$iter, data = train,
                      control = rpart.control(maxdepth = model$maxdepth))
    yval[xvs == i] <- stats::predict(ada.t, newdata = t1, type = "prob")[1:nrow(test),2]
  }

  list(accuracy = loss.bin(pred = yval, true_y = dat$y, loss = "class"),
       auc = loss.bin(pred = yval, true_y = dat$y, loss = "auc"))
}


#------------------------------------------------------------------------------
#                           EN binary cv accuracy
#------------------------------------------------------------------------------

en.bin.cv <- function(x, y, model, cross = 10) {

  y <- as.numeric(as.factor(y)) - 1
  x <- as.matrix(dummy(x))
  yval <- rep(0, nrow(x))
  xvs <- rep(1:cross, length = nrow(x))
  xvs <- sample(xvs)
  cv.acc <- rep(0, cross)
  for(i in 1:cross) {
    train.x <- x[xvs != i, ]
    train.y <- y[xvs != i]
    test.x <- x[xvs == i, ]
    test.y <- y[xvs == i]
    t1 <- rbind(test.x, train.x)
    en.t <- glmnet::glmnet(train.x, train.y, family = "binomial",
                           alpha = model$alpha, lambda = model$lambda)
    yval[xvs == i] <- stats::predict(en.t, newx = t1,
                                     type = "response")[1:nrow(test.x)]
  }

  list(accuracy = loss.bin(pred = yval, true_y = y, loss = "class"),
       auc = loss.bin(pred = yval, true_y = y, loss = "auc"))
}


#------------------------------------------------------------------------------
#                           EN regression cv accuracy
#------------------------------------------------------------------------------

en.reg.cv <- function(x, y, model, cross = 10) {

  x <- as.matrix(dummy(x))
  yval <- rep(0, nrow(x))
  xvs <- rep(1:cross, length = nrow(x))
  xvs <- sample(xvs)
  cv.acc <- rep(0, cross)
  for(i in 1:cross) {
    train.x <- x[xvs != i, ]
    train.y <- y[xvs != i]
    test.x <- x[xvs == i, ]
    test.y <- y[xvs == i]
    t1 <- rbind(test.x, train.x)
    en.t <- glmnet::glmnet(train.x, train.y, family = "gaussian",
                           alpha = model$alpha, lambda = model$lambda)
    yval[xvs == i] <- stats::predict(en.t, newx = t1,
                                     type = "response")[1:nrow(test.x)]
  }

  list(mse = loss.reg(pred = yval, true_y = y, loss = "mse"),
       mae = loss.reg(pred = yval, true_y = y, loss = "mae"))
}


#------------------------------------------------------------------------------
#                           GBM regression cv error
#------------------------------------------------------------------------------

gbm.reg.cv <- function(x, y, model, cross = 10) {

  dat <- as.data.frame(cbind(y, x))
  yval <- cbind(rep(0, nrow(x)), y)
  xvs <- rep(1:cross, length = nrow(x))
  xvs <- sample(xvs)
  cv.acc <- rep(0, cross)
  for(i in 1:cross) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    t1 <- rbind(test, train)
    gbm.t <- gbm::gbm(y ~ ., distribution = "gaussian",
                      n.trees = model$n.trees,
                      interaction.depth = model$interaction.depth,
                      n.minobsinnode = model$n.minobsinnode,
                      shrinkage = model$shrinkage, data = train)
    yval[xvs == i, 1] <- gbm::predict.gbm(gbm.t, newdata = t1, type="response",
                                          n.trees = round(model$n.trees))[1:nrow(test)]
  }

  list(mse = loss.reg(pred = yval, true_y = y, loss = "mse"),
       mae = loss.reg(pred = yval, true_y = y, loss = "mae"))
}


#------------------------------------------------------------------------------
#                           GBM binary cv accuracy
#------------------------------------------------------------------------------

gbm.bin.cv <- function(x, y, model, cross = 10) {

  y <- as.numeric(as.factor(y)) - 1
  dat <- as.data.frame(cbind(y, x))
  yval <- rep(0, nrow(x))
  xvs <- rep(1:cross, length = nrow(x))
  xvs <- sample(xvs)
  cv.acc <- rep(0, cross)
  for(i in 1:cross) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    t1 <- rbind(test, train)
    gbm.t <- gbm::gbm(y ~ ., distribution = "bernoulli",
                      interaction.depth = model$interaction.depth,
                      n.trees = model$n.trees,
                      shrinkage = model$shrinkage,
                      n.minobsinnode = model$n.minobsinnode,
                      data = train)
    yval[xvs == i] <- round(gbm::predict.gbm(gbm.t, newdata = t1, type="response",
                                                n.trees = model$n.trees))[1:nrow(test)]
  }

  list(accuracy = loss.bin(pred = yval, true_y = dat$y, loss = "class"),
       auc = loss.bin(pred = yval, true_y = dat$y, loss = "auc"))
}


#------------------------------------------------------------------------------
#                           SVM regression cv mse
#------------------------------------------------------------------------------

svm.reg.cv <- function(x, y, model, cross = 10) {

  dat <- as.data.frame(cbind(y, x))
  yval <- rep(0, nrow(dat))
  xvs <- rep(1:cross, length = nrow(dat))
  xvs <- sample(xvs)
  for(i in 1:cross) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    t1 <- rbind(test, train)
    svm.t <- e1071::svm(y ~ ., data = train, cost = model$cost,
                        gamma = model$gamma, epsilon = model$epsilon)
    yval[xvs == i] <- stats::predict(svm.t, newdata = t1[, -1])[1:nrow(test)]
  }

  list(mse = loss.reg(pred = yval, true_y = dat$y, loss = "mse"),
       mae = loss.reg(pred = yval, true_y = dat$y, loss = "mae"))
}


#------------------------------------------------------------------------------
#                           SVM binary cv accuracy
#------------------------------------------------------------------------------

svm.bin.cv <- function(x, y, model, cross = 10) {

  y <- as.numeric(as.factor(y)) - 1
  dat <- as.data.frame(cbind(y, x))
  yval <- rep(0, nrow(dat))
  xvs <- rep(1:cross, length = nrow(dat))
  xvs <- sample(xvs)
  for(i in 1:cross) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    # t1 <- rbind(test, train)
    svm.t <- e1071::svm(as.factor(y) ~ ., data = train, cost = model$cost,
                        gamma = model$gamma, probability = TRUE)
    pr <- stats::predict(svm.t, newdata = test, probability = TRUE)
    yval[xvs == i] <- attr(pr, "probabilities")[, colnames(attr(pr, "probabilities")) == "1"]
    # yval[xvs == i] <- stats::predict(svm.t, newdata = test[, -1])
  }

  list(accuracy = loss.bin(pred = yval, true_y = dat$y, loss = "class"),
       auc = loss.bin(pred = yval, true_y = dat$y, loss = "auc"))
}




