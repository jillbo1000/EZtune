#' Prediction function for EZtune
#'
#' \code{predict.eztune} Computes predictions for a validation dataset.
#' @param object An object of class "\code{eztune}".
#' @param newdata Matrix or data frame containing the test or validation dataset.
#' @param ... Additional parameters to pass to predict.
#' @return Function returns a vector of predictions if the response is
#' continuous. If the response is binary, a \code{data.frame} with the predicted
#' response and the probabilities of each response type is returned.
#'
#' @examples
#' library(EZtune)
#' data(lichen)
#' data(lichenTest)
#'
#' y <- lichen[, 2]
#' x <- lichen[, 9:41]
#'
#' # Optimize an SVM classification model using the default settings
#' mod1 <- eztune(x, y)
#'
#' # Obtain predictions using the lichenTest dataset and compute classification
#' # error
#' pred <- predict(mod1, lichenTest)
#' mean(pred$predictions == as.factor(lichenTest$LobaOreg))
#'
#' # Optimize an SVM regression model using the default settings
#' library(mlbench)
#' library(dplyr)
#' library(yardstick)
#' data(BostonHousing2)
#' bh <- mutate(BostonHousing2, lcrim = log(crim)) %>%
#'   select(-town, -medv, -crim)
#' x <- bh[, c(1:3, 5:17)]
#' y <- bh[, 4]
#' mod2 <- eztune(x, y)
#'
#' # Obtain predictions from the original data and compute the rmse
#' pred <- predict(mod2, x)
#' rmse_vec(pred, y)
#' @rdname predict.eztune
#' @export
#'
predict.eztune <- function(object, newdata, ...) {

  x <- newdata
  if(!is.data.frame(x) & !is.matrix(x)) stop("newdata must be a data.frame or matrix")
  type <- "reg"
  lev <- NULL
  try(lev <- object$levels)
  nms <- object$variables
  model <- object$model

  if("svm" %in% class(model)) {
    if(!is.null(lev)) {
      pr <- stats::predict(model, newdata = x, probability = TRUE, ...)
      pred <- data.frame(predictions = round(attr(pr, "probabilities")[, colnames(attr(pr, "probabilities")) == "1"]),
                         prob1 = attr(pr, "probabilities")[, colnames(attr(pr, "probabilities")) == "0"],
                         prob2 = attr(pr, "probabilities")[, colnames(attr(pr, "probabilities")) == "1"])
      type <- "bin"
    } else {
      pred <- stats::predict(model, newdata = x, ...)
    }
  } else if("gbm" %in% class(model)) {
    if(!is.null(lev)) {
      pr <- gbm::predict.gbm(model, newdata = x, type="response",
                             n.trees = model$n.trees, ...)
      pred <- data.frame(predictions = round(pr), prob1 = 1 - pr, prob2 = pr)
      type <- "bin"
    } else {
      pred <- gbm::predict.gbm(model, newdata = x, type="response",
                               n.trees = round(model$n.trees), ...)
    }
  } else if("glmnet" %in% class(model)) {
    x <- x[, colnames(x) %in% nms]
    x <- as.matrix(dummy(x))
    if(!is.null(lev)) {
      pr <- stats::predict(model, newx = x, type = "response")
      pred <- data.frame(predictions = round(pr), prob1 = 1 - pr, prob2 = pr)
      type <- "bin"
    } else {
      pred <- stats::predict(model, newx = x, type = "response", ...)[, 1]
    }
  } else if(class(model) == "ada") {
    pr <- stats::predict(model, newdata = x, type = "prob")
    pred <- data.frame(predictions = round(pr[, 2]), prob1 = pr[, 1],
                       prob2 = pr[, 2], ...)
    type <- "bin"
  }

  if(type == "bin") {
    colnames(pred)[1] <- c("predictions")
    pred$predictions <- ifelse(pred$predictions, lev[2], lev[1])
    pred$predictions <- factor(pred$predictions, levels = lev)
    colnames(pred)[2:3] <- paste0(lev, "_prob")
  }

  pred
}
