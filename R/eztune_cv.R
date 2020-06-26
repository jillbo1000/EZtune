#' Cross Validated Accuracy for Supervised Learning Model
#'
#' \code{eztune_cv} returns the cross-validated
#' loss measures for a model returned by \code{eztune}.
#' The function \code{eztune} can tune a model using validation data,
#' resubstitution or cross validation. If resubstitution or a fast method
#' is used to tune the model, the accuracy obtained from the function
#' may not be accurate. The function \code{eztune_cv} will return
#' cross-validated accuracy measures for any model returned by \code{eztune}.
#' @param x Matrix or data frame containing the dependent variables used
#' to create the model.
#' @param y Vector of the response used to create the model. Can be either
#' numeric or a factor.
#' @param model Object generated with the function \code{eztune}.
#' @param cross Number of folds to use for n-fold cross-validation.
#' @return Function returns a numeric value that represents the
#' cross-validated accuracy of the model. Both classification accuracy
#' and the AUC are returned for models with a binary response. MSE and
#' mean absolute error (MAE) are returned for models with a continuous
#' response.
#' \item{accuracy}{Cross-validated classification accuracy.}
#' \item{auc}{Cross-validated AUC.}
#' \item{mse}{Cross-validated MSE.}
#' \item{mae}{Cross-validated MAE.}
#'
#' @examples
#' library(mlbench)
#' data(Sonar)
#' sonar <- Sonar[sample(1:nrow(Sonar), 100), ]
#'
#' y <- sonar[, 61]
#' x <- sonar[, 1:10]
#'
#' sonar_default <- eztune(x, y)
#' eztune_cv(x, y, sonar_default)
#'
#' sonar_svm <- eztune(x, y, fast = FALSE, cross = 3)
#' eztune_cv(x, y, sonar_svm)
#'
#' sonar_gbm <- eztune(x, y, method = "gbm", fast = 50)
#' eztune_cv(x, y, sonar_gbm)
#'
#'
#' @export
#'
eztune_cv <- function(x, y, model, cross = 10) {

  if(length(unique(y)) == 2) {
    y <- as.numeric(as.factor(y)) - 1
    type <- "bin"
  } else {
    y <- as.numeric(as.character(y))
    type <- "reg"
  }

  mod <- substr(class(model$model)[1], 1, 3)
  type <- paste(mod, type, "cv", sep = ".")

  res <- switch(type,
                ada.bin.cv = ada.bin.cv(x, y, model, cross = cross),
                log.bin.cv = en.bin.cv(x, y, model, cross = cross),
                eln.reg.cv = en.reg.cv(x, y, model, cross = cross),
                gbm.bin.cv = gbm.bin.cv(x, y, model, cross = cross),
                gbm.reg.cv = gbm.reg.cv(x, y, model, cross = cross),
                svm.bin.cv = svm.bin.cv(x, y, model, cross = cross),
                svm.reg.cv = svm.reg.cv(x, y, model, cross = cross))

  res
}
