#' Supervised Learning Function
#'
#' \code{eztune} is a function that automatically tunes adaboost, support
#' vector machines, gradient boosting machines, and elastic net. An
#' optimization algorithm is used to find a good set of tuning parameters
#' for the selected model. The function optimizes on a validation dataset,
#' cross validated accuracy, or resubstitution accuracy.
#' @param x Matrix or data frame containing the dependent variables.
#' @param y Vector of responses. Can either be a factor or a numeric vector.
#' @param method Model to be fit. Choices are "\code{ada}" for adaboost,
#' "\code{en}" for elastic net, "\code{gbm}" for gradient boosting machines,
#' and "\code{svm}" for support
#' vector machines.
#' @param optimizer Optimization method. Options are "\code{ga}" for a genetic
#'  algorithm and "\code{hjn}" for a Hooke-Jeeves optimizer.
#' @param fast Indicates if the function should use a subset of the
#'  observations when optimizing to speed up calculation time. A value
#'  of \code{TRUE} will use the smaller of 50\% of the data or 200 observations
#'  for model fitting, a number between \code{0} and \code{1} specifies the
#'  proportion of data to be used to fit the model, and a positive integer
#'  specifies the number of observations to be used to fit the
#'  model. A model is computed using a random selection of data and
#'  the remaining data are used to validate model performance. The
#'  validation error measure is used as the optimization criterion.
#' @param loss The type of loss function used for optimization. Options
#' for models with a binary response are "\code{class}" for classification
#' error and "\code{auc}" for area under the curve. Options for models with a
#' continuous response are "\code{mse}" for mean squared error and
#' "\code{mae}" for mean absolute error. If the option "default" is selected,
#' or no loss is specified, the classification accuracy will be used for a binary
#' response model and the MSE will be use for models with a continuous
#' model.
#' @param cross If an integer k \> 1 is specified, k-fold cross-validation
#'  is used to fit the model. This method is very slow for large datasets.
#'  This parameter is ignored unless \code{fast = FALSE}.
#' @return Function returns an object of class "\code{eztune}" which contains
#' a summary of the tuning parameters for the best model, the best loss
#' measure achieved (classification accuracy, AUC, MSE, or MAE), and the best
#' model.
#' \item{loss}{Best loss measure obtained by the optimizer. This is
#' the measure specified by the user that the optimizer uses to choose a
#' "best" model (classification accuracy, AUC, MSE, or MAE). Note that
#' if the default option is used it is the classification
#' accuracy for a binary response and the MSE for a continuous response.}
#' \item{model}{Best model found by the optimizer. Adaboost model
#'  comes from package \code{ada} (\code{ada} object), elastic net model
#'  comes from package \code{glmnet} (\code{glmnet} object), gbm model
#'  comes from package \code{gbm} (\code{gbm.object} object), svm (\code{svm}
#'  object) model comes from package \code{e1071}.}
#' \item{n}{Number of observations used in model training when
#' fast option is used}
#' \item{nfold}{Number of folds used if cross validation is used
#' for optimization.}
#' \item{iter}{Tuning parameter for adaboost.}
#' \item{nu}{Tuning parameter for adaboost.}
#' \item{shrinkage}{Tuning parameter for adaboost and gbm.}
#' \item{lambda}{Tuning parameter for elastic net}
#' \item{alpha}{Tuning parameter for elastic net}
#' \item{n.trees}{Tuning parameter for gbm.}
#' \item{interaction.depth}{Tuning parameter for gbm.}
#' \item{n.minobsinnode}{Tuning parameter for gbm.}
#' \item{cost}{Tuning parameter for svm.}
#' \item{gamma}{Tuning parameter for svm.}
#' \item{epsilon}{Tuning parameter for svm regression.}
#' \item{levels}{If the model has a binary response, the levels of y are listed.}
#'
#' @examples
#' library(mlbench)
#' data(Sonar)
#' sonar <- Sonar[sample(1:nrow(Sonar), 100), ]
#'
#' y <- sonar[, 61]
#' x <- sonar[, 1:10]
#'
#' # Optimize an SVM using the default fast setting and Hooke-Jeeves
#' eztune(x, y)
#'
#' # Optimize an SVM with 3-fold cross validation and Hooke-Jeeves
#' eztune(x, y, fast = FALSE, cross = 3)
#'
#' # Optimize GBM using training set of 50 observations and Hooke-Jeeves
#' \donttest{eztune(x, y, method = "gbm", fast = 50, loss = "auc")}
#'
#' # Optimize SVM with 25% of the observations as a training dataset
#' # using a genetic algorithm
#' \donttest{eztune(x, y, method = "svm", optimizer = "ga", fast = 0.25)}
#'
#' @export
#'
eztune <- function(x, y, method = "svm", optimizer = "hjn", fast = TRUE,
                   cross = NULL, loss = "default") {

  nms <- colnames(x)

  if(length(unique(y)) == 2) {
    lev <- levels(as.factor(y))
    y <- as.numeric(as.factor(y)) - 1
    type <- "bin"
    if(loss == "default") loss = "class"
  } else {
    y <- as.numeric(as.character(y))
    type <- "reg"
    if(loss == "default") loss = "mse"
  }

  if(fast > 1) {
    fast <- round(fast)
  }

  if(!is.null(cross)) {
    cross <- round(cross)
  }


  command <- paste(type, method, optimizer, sep = ".")

  ezt <- switch(command,
                bin.ada.ga = ada.bin.ga(x, y, cross = cross, fast = fast, loss = loss),
                bin.ada.hjn = ada.bin.hjn(x, y, cross = cross, fast = fast, loss = loss),
                bin.gbm.ga = gbm.bin.ga(x, y, cross = cross, fast = fast, loss = loss),
                bin.gbm.hjn = gbm.bin.hjn(x, y, cross = cross, fast = fast, loss = loss),
                bin.svm.ga = svm.bin.ga(x, y, cross = cross, fast = fast, loss = loss),
                bin.svm.hjn = svm.bin.hjn(x, y, cross = cross, fast = fast, loss = loss),
                bin.en.ga = en.bin.ga(x, y, cross = cross, fast = fast, loss = loss),
                bin.en.hjn = en.bin.hjn(x, y, cross = cross, fast = fast, loss = loss),
                reg.gbm.ga = gbm.reg.ga(x, y, cross = cross, fast = fast, loss = loss),
                reg.gbm.hjn = gbm.reg.hjn(x, y, cross = cross, fast = fast, loss = loss),
                reg.svm.ga = svm.reg.ga(x, y, cross = cross, fast = fast, loss = loss),
                reg.svm.hjn = svm.reg.hjn(x, y, cross = cross, fast = fast, loss = loss),
                reg.en.ga = en.reg.ga(x, y, cross = cross, fast = fast, loss = loss),
                reg.en.hjn = en.reg.hjn(x, y, cross = cross, fast = fast, loss = loss)
  )

  ezt$variables <- nms

  if(grepl("bin.", command)) ezt$levels <- lev

  class(ezt) <- "eztune"

  ezt
}
