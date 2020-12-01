
# pp_score --------------------------------------------------------------------
#' @title pp_score
#'
#' @description Calculates predictive power score for `x` predicts `y`. Where
#' `x` can be multiple features in `df`. This is a light (wip) implementation
#' which for now only supports binary classification and x as numeric features.
#'
#' @param df data.frame
#' @param x `str` name of features in `df`
#' @param y `str` target feature
#' @param cv_folds number of cross validations folds
#'
#' @return A list with the predictive power score for each feature in `x`
#'
pp_score <- function(df, x, y, cv_folds) {

  stopifnot(is.data.frame(df), c(x, y) %in% names(df))

  results <- list()

  for (i in x) {
    results[i] <- score(df, i, y, cv_folds)
  }

  return(results)
}


# score -----------------------------------------------------------------------
#' @title Calculates predictive power score
#'
#' @description Calculates predictive power score for `x` predicts `y`. This is
#' a light (wip) implementation which for now only supports binary
#' classification and x as numeric a feature.
#'
#' @param df `data.frame` input data which contains `x` and `y`
#' @param x `str` name of feature
#' @param y `str` name of target
#' @param cv_folds `int` number of cross validations folds
#'
#' @importFrom rpart rpart
#' @importFrom MLmetrics AUC
#'
#' @return The predictive power score.
#'
score <- function(df, x, y, cv_folds = 5L) {

  stopifnot(is.numeric(df[[x]]), length(unique(df[[y]])) == 2)

  # removing NA
  df <- na.omit(df[c(x, y)])

  # Identifying cross validations folds
  cv <- sample(1:cv_folds, size = nrow(df), replace = TRUE)

  results <- list()

  for (i in 1:cv_folds) {

    # Splitting data
    train <- data[cv != i, c(x, y)]
    test <- data[cv == i, c(x, y)]

    # Model
    fit <- rpart(paste(y, "~", x), data = train, method = "class")

    # Predictions
    pred <- predict(fit, test)[, 2]

    # Calculation roc auc
    roc_auc <- AUC(pred, test[[y]])

    results[i] <- roc_auc

    #f1 <- F1_Score(test[[y]], pred)
    # ConfusionDF(pred, test[[y]])
  }

  mean(unlist(results))

}

