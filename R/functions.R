

# score -----------------------------------------------------------------------
#' @title Calculates predictive power score
#'
#' @description Calculates predictive power score for `x` predicts `y`. This is
#' a light wip implementation.
#'
#' @param df `data.frame` input data which contains `x` and `y`
#' @param x `str` name of feature
#' @param y `str` name of target
#' @param cv_folds `int` number of cross validations folds
#'
#' @importFrom rpart rpart
#' @importFrom pROC auc roc
#'
#' @return The predictive power score.
#'
score <- function(df, x, y, cv_folds = 5L) {

  stopifnot(is.data.frame(df), c(x, y) %in% names(df))

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
    roc_auc <- suppressMessages(auc(roc(test[[y]], pred)))

    results[i] <- roc_auc

    #f1 <- F1_Score(test[[y]], pred)
    # ConfusionDF(pred, test[[y]])
  }

  mean(unlist(results))

}

