
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
#' @param sample_size `int` random sample taken from `df` to speed
#' up calculations. If *NULL* all samples are used.
#' @param cv_folds number of cross validations folds
#' @param repeated_cv `int` number of repeated cross validations
#'
#' @return A list with the predictive power score for each feature in `x`
#'
pp_score <- function(df, x, y, sample_size = NULL,
                     cv_folds = 5, repeated_cv = 1) {

  stopifnot(is.data.frame(df), c(x, y) %in% names(df))

  results <- list()

  for (i in x) {
    results[i] <- score(df, i, y, sample_size, cv_folds, repeated_cv)
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
#' @details for mere information: https://github.com/8080labs/ppscore
#'
#' @param df `data.frame` input data which contains `x` and `y`
#' @param x `str` name of feature
#' @param y `str` name of target
#' @param sample_size `int` random sample taken from `df` to speed
#' up calculations. If *NULL* all samples are used.
#' @param cv_folds `int` number of cross validations folds
#' @param repeated_cv `int` number of repeated cross validations
#'
#' @importFrom rpart rpart
#'
#' @return The predictive power score for `x`.
#'
score <- function(df, x, y, metric, sample_size = NULL, cv_folds = 5L,
                  repeated_cv = 1L) {

  stopifnot(is.numeric(df[[x]]), length(unique(df[[y]])) == 2)

  # Removing NA & keeping only x and y
  df <- na.omit(df[c(x, y)])
  if (nrow(df) == 0) stop("Zero rows in data after removing NA's!")

  # Looping over repeated cv and cv folds
  results <- list()

  for(j in 1:repeated_cv) {

    # Sampling data
    df_sampled <- sample_data(df, sample_size)

    # Identifying cross validations folds
    cv <- sample(1:cv_folds, size = nrow(df_sampled), replace = TRUE)

    for (i in 1:cv_folds) {

      # Splitting data
      train <- df_sampled[cv != i, ]
      test <- df_sampled[cv == i, ]

      # Model
      fit <- rpart(paste(y, "~", x), data = train, method = "class")

      # Calculation metric
      out <- calculate_metric(fit, test[x], test[[y]], metric)

      # Save in results
      results[[as.character(j)]][i] <- out

      #f1 <- F1_Score(test[[y]], pred)
      # ConfusionDF(pred, test[[y]])
    }
  }

  mean(unlist(results))
}

# calculate_metric ------------------------------------------------------------
#' @title calculate_metric
#'
#' @param model `object` a model object
#' @param df `data.frame` test data to score
#' @param label `numeric` target vector
#' @param metric `str` Machine learning metric to evaluate on. Choose between
#' *roc_auc*.
#'
#' @importFrom MLmetrics AUC
#' @importFrom stats predict na.omit
#'
#' @return `numeric` calculated ml metric
#'
calculate_metric <- function(model, df, label, metric) {

  # Calculates metric
  if (metric == "roc_auc") {

    pred <- predict(model, df)[, 2]
    out <- AUC(pred, label)

  } else {
    stop(paste(metric, "is not a valid option!"))
  }

  return(out)
}

# sample_data -----------------------------------------------------------------
#' @title sample_data
#'
#' @description Takes a random sample from `df` of size `sample_size`.
#'
#' @param df `data.frame` input data to sample from
#' @param sample_size `int` or `NULL` number of samples to take from `df`.
#'
#' @return `data.frame` with samples from `df` or `df` if `sample_size`
#' is *NULL*.
#'
sample_data <- function(df, sample_size) {

  if (!is.null(sample_size))  {
    df <- df[sample(nrow(df), sample_size), ]
  }

  return(df)
}
