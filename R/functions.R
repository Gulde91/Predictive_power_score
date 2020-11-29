
library(rpart)
library(caret)
library(MLmetrics)
library(pROC)

load("./Data/titanic.rda")

data <- titanic
x <- "parch"
y <- "survived"
cv_folds <- 4



cv <- sample(1:cv_folds, size = nrow(data), replace = TRUE)

results <- list()

for (i in 1:cv_folds) {

  # dividing data
  train <- data[cv != i, c(x, y)]
  test <- data[cv == i, c(x, y)]

  # model
  fit <- rpart(paste(y, "~", x), data = train, method = "class")

  # predictions
  pred <- predict(fit, test)[, 2]

  # udregner roc auc
  roc_auc <- suppressMessages(auc(roc(test[[y]], pred)))

  results[i] <- roc_auc

  #f1 <- F1_Score(test[[y]], pred)
  # ConfusionDF(pred, test[[y]])

  # anden træ funktion
  # fit_ny <- tree::tree(paste(y, "~", x), train, split = "gini")
  # fit_cv <- tree::cv.tree(fit_ny, K = cv_folds)
}

sapply(1:cv_folds, function(i) get_cv_auc(x, y, data[cv != i, ], data[cv == i, ]))
#sapply(4, function(i) get_cv_auc(x, y, data[cv != i, ], d[cv == i, ]))

get_cv_auc <- function(x, y, train, test) {

  # model
  fit <- rpart(paste(y, "~", x), data = train, method = "class")

  # predictions
  pred <- predict(fit, test[x])[, 2]

  # udregner roc auc
  roc_auc <- suppressMessages(auc(roc(test[[y]], pred)))

}


pred <- predict(fit, df_test)



# index <- createDataPartition(y, times = 2, p = 0.8)
#
# x_train <- x[index[[1]]]
# x_test <- x[-index[[1]]]
#
# y_train <- y[index[[1]]]
# y_test <- y[-index[[1]]]
#
# df_train <- data.frame(x = x_train, y = y_train)
# df_test <- data.frame(x = x_test, y = y_test)
