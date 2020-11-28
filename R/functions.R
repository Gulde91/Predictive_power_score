
library(rpart)

load("./Data/titanic.rda")

x <- titanic$parch
y <- titanic$survived

index <- caret::createDataPartition(y, times = 2, p = 0.8)

x_train <- x[index[[1]]]
x_test <- x[-index[[1]]]

y_train <- y[index[[1]]]
y_test <- y[-index[[1]]]


fit <- rpart(y~x, method = 'class')

