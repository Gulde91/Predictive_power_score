library(rpart)
library(caret)
library(MLmetrics)
library(pROC)

load("./Data/titanic.rda")

obs <- 1e4

sim_response <- sample(0:1, obs, replace = TRUE, prob = c(0.8, 0.2))
sim_feature <- rnorm(obs)
sim_feature[sim_response == 1] <- rnorm(length(sim_response[sim_response == 1]), 0, 3)

plot(sim_feature, sim_response)
df <- data.frame(y = sim_response, x = sim_feature)

score(df, "x", "y", "F1", cv_folds = 4L, repeated_cv = 5L)



get_cv_auc <- function(x, y, train, test) {

  # model
  fit <- rpart(paste(y, "~", x), data = train, method = "class")

  # predictions
  pred <- predict(fit, test[x])[, 2]

  # udregner roc auc
  roc_auc <- AUC(pred, test[[y]])

}

score(df = titanic, x = "parch", y = "survived",
      sample_size = NULL, cv_folds = 4L, repeated_cv = 2L)

pp_score(titanic, c("sibsp", "parch", "pclass"), "survived",
         sample_size = NULL, 5, 10)



