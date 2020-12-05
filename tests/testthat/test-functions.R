context("functions")

test_that("Testing calculate_metric", {

  # creating test data
  set.seed(3)
  obs <- 1e3
  response <- sample(0:1, obs, replace = TRUE)
  feature <- rnorm(obs)
  feature[response == 1] <- rnorm(length(response[response == 1]), 0, 3)

  test_df <- data.frame(y = response, x = feature)

  # creating test model
  set.seed(3)
  fit <- rpart(y ~ x, test_df, method = "class")

  # applying function
  roc_auc <- calculate_metric(fit, test_df["x"], test_df[["y"]], metric = "roc_auc")
  pr_auc <- calculate_metric(fit, test_df["x"], test_df[["y"]], metric = "pr_auc")
  f1 <- calculate_metric(fit, test_df["x"], test_df[["y"]], metric = "F1")

  # testing
  expect_equal(roc_auc, 0.7545659, tolerance = 1e-7)
  expect_equal(pr_auc, 0.6189134, tolerance = 1e-7)
  expect_equal(f1, 0.7842149, tolerance = 1e-7)
})

test_that("Testing sample_data", {

  # creating test data
  test_df <- data.frame(V1 = rnorm(100), V2 = runif(100))
  size <- 49

  # applying function
  out <- sample_data(test_df, sample_size = size)
  out_full <- sample_data(test_df, NULL)

  # testing
  expect_equal(nrow(out), size)
  expect_equal(out_full, test_df)

})

