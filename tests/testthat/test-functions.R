context("functions")



test_that("Testing score", {

  # creating test data
  n_obs <- 100
  set.seed(3)
  test_df <- data.frame(y = sample(0:1, n_obs, replace = TRUE),
                        x = rnorm(n_obs),
                        x_na = NA_real_)

  # applying function
  out <- score(test_df, "x", "y", cv_folds = 2, repeated_cv = 2)

  # testing
  val_data <- list(pp_score = .5077,
                   eval_metric = "roc_auc",
                   cv_scores = list(`1` = c(.4948, .4935),
                                    `2` = c(.4458, .5967)))

  expect_equal(out, val_data, tolerance = 0.4)
  expect_error(score(test_df, "x_na", "y"), "Zero rows in data after removing NA's!")

})

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

