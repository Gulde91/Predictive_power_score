context("functions")

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

