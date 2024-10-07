library(testthat)

test_that("test if wget is installed", {
  wget= check_wget()

  # check the result
  expect_true(is.logical(wget))
})
