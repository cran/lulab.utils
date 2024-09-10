library(testthat)
data("melanoma", package = "boot")

test_that("checking for missing values for character columns works", {
  melanoma2 <- melanoma
  result= check_cha('status', melanoma2)

  expect_s3_class(result, "table")
})
