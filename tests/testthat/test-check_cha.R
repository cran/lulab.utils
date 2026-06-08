library(testthat)
data("melanoma", package = "boot")

test_that("checking for missing values for character columns works", {
  melanoma2 <- melanoma
  result= check_cha('status', melanoma2)

  expect_s3_class(result, "table")
})

test_that("check_cha handles factors, blanks, and missing values", {
  df <- data.frame(x = factor(c("A", "", NA, "Missing", "B")))

  result <- check_cha("x", df, verbose = FALSE)

  expect_s3_class(result, "table")
  expect_true("Missing" %in% names(result))
  expect_equal(unname(result[["Missing"]]), 0.6)
})

test_that("check_cha validates inputs", {
  expect_error(check_cha("missing", data.frame(x = 1)))
})
