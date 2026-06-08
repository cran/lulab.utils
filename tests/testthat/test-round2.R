library(testthat)

test_that("round2 rounds halves away from zero", {
  expect_equal(round2(1.25, digits = 1), 1.3)
  expect_equal(round2(-1.25, digits = 1), -1.3)
  expect_equal(round2(1.2), 1)
})

test_that("round2 validates inputs", {
  expect_error(round2("1"))
  expect_error(round2(1, digits = NA))
})
