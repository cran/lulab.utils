library(testthat)

test_that("Table1 creates a table and workbook for two groups", {
  df <- data.frame(
    group = factor(rep(c("Control", "Case"), each = 20)),
    age = c(40:59, 45:64),
    marker = c(rnorm(20), rexp(20)),
    sex = factor(rep(c("Female", "Male"), 20))
  )

  result <- Table1(
    df = df,
    ycol = "group",
    xcol = c("age", "marker", "sex"),
    result_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_true(file.exists(file.path(tempdir(), "Table1.xlsx")))
  expect_true(any(grepl("P-value", names(result), fixed = TRUE)))
})

test_that("Table1 handles multiple groups and sparse categorical data", {
  df <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 8)),
    value = c(1:8, 2:9, c(3:9, NA)),
    category = factor(c(rep("x", 7), "y", rep("x", 8), rep(c("x", "z"), 4)))
  )

  result <- Table1(
    df = df,
    ycol = "group",
    xcol = c("value", "category"),
    result_dir = tempdir(),
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("Table1 validates inputs", {
  expect_error(Table1(data.frame(x = 1), "missing", "x", result_dir = tempdir()))
  expect_error(Table1(data.frame(g = 1), "g", "missing", result_dir = tempdir()))
})
