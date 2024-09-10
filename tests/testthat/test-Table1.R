library(testthat)
library(magrittr)
data("melanoma", package = "boot")

test_that("making Table1 in SCI papers works", {
  # Prepare the data
  melanoma2 <- melanoma
  melanoma2$status <- factor(melanoma2$status, 
                             levels = c(2, 1, 3),
                             labels = c("Alive", "Melanoma death", "Non-melanoma death"))
  
  # Call the function
  result <- Table1(
    df = melanoma2,
    xcol = setdiff(names(melanoma2), "status"),
    ycol = "status",
    result_dir = tempdir()
  )
  
  # Check the results
  expect_true(file.exists(file.path(tempdir(),"Table1.xlsx")))
  expect_s3_class(result, "data.frame")
  expect_named(result)
  expect_true(nrow(result) > 0)
  expect_true(ncol(result) > 0)
})

