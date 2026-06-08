library(testthat)

test_that("test if wget is installed", {
  old_method <- getOption("download.file.method")
  old_extra <- getOption("download.file.extra")
  on.exit({
    options(download.file.method = old_method)
    options(download.file.extra = old_extra)
  }, add = TRUE)

  expect_false(use_wget(use = FALSE))

  # check the result
  expect_true(is.null(getOption("download.file.method")))
  expect_true(is.null(getOption("download.file.extra")))

  # use wget
  result <- use_wget(use = TRUE)

  # check the result
  if(Sys.info()["sysname"] == "Windows" && isTRUE(result)){
    expect_equal(getOption("download.file.method"), "wget")
    expect_equal(getOption("download.file.extra"), c("-c"))
  }
})
