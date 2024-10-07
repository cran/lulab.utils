library(testthat)

test_that("test if wget is installed", {
  use_wget(use = FALSE)

  # check the result
  expect_true(is.null(getOption("download.file.method")))
  expect_true(is.null(getOption("download.file.extra")))

  # use wget
  use_wget(use = TRUE)

  # check the result
  if(Sys.info()["sysname"] == "Windows"){
    expect_equal(getOption("download.file.method"), "wget")
    expect_equal(getOption("download.file.extra"), c("-c"))

    # test download to prove wget is used
    test_url <- "https://eternallybored.org/misc/wget/1.21.4/64/wget.exe"
    test_destfile <- tempfile()
    download.file(test_url, destfile = test_destfile)

    expect_true(file.exists(test_destfile))
  }
})
