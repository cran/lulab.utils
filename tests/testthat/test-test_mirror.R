test_that("test CRAN mirror works", {
  mirrors= test_mirror(region = 'China')

  # check the result
  expect_s3_class(mirrors, 'data.frame')
  expect_named(mirrors, c('Name', 'URL', 'download_time'))
  expect_true(nrow(mirrors) > 0)
  expect_true(ncol(mirrors) > 0)
})
