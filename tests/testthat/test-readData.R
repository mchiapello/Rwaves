x <- readData("../inst/extdata", "ANA")

test_that("multiplication works", {
  expect_equal(sum(is.na(x$File)), 0)
  expect_equal(sum(is.na(x$waveforms)), 0)
  expect_equal(sum(is.na(x$time)), 0)
})

test_that("values", {
  expect_gt(min(x$time), 0)
  expect_lte(max(x$time), 86400)
  expect_equal(min(x$waveforms), 1)
  expect_equal(max(x$waveforms), 99)
})

test_that("dimentions", {
   expect_equal(ncol(x), 3)
})
