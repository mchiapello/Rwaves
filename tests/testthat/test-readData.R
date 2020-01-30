library(Rwaves)

test_that("multiplication works", {
  expect_equal(sum(is.na(original$File)), 0)
  expect_equal(sum(is.na(original$waveforms)), 0)
  expect_equal(sum(is.na(original$time)), 0)
})

test_that("values", {
  expect_gt(min(original$time), 0)
  expect_lte(max(original$time), 86400)
  expect_equal(min(original$waveforms), 1)
  expect_equal(max(original$waveforms), 99)
})

test_that("dimentions", {
   expect_equal(ncol(original), 3)
})


