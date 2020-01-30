library(Rwaves)

test_that("dimentions", {
  expect_equal(nrow(y), x %>% dplyr::count(File) %>% nrow)
  expect_equal(ncol(y), 14)
})

test_that("consistency", {
  expect_gte(min(y$f1_1), 0)
  expect_gte(min(y$f2_1), 0)
  expect_gte(min(y$f3_1), 0)
  expect_gte(min(y$f14), 0)
  expect_gte(min(y$f24), 0)
  expect_gte(min(y$f2_2), 0)
  expect_gte(min(y$f2_6), 0)
  expect_gte(min(y$f1_7), 0)
  expect_gte(min(y$f2_7), 0)
  expect_gte(min(y$f115_2), 0)
  expect_gte(min(y$f115_6), 0)
  expect_gte(min(y$f115_7), 0)
})
