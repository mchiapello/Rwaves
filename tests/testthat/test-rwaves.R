library(Rwaves)

test_that("dimentions", {
  expect_equal(nrow(processed), original %>% dplyr::count(File) %>% nrow)
  expect_equal(ncol(processed), 14)
})

test_that("consistencprocessed", {
  expect_gte(min(processed$f1_1), 0)
  expect_gte(min(processed$f2_1), 0)
  expect_gte(min(processed$f3_1), 0)
  expect_gte(min(processed$f14), 0)
  expect_gte(min(processed$f24), 0)
  expect_gte(min(processed$f2_2), 0)
  expect_gte(min(processed$f2_6), 0)
  expect_gte(min(processed$f1_7), 0)
  expect_gte(min(processed$f2_7), 0)
  expect_gte(min(processed$f115_2), 0)
  expect_gte(min(processed$f115_6), 0)
  expect_gte(min(processed$f115_7), 0)
})
