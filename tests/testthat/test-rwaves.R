library(Rwaves)

f <- system.file("extdata", package = "Rwaves")
x <- readData(f, estention = "ANA")
pr <- rwaves(x)

test_that("consistencpr", {
  expect_gte(min(pr$f1_1), 0)
  expect_gte(min(pr$f2_1), 0)
  expect_gte(min(pr$f3_1), 0)
  expect_gte(min(pr$f14), 0)
  expect_gte(min(pr$f24), 0)
  expect_gte(min(pr$f2_2), 0)
  expect_gte(min(pr$f2_6), 0)
  expect_gte(min(pr$f1_7), 0)
  expect_gte(min(pr$f2_7), 0)
  expect_gte(min(pr$f115_2), 0)
  expect_gte(min(pr$f115_6), 0)
  expect_gte(min(pr$f115_7), 0)
})



