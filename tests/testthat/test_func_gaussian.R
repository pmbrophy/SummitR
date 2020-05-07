context("func_gaussian")

library(DescTools)
test_that("func_gaussian returns correct data formats when used to generate probability distribution", {
  xVec <- seq(from = 1, to = 100, by = 0.1)

  pdist <- func_gaussian(x = xVec, mu = 10, sigma = 1, probDensity = TRUE)
  area <- AUC(x = xVec, y = pdist, from = 1, to = 100)

  expect_vector(pdist)
  expect_equal(length(xVec), length(pdist))
  expect_equal(area, 1)
  expect_true(which.max(pdist) == 91)
})

test_that("func_gaussian returns correct data formats when used to generate gaussian peak", {
  xVec <- seq(from = 1, to = 100, by = 0.1)

  expect_error(func_gaussian(x = xVec, mu = 10, sigma = 1, probDensity = FALSE))

  gpeak <- func_gaussian(x = xVec, mu = 10, sigma = 1, probDensity = FALSE, k = 10)

  expect_vector(gpeak)
  expect_equal(length(xVec), length(gpeak))
  expect_equal(which.max(gpeak), 91)
})
