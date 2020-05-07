context("func_biGaussian")

library(DescTools)
test_that("func_biGaussian returns correct data formats when used to generate probability distribution", {
  xVec <- seq(from = 1, to = 100, by = 0.1)

  pdist <- func_biGaussian(x = xVec, mu = 10, sigma1 = 1, sigma2 = 2, probDensity = T)
  area <- AUC(x = xVec, y = pdist, from = 1, to = 100)

  expect_vector(pdist)
  expect_equal(length(xVec), length(pdist))
  expect_equal(area, 1)
  expect_named(pdist)
  expect_true(which.max(pdist) == 91)
})

test_that("func_biGaussian returns correct data formats when used to generate gaussian peak", {
  xVec <- seq(from = 1, to = 100, by = 0.1)

  expect_error(func_biGaussian(x = xVec, mu = 10, sigma = 1, probDensity = F))

  gpeak <- func_biGaussian(x = xVec, mu = 10, sigma1 = 1, sigma2 = 2, probDensity = F, k = 10)

  expect_vector(gpeak)
  expect_equal(length(xVec), length(gpeak))
  expect_named(gpeak)
  expect_true(which.max(gpeak) == 91)
})
