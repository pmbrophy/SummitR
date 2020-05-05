context("func_expGaussian")

library(DescTools)
test_that("func_expGaussian returns correct data formats when used to generate probability distribution", {
  xVec <- seq(from = 1, to = 100, by = 0.1)

  pdist <- func_expGaussian(x = xVec, mu = 10, sigma = 1, lambda = 0.2, probDensity = TRUE)
  area <- AUC(x = xVec, y = pdist, from = 1, to = 100)

  expect_vector(pdist)
  expect_equal(length(xVec), length(pdist))
  expect_true(area < 1.001 & area > 0.999)
  expect_equal(which.max(pdist), 106)
})

test_that("func_expGaussian returns correct data formats when used to generate exponentially modified gaussian peak", {
  xVec <- seq(from = 1, to = 100, by = 0.1)

  expect_error(func_expGaussian(x = xVec, mu = 10, sigma = 1, lambda = 0.2, probDensity = FALSE))

  gpeak <- func_expGaussian(x = xVec, mu = 10, sigma = 1, lambda = 0.2, probDensity = FALSE, k = 10)

  expect_vector(gpeak)
  expect_equal(length(xVec), length(gpeak))
  expect_equal(which.max(gpeak), 106)
})
