context("func_lorentzian")

library(DescTools)
test_that("func_lorentzian returns correct data formats when used to generate probability distribution", {
  xVec <- seq(from = 1, to = 1000, by = 0.1)

  pdist <- func_lorentzian(x = xVec, x0 = 500, gamma = 1, probDensity = TRUE)
  area <- AUC(x = xVec, y = pdist, from = 1, to = 100)

  expect_vector(pdist)
  expect_equal(length(xVec), length(pdist))
  expect_true(area > 0.99 & area < 1.01)
  expect_true(which.max(pdist) == 4991)
})

test_that("func_lorentzian returns correct data formats when used to generate Lorentzian peak", {
  xVec <- seq(from = 1, to = 1000, by = 0.1)

  expect_error(func_lorentzian(x = xVec, x0 = 10, gamma = 1, probDensity = FALSE))

  gpeak <- func_lorentzian(x = xVec, x0 = 500, gamma = 1, probDensity = FALSE, k = 10)

  expect_vector(gpeak)
  expect_equal(length(xVec), length(gpeak))
  expect_equal(max(gpeak), 10)
  expect_equal(which.max(gpeak), 4991)
})
