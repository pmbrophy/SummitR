context("func_pseudoVoigt")

library(DescTools)
test_that("func_pseudoVoigt returns correct data formats when used to generate probability distribution", {
  xVec <- seq(from = 1, to = 100, by = 0.1)

  pdist <- func_pseudoVoigt(x = xVec, mu = 50, sigma = 1, gamma = 1, probDensity = TRUE)
  area <- AUC(x = xVec, y = pdist, from = 1, to = 100)

  expect_vector(pdist)
  expect_equal(length(xVec), length(pdist))
  expect_true(area > 0.99 & area < 1)
  expect_equal(which.max(pdist), 491)
})

test_that("func_pseudoVoigt returns correct data formats when used to generate gaussian peak", {
  xVec <- seq(from = 1, to = 100, by = 0.1)

  expect_error(func_pseudoVoigt(x = xVec, mu = 50, sigma = 1, gamma = 1, probDensity = FALSE))

  gpeak <- func_pseudoVoigt(x = xVec, mu = 50, sigma = 1, gamma = 1, probDensity = FALSE, k = 10)

  expect_vector(gpeak)
  expect_equal(length(xVec), length(gpeak))
  expect_equal(which.max(gpeak), 491)
})
