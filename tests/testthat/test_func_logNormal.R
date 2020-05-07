context("func_logNormal")

library(DescTools)
test_that("func_logNormal returns correct data formats when used to generate probability distribution", {
  xVec <- seq(from = 1, to = 100, by = 0.1)

  pdist <- func_logNormal(x = xVec, mu = 20, sigma = 0.2, probDensity = TRUE)
  area <- AUC(x = xVec, y = pdist, from = 1, to = 100)

  expect_vector(pdist)
  expect_equal(length(xVec), length(pdist))
  expect_equal(area, 1)
  expect_equal(which.max(pdist), 183)
})

test_that("func_logNormal returns correct data formats when used to generate gaussian peak", {
  xVec <- seq(from = 1, to = 100, by = 0.1)

  expect_error(func_logNormal(x = xVec, mu = 20, sigma = 0.2, probDensity = FALSE))

  gpeak <- func_logNormal(x = xVec, mu = 20, sigma = 0.2, probDensity = F, k = 10)

  expect_vector(gpeak)
  expect_equal(length(xVec), length(gpeak))
  expect_equal(which.max(gpeak), 183)
})
