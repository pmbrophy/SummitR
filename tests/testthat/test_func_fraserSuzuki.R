context("func_fraserSuzuki")

library(DescTools)
test_that("func_fraserSuzuki returns correct data formats when used to generate probability distribution a > 0", {
  xVec <- seq(from = 1, to = 100, by = 0.1)

  pdist <- func_fraserSuzuki(x = xVec, mu = 10, sigma = 1, a = 1, probDensity = T)
  area <- AUC(x = xVec, y = pdist, from = 1, to = 100)

  expect_vector(pdist)
  expect_equal(length(xVec), length(pdist))
  #expect_equal(area, 1)
  expect_true(which.max(pdist) == 91)
})

test_that("func_fraserSuzuki returns correct data formats when used to generate probability distribution with a < 0", {
  xVec <- seq(from = 1, to = 100, by = 0.1)

  pdist <- func_fraserSuzuki(x = xVec, mu = 10, sigma = 1, a = -1, probDensity = T)
  area <- AUC(x = xVec, y = pdist, from = 1, to = 100)

  expect_vector(pdist)
  expect_equal(length(xVec), length(pdist))
  #expect_equal(area, 1)
  expect_true(which.max(pdist) == 91)
})

test_that("func_fraserSuzuki returns correct data formats when used to generate gaussian peak", {
  xVec <- seq(from = 1, to = 100, by = 0.1)

  expect_error(func_fraserSuzuki(x = xVec, mu = 10, sigma = 1, a =1, probDensity = F))

  gpeak <- func_fraserSuzuki(x = xVec, mu = 10, sigma = 1, a= 1, probDensity = F, k = 10)

  expect_vector(gpeak)
  expect_equal(length(xVec), length(gpeak))
  expect_true(which.max(gpeak) == 91)
})
