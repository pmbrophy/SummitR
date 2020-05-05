context("localMax")

test_that("localMax returns correct values for detectig local maxima when neighborhood = 1",{
  maxTests <- c(1,0,1,0,1,0,0,0,1)
  maxima <- localMax(vec = maxTests, neighborhood = 1)

  expect_vector(maxima)
  expect_type(maxima, "logical")
  expect_true(maxima[3])
  expect_true(maxima[5])
  expect_false(maxima[1])
  expect_false(maxima[length(maxima)])
})

test_that("localMax returns correct values for detectig local maxima when neighborhood = 1",{
  maxTests <- c(1,0,1,0,1,0,0,0,1)
  maxima <- localMax(vec = maxTests, neighborhood = 2)

  expect_false(maxima[3])
  expect_false(maxima[5])
  expect_false(maxima[1])
  expect_false(maxima[length(maxima)])
})
