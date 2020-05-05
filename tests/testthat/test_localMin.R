context("localMin")

test_that("localMin returns correct values for detectig local minima when neighborhood = 1",{
  minTests <- c(-1,0,-1,0,-1,0,0,0,-1)
  minima <- localMin(vec = minTests, neighborhood = 1)

  expect_vector(minima)
  expect_type(minima, "logical")
  expect_true(minima[3])
  expect_true(minima[5])
  expect_false(minima[1])
  expect_false(minima[length(minima)])
})

test_that("localMin returns correct values for detectig local minima when neighborhood = 2",{
  minTests <- c(-1,0,-1,0,-1,0,0,0,-1)
  minima <- localMin(vec = minTests, neighborhood = 2)

  expect_false(minima[3])
  expect_false(minima[5])
  expect_false(minima[1])
  expect_false(minima[length(minima)])
})
