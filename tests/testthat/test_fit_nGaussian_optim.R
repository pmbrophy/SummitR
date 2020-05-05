context("fit_nGaussian_optim")

test_that("fit_nGaussian_optim returns correct data types", {
  xVec <- seq(from = 1, to = 100, by = 0.1)
  gauss <- multi_gaussian(x = xVec, mus = c(10, 20, 30), sigmas = c(1, 1, 1), probDensity = FALSE, k = c(10, 15, 20))

  gauss <- gauss + rnorm(n = length(gauss), mean = 0, sd = 1)

  fit <- fit_nGaussian_optim(x = xVec, y = gauss, init_mus = c(9, 21, 32), constant_sigma =  1, init_ks = c(11, 12, 13), maxit = 10000)

  expect_type(fit, "list")
  expect_named(fit)
  expect_equal(length(fit$fitPeak), length(xVec))
})
