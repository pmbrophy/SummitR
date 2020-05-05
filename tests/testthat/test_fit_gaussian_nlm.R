context("fit_gaussian_nlm")

test_that("fit_gaussian_nlm returns correct data types", {
  xVec <- seq(from = 1, to = 100, by = 0.1)
  gauss <- func_gaussian(x = xVec, mu = 10, sigma = 1, probDensity = FALSE, k = 10)

  gauss <- gauss + rnorm(n = length(gauss), mean = 0, sd = 1)

  fit <- fit_gaussian_nlm(x = xVec, y = gauss, init_mu = 9, init_sigma = 0.5, init_k = 3)

  expect_type(fit, "list")
  expect_named(fit)
  expect_equal(length(fit$fitPeak), length(xVec))
})
