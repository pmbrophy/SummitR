context("multi_logNormal")

test_that("multi_logNormal returns correct data formats when used to generate probability density function", {
  xVec <- seq(from = 1, to = 100, by = 0.1)

  pdist <- multi_logNormal(x = xVec, mus = c(5, 10, 15), sigmas = c(1, 2, 4), probDensity = TRUE, returnComponentPks = FALSE)
  pdist_components <- multi_logNormal(x = xVec, mus = c(5, 10, 15), sigmas = c(1, 2, 4), probDensity = TRUE, returnComponentPks = TRUE)


  expect_vector(pdist)
  expect_true(is.data.frame(pdist_components))
  expect_equal(length(xVec), length(pdist))
  expect_true(all(pdist == pdist_components$peak_sum))
})

test_that("multi_logNormal returns correct data formats when used to generate log-normal peaks", {
  xVec <- seq(from = 1, to = 100, by = 0.1)

  expect_error(multi_logNormal(x = xVec, mus = c(5, 10, 15), sigmas = c(1, 2, 4), probDensity = FALSE, returnComponentPks = FALSE))
  expect_error(multi_logNormal(x = xVec, mus = c(5, 10, 15), sigmas = c(1, 2, 4), probDensity = FALSE, returnComponentPks = TRUE))

  gpeak <- multi_logNormal(x = xVec, mus = c(5, 10, 15), sigmas = c(1, 2, 4), probDensity = FALSE, returnComponentPks = FALSE, ks = c(1, 5, 10))
  gpeak_components <- multi_logNormal(x = xVec, mus = c(5, 10, 15), sigmas = c(1, 2, 4), probDensity = FALSE, returnComponentPks = TRUE, ks = c(1, 5, 10))

  expect_vector(gpeak)
  expect_true(is.data.frame(gpeak_components))
  expect_equal(length(xVec), length(gpeak))
  expect_true(all(gpeak == gpeak_components$peak_sum))
})
