context("multiPeak_sum")

test_that("multiPeak_sum returns the correctly formatted data", {
  xVec <- c(1:100)
  yVec <- 2*c(1:100)
  peaks <- list(yVec, yVec*2)

  pdensity <- multiPeak_sum(x = xVec, peaks = peaks, probDensity = TRUE, returnComponentPks = FALSE)
  expect_vector(pdensity)
  expect_length(pdensity, 100)

  pdensity_comp <- multiPeak_sum(x = xVec, peaks = peaks, probDensity = TRUE, returnComponentPks = TRUE)
  expect_named(pdensity_comp)
  expect_length(pdensity_comp, 4)

  peak <- multiPeak_sum(x = xVec, peaks = peaks, probDensity = FALSE, returnComponentPks = FALSE)
  expect_vector(pdensity)
  expect_length(pdensity, 100)
  expect_true(all(pdensity/peak == 0.5))

  peak_comp <- multiPeak_sum(x = xVec, peaks = peaks, probDensity = FALSE, returnComponentPks = TRUE)
  expect_named(pdensity_comp)
  expect_length(pdensity_comp, 4)
})
