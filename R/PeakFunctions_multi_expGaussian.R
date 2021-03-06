#' Calculate Exponentially Modified Gaussian Probability Density(s) or Peak(s)
#'
#' @param x a vector of x-coordinates from which the corrisponding y-coordinates
#'   are calculated
#' @param mus the means of the gaussian components
#' @param sigmas the standard deviations of the gaussian components
#' @param lambdas rate parameters of the exponential components
#' @param probDensity Should the function produce a probability density
#'   `TRUE` or exponentially modified gaussian peaks `FALSE` with amplitudes ks? default is `TRUE`.
#' @param ks Amplitudes of the gaussian peaks. Only used when `probDensity == TRUE`
#' @param returnComponentPks Should the function return a single vector
#'   containing the sum of each individual peak `FALSE` or a data.frame
#'   containing the input vector `x`, each component peak `peak_n`, and the
#'   summed result of the component peaks `peak_sum`. The default is FALSE.
#' @return returns either a single vector of y-coordinates the same length as x or a data.frame
#' @export
#'
#' @examples
#' xVec <- seq(from = 1, to = 100, by = 0.1)
#' pdist <- multi_expGaussian(x = xVec,
#'                            mus = c(5, 10, 15),
#'                            sigmas = c(1, 2, 4),
#'                            lambdas = c(0.1, 0.1, 0.2),
#'                            probDensity = TRUE,
#'                            returnComponentPks = FALSE)
#' pdist_components <- multi_expGaussian(x = xVec,
#'                                       mus = c(5, 10, 15),
#'                                       sigmas = c(1, 2, 4),
#'                                       lambdas = c(0.1, 0.1, 0.2),
#'                                       probDensity = TRUE,
#'                                       returnComponentPks = TRUE)
#' plot(x = xVec, y = pdist)
#' plot(x = pdist_components$x, y = pdist_components$peak_sum)
#' points(x = pdist_components$x, y = pdist_components$peak_1, col = "red")
#' points(x = pdist_components$x, y = pdist_components$peak_2, col = "blue")
#' points(x = pdist_components$x, y = pdist_components$peak_3, col = "green")
#'

multi_expGaussian <- function(x, mus, sigmas, lambdas, probDensity, ks, returnComponentPks = FALSE){
  if(probDensity){
    ks <- 1
  }

  peaks <- mapply(FUN = func_expGaussian,
                  mu = mus,
                  sigma = sigmas,
                  lambda = lambdas,
                  k = ks,
                  probDensity = probDensity,
                  MoreArgs = list(x = x),
                  SIMPLIFY = FALSE)

  multiPeak_sum(x = x, peaks = peaks, probDensity = probDensity, returnComponentPks = returnComponentPks)
}
