#' Calculate Pseudo-Voigt Using Numerical Approximation
#'
#' Calculates the linear combination of gaussian and lorentzian peaks with normalization factor
#'
#' @param x a vector of x-coordinates from which the corrisponding y-coordinates
#'   are calculated
#' @param mus the mean
#' @param sigmas the standard deviation of the gaussian
#' @param gammas the lorentzian scale parameter specifying the half-width at half-maximum (HWHM)
#' @param probDensity Should the function produce a probability density function
#'   `TRUE` a peak `FALSE` with amplitude k? default is `TRUE`.
#' @param ks Amplitude of the peak. Only used when `probDensity == FALSE`
#' @param returnComponentPks Should the function return a single vector
#'   containing the sum of each individual peak `FALSE` or a data.frame
#'   containing the input vector `x`, each component peak `peak_n`, and the
#'   summed result of the component peaks `peak_sum`. The default is FALSE.
#' @return a vector of y-coordinates the same length as x
#' @export
#'
#' @examples
#' #Pseudo-Voigt Probability Distribution from multiple peaks
#' xVec <- seq(from = 1, to = 100, by = 0.1)
#' pdist <- multi_pseudoVoigt(x = xVec,
#'                            mus = c(20, 40, 60),
#'                            sigmas = c(1, 1, 1),
#'                            gammas = c(1,2,1.5),
#'                            probDensity = TRUE,
#'                            returnComponentPks = FALSE)
#'
#' pdist_components <- multi_pseudoVoigt(x = xVec,
#'                                       mus = c(20, 40, 60),
#'                                       sigmas = c(1, 1, 1),
#'                                       gammas = c(1,2,1.5),
#'                                       probDensity = TRUE,
#'                                       returnComponentPks = TRUE)
#'
#' plot(x = xVec, y = pdist)
#' plot(x = pdist_components$x, y = pdist_components$peak_sum)
#' points(x = pdist_components$x, y = pdist_components$peak_1, col = "red")
#' points(x = pdist_components$x, y = pdist_components$peak_2, col = "blue")
#' points(x = pdist_components$x, y = pdist_components$peak_3, col = "green")
#'

multi_pseudoVoigt <- function(x, sigmas, gammas, mus, probDensity, ks, returnComponentPks){
  if(any(probDensity)){
    ks <- 1
  }
  peaks <- mapply(FUN = func_pseudoVoigt,
                  mu = mus,
                  sigma = sigmas,
                  gamma = gammas,
                  probDensity = probDensity,
                  k = ks,
                  MoreArgs = list(x = x),
                  SIMPLIFY = FALSE)

  multiPeak_sum(x = x, peaks = peaks, probDensity = probDensity, returnComponentPks = returnComponentPks)
}
