#' Calculate Bi-Gaussian Distribution(s) or Bi-Gaussian Peak(s)
#'
#' @param x A vector of x-coordinates from which the corrisponding y-coordinates
#'   are calculated
#' @param mus A vector of mean values
#' @param sigma1s A vector of standard deviations for the left side of the peaks
#' @param sigma2s A vector of standard deviations for the right side of the
#'   peaks
#' @param probDensity Should the function produce probability distributions
#'   `TRUE` with integrated area from min(x) to max(x) equal to 1 or produce
#'   gaussian peaks `FALSE` with amplitudes ks? default is `TRUE`.
#' @param ks A vector of amplitudes for the gaussian peaks. Only used when
#'   `probDensity == FALSE`
#' @param returnComponentPks Should the function return a single vector
#'   containing the sum of each individual peak `FALSE` or a data.frame
#'   containing the input vector `x`, each component peak `peak_n`, and the
#'   summed result of the component peaks `peak_sum`. The default is FALSE.
#'
#' @return returns either a single vector of y-coordinates the same length as x
#'   or a data.frame
#' @export
#'
#' @examples
#' #Exponentially Modified BiGaussian Probability Distribution from multiple peaks
#' xVec <- seq(from = 1, to = 100, by = 0.1)
#' pdist <- multi_biGaussian(x = xVec,
#'                           mus = c(5, 10, 15),
#'                           sigma1s = c(1, 2, 4),
#'                           sigma2s = c(2,2,2),
#'                           probDensity = TRUE,
#'                           returnComponentPks = FALSE)
#' pdist_components <- multi_biGaussian(x = xVec,
#'                                      mus = c(5, 10, 15),
#'                                      sigma1s = c(1, 2, 4),
#'                                      sigma2s = c(2,2,2),
#'                                      probDensity = TRUE,
#'                                      returnComponentPks = TRUE)
#' plot(x = xVec, y = pdist)
#'
#' plot(x = pdist_components$x, y = pdist_components$peak_sum)
#' points(x = pdist_components$x, y = pdist_components$peak_1, col = "red")
#' points(x = pdist_components$x, y = pdist_components$peak_2, col = "blue")
#' points(x = pdist_components$x, y = pdist_components$peak_3, col = "green")
#'

multi_biGaussian <- function(x, mus, sigma1s, sigma2s, probDensity, returnComponentPks = FALSE, ks){
  if(any(probDensity)){
    ks <- 1
  }
  peaks <- mapply(FUN = func_biGaussian,
                  mu = mus,
                  sigma1 = sigma1s,
                  sigma2 = sigma2s,
                  probDensity = probDensity,
                  k = ks,
                  MoreArgs = list(x = x),
                  SIMPLIFY = FALSE)

  multiPeak_sum(x = x, peaks = peaks, probDensity = probDensity, returnComponentPks = returnComponentPks)
}
