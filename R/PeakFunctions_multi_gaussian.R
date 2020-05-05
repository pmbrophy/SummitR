#' Calculate Normal Distribution(s) or Gaussian Peak(s)
#'
#' @param x A vector of x-coordinates from which the corrisponding y-coordinates
#'   are calculated
#' @param mus A vector of mean values
#' @param sigmas A vector of standard deviations
#' @param probDensity Should the function produce probability distributions
#'   `TRUE` with integrated area from min(x) to max(x) equal to 1 or produce
#'   gaussian peaks `FALSE` with amplitudes ks? default is `TRUE`.
#' @param ks A vector of amplitudes for the gaussian peaks. Only used when
#'   `probDensity == TRUE`
#' @param returnComponentPks Should the function return a single vector
#'   containing the sum of each individual peak `FALSE` or a data.frame
#'   containing the input vector `x`, each component peak `peak_n`, and the
#'   summed result of the component peaks `peak_sum`
#'
#' @return returns either a single vector of y-coordinates the same length as x or a data.frame
#' @export
#'
#' @examples
#' #Exponentially Modified Gaussian Probability Distribution from multiple peaks
#' xVec <- seq(from = 1, to = 100, by = 0.1)
#' pdist <- multi_gaussian(x = xVec, mus = c(5, 10, 15), sigmas = c(1, 2, 4), probDensity = TRUE, returnComponentPks = FALSE)
#' pdist_components <- multi_gaussian(x = xVec, mus = c(5, 10, 15), sigmas = c(1, 2, 4), probDensity = TRUE, returnComponentPks = TRUE)
#' p1 <- plot(x = xVec, y = pdist)
#' p2 <- plot(x = pdist_components$x, y = pdist_components$peak_sum);points(x = pdist_components$x, y = pdist_components$peak_1, col = "red");points(x = pdist_components$x, y = pdist_components$peak_2, col = "blue");points(x = pdist_components$x, y = pdist_components$peak_3, col = "green")

multi_gaussian <- function(x, mus, sigmas, probDensity, returnComponentPks = FALSE, ks){
  if(any(probDensity)){
    ks <- 1
  }
  peaks <- mapply(FUN = func_gaussian,
                  mu = mus,
                  sigma = sigmas,
                  probDensity = probDensity,
                  k = ks,
                  MoreArgs = list(x = x),
                  SIMPLIFY = FALSE)

  multiPeak_sum(x = x, peaks = peaks, probDensity = probDensity, returnComponentPks = returnComponentPks)
}
