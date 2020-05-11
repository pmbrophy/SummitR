#' Calculate Log-Normal Distribution(s) or Peak(s)
#'
#' @param x A vector of x-coordinates from which the corrisponding y-coordinates
#'   are calculated
#' @param mus A vector of mean values
#' @param sigmas A vector of standard deviations
#' @param probDensity Should the function produce probability distributions
#'   `TRUE` with integrated area from min(x) to max(x) equal to 1 or produce
#'   log-normal peaks `FALSE` with amplitudes ks? default is `TRUE`.
#' @param ks A vector of amplitudes for the log-normal peaks. Only used when
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
#' #Log-Normal Probability Distribution from multiple peaks
#' xVec <- seq(from = 1, to = 100, by = 0.1)
#' pdist <- multi_logNormal(x = xVec,
#'                          mus = c(20, 40, 60),
#'                          sigmas = c(1, 1, 1),
#'                          probDensity = TRUE,
#'                          returnComponentPks = FALSE)
#'
#' pdist_components <- multi_logNormal(x = xVec,
#'                                     mus = c(20, 40, 60),
#'                                     sigmas = c(1, 1, 1),
#'                                     probDensity = TRUE,
#'                                     returnComponentPks = TRUE)
#'
#' plot(x = xVec, y = pdist)
#' plot(x = pdist_components$x, y = pdist_components$peak_sum)
#' points(x = pdist_components$x, y = pdist_components$peak_1, col = "red")
#' points(x = pdist_components$x, y = pdist_components$peak_2, col = "blue")
#' points(x = pdist_components$x, y = pdist_components$peak_3, col = "green")
#'

multi_logNormal <- function(x, mus, sigmas, probDensity, returnComponentPks = FALSE, ks){
  if(any(probDensity)){
    ks <- 1
  }
  peaks <- mapply(FUN = func_logNormal,
                  mu = mus,
                  sigma = sigmas,
                  probDensity = probDensity,
                  k = ks,
                  MoreArgs = list(x = x),
                  SIMPLIFY = FALSE)

  multiPeak_sum(x = x, peaks = peaks, probDensity = probDensity, returnComponentPks = returnComponentPks)
}
