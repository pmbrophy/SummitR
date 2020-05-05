#' Calculate Cauchy (Lorentz) probability density function(s) or Lorentzian Peak(s)
#'
#' @param x a vector of x-coordinates from which the corrisponding y-coordinates
#'   are calculated
#' @param x0s location parameter specifying the locations of the peaks of the distribution
#' @param gammas scale parameter specifying the half-widths at half-maximum (HWHM)
#' @param probDensity Should the function produce a probability density function
#'   `TRUE` or a gaussian peak `FALSE` with amplitude k? default is `TRUE`.
#' @param returnComponentPks Should the function return a single vector
#'   containing the sum of each individual peak `FALSE` or a data.frame
#'   containing the input vector `x`, each component peak `peak_n`, and the
#'   summed result of the component peaks `peak_sum`
#' @param ks amplitude of the peak at location x0
#'
#' @return returns either a single vector of y-coordinates the same length as x or a data.frame
#' @export
#'
#' @examples
#' xVec <- seq(from = 1, to = 100, by = 0.1)
#' pdist <- multi_lorentzian(x = xVec, x0s = c(5, 10, 15), gammas = c(1, 2, 4), probDensity = TRUE, returnComponentPks = FALSE)
#' p1 <- plot(x = xVec, y = pdist)
#' lpeak_components <- multi_lorentzian(x = xVec, x0s = c(5, 10, 15), gammas = c(1, 2, 4), probDensity = FALSE, returnComponentPks = TRUE, ks = c(1, 5, 10))
#' p2 <- plot(x = lpeak_components$x, y = lpeak_components$peak_sum);points(x = lpeak_components$x, y = lpeak_components$peak_1, col = "red");points(x = lpeak_components$x, y = lpeak_components$peak_2, col = "blue");points(x = lpeak_components$x, y = lpeak_components$peak_3, col = "green")
#'

multi_lorentzian <- function(x, x0s, gammas, probDensity, returnComponentPks = FALSE, ks){
  if(probDensity){
    ks <- 1
  }

  peaks <- mapply(FUN = func_lorentzian,
                  x0 = x0s,
                  gamma = gammas,
                  k = ks,
                  probDensity = probDensity,
                  MoreArgs = list(x = x),
                  SIMPLIFY = FALSE)

  multiPeak_sum(x = x, peaks = peaks, probDensity = probDensity, returnComponentPks = returnComponentPks)
}
