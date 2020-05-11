#' Calculate Pseudo-Voigt Using Numerical Approximation
#'
#' Calculates the linear combination of gaussian and lorentzian peaks with normalization factor
#'
#' @param x a vector of x-coordinates from which the corrisponding y-coordinates
#'   are calculated
#' @param mu the mean
#' @param sigma the standard deviation of the gaussian
#' @param gamma the lorentzian scale parameter specifying the half-width at half-maximum (HWHM)
#' @param probDensity Should the function produce a probability density function
#'   `TRUE` a peak `FALSE` with amplitude k? default is `TRUE`.
#' @param k Amplitude of the peak. Only used when `probDensity == FALSE`
#'
#' @return a vector of y-coordinates the same length as x
#' @export
#'
#' @examples
#' xVec <- seq(from = 1, to = 100, by = 0.1)
#' pdensity <- func_pseudoVoigt(x = xVec, mu = 10, sigma = 1, gamma = 1.5, probDensity = TRUE)
#' p1 <- plot(x = xVec, y = pdensity)
#'
#' #Pseudo-Voigt Peak
#' pdensity <- func_pseudoVoigt(x = xVec, mu = 10, sigma = 1, gamma = 1.5, probDensity = FALSE, k = 10)
#' p2 <- plot(x = xVec, y = pdensity)

func_pseudoVoigt <- function(x, sigma, gamma, mu, probDensity, k){
  #Calculate gaussian and lorentzian peaks
  g <- func_gaussian(x = x, mu = mu, sigma = sigma, probDensity = probDensity, k = k)
  l <- func_lorentzian(x = x, x0 = mu, gamma = gamma, probDensity = probDensity, k = k)

  #FWHMs
  fwhm_g <- 2 * sqrt(2 * log(2)) * sigma
  fwhm_l <- 2 * gamma

  #FWHM of pseudo Voigt
  f <- (fwhm_g^5) + (2.69269 * (fwhm_g^4) * fwhm_l) + (2.42843 * (fwhm_g^3) * (fwhm_l^2)) + (4.47163 * (fwhm_g^2) * (fwhm_l^3)) + (0.07842 * (fwhm_g) * (fwhm_l^4))
  f <- f^(1/5)

  nu <- (1.36603 * (fwhm_l/f)) - (0.47719 * (fwhm_l/f)^2) + (0.11116 * (fwhm_l/f)^3)

  #nu is only valid between 0 and 1
  if(nu < 0 | nu > 1){
    stop(paste0("nu must be between 0 and 1. nu =", nu))
  }

  peak <- (nu * l) + ((1 - nu) * g)
  peak
}
