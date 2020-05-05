#' Calculate Normal Distribution or Gaussian Peak
#'
#' Calculates either a normal distribution similar to [dnorm()] with an
#' integrated area of 1 or a gaussian peak with amplitude k
#'
#' @param x a vector of x-coordinates from which the corrisponding y-coordinates
#'   are calculated
#' @param mu the mean
#' @param sigma the standard deviation
#' @param probDensity Should the function produce a probability density function
#'   `TRUE` or a gaussian peak `FALSE` with amplitude k? default is `TRUE`.
#' @param k Amplitude of the gaussian peak. Only used when `probDensity == TRUE`
#' @param weights weighting vector to augment the gaussian shape profile
#'
#' @return a vector of y-coordinates the same length as x
#' @export
#'
#' @examples
#' #normal distribution
#' xVec <- seq(from = 1, to = 100, by = 0.1)
#' pdensity <- func_gaussian(x = xVec, mu = 10, sigma = 1, probDensity = T)
#' p1 <- plot(x = xVec, y = pdensity)
#'
#' #gaussian peak
#' gpeak <- func_gaussian(x = xVec, mu = 10, sigma = 1, probDensity = F, k = 10)
#' p2 <- plot(x = xVec, y = gpeak)
#'
#' #See also: dnorm()
func_arbGaussian <- function(x, mu, sigma, probDensity, k, weights){
  if(probDensity){
    k <- (1/(sigma*sqrt(2*pi)))
  }else{
    if(missing(k)){
      stop("k not specified")
    }
  }
  if(missing(weights)){
    stop("weights not specified")
  }
  if(length(weights) != length(x)){
    stop("Weighting vector must be same length as x")
  }

  peak <- k * exp(-0.5 * ((x-mu)/sigma)^2)
  peak*weights
}
