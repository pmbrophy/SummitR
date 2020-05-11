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
#' @param k Amplitude of the peak. Only used when `probDensity == FALSE`
#' @param weights weighting vector to augment the gaussian shape profile
#'
#' @return a vector of y-coordinates the same length as x
#' @export
#'
#' @examples
#' xVec <- seq(from = 1, to = 100, by = 0.1)
#' weights <- rep(x = 1, times = length(xVec))
#' weights[490:510] <- 0.1
#' pdist <- func_arbGaussian(x = xVec, mu = 50, sigma = 1, probDensity = TRUE, weights = weights)
#' plot(xVec, pdist)

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
