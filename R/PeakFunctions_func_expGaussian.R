#' Calculate an Exponentially Modified Gaussian Probability Density or Peak
#'
#' @param x a vector of x-coordinates from which the corrisponding y-coordinates
#'   are calculated
#' @param mu the mean of the gaussian component
#' @param sigma the standard deviation of the gaussian component
#' @param lambda rate parameter of the exponential component
#' @param probDensity Should the function return a probability density
#'   `TRUE` or an exponentially modified gaussian peak `FALSE` with amplitude k? default is `TRUE`.
#' @param k Amplitude of the gaussian peak. Only used when `probDensity == TRUE`
#'
#' @return a vector of y-coordinates the same length as x
#' @export
#'
#' @examples
#' #Probability density of exponentially modified gaussian
#' xVec <- seq(from = 1, to = 100, by = 0.1)
#' pdist <- func_expGaussian(x = xVec, mu = 10, sigma = 1, lambda = 0.2, probDensity = TRUE)
#' plot(x = xVec, y = pdist)
#'
#' #Exponentially modified gaussian peak
#' gpeak <- func_expGaussian(x = xVec, mu = 10, sigma = 1, lambda = 0.2, probDensity = FALSE, k = 10)
#' plot(x = xVec, y = gpeak)
#'

func_expGaussian <- function(x, mu, sigma, lambda, probDensity = TRUE, k){
  if(probDensity){
    k <- (lambda/2)
  }else{
    if(missing(k)){
      stop("k not specified")
    }
  }

  #Exponential Terms
  A <- 2*mu + (lambda*sigma^2) - 2*x

  #Complementary error function terms
  erfc_numerator <- mu + (lambda*sigma^2) - x
  erfc_denomenator <- sqrt(2)*sigma
  A_erfc <- erfc_numerator/erfc_denomenator

  #Calculate
  k*exp((lambda/2)*A)*pracma::erfc(A_erfc)
}
