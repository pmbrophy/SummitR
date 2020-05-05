#' Calculate Normal Distribution or Gaussian Peak
#'
#' Calculates either a probability density function using the Fraser-Suzuki
#' model with an integrated area of 1 or a peak with amplitude k
#'
#' @param x a vector of x-coordinates from which the corrisponding y-coordinates
#'   are calculated
#' @param mu location of apex
#' @param sigma the standard deviation
#' @param a parameter controling tailing. If a > 0, model produces peak tails. If a < 0, model
#'   produces fronting peaks
#' @param probDensity Should the function produce a probability density function
#'   `TRUE` or a gaussian peak `FALSE` with amplitude k? default is `TRUE`.
#' @param k Amplitude of the peak. Only used when `probDensity == FALSE`
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
func_fraserSuzuki <- function(x, mu, sigma, a, probDensity = TRUE, k){
  if(probDensity){
    k <- 1/(sqrt(2 * pi) * sigma * exp((a^2) / (4 * log(2))))
  }else{
    if(missing(k)){
      stop("k not specified")
    }
  }

  if(a == 0){
    stop("a cannot equal 0")
  }

  #Calculate fractional term and check for valid ranges
  frac <- (a * (x - mu))/sigma
  valid_x <- which(frac > -1)
  frac <- frac[valid_x]

  peak <- vector(mode = "numeric", length = length(x))

  peak[valid_x] <- k * exp(-0.5 * (1/a^2) * log(1 + frac)^2)

  peak
}
