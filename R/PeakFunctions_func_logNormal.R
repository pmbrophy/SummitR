#' Calculate Log-Normal Distribution or Log-Gaussian Peak
#'
#' Calculates either a log-normal distribution with an
#' integrated area of 1 or a log-normal peak with amplitude k
#'
#' @param x a vector of x-coordinates from which the corrisponding y-coordinates
#'   are calculated
#' @param mu location of apex
#' @param sigma the standard deviation
#' @param probDensity Should the function produce a probability density function
#'   `TRUE` or a gaussian peak `FALSE` with amplitude k? default is `TRUE`.
#' @param k Amplitude of the peak multiplied by 1/x to form pre-exponential term. Only used when `probDensity == FALSE`
#'
#' @return a vector of y-coordinates the same length as x
#' @export
#'
#' @examples
#' #log-normal distribution
#' xVec <- seq(from = 1, to = 100, by = 0.1)
#' pdensity <- func_logNormal(x = xVec, mu = 10, sigma = 1, probDensity = T)
#' p1 <- plot(x = xVec, y = pdensity)
#'
#' #log-normal peak
#' gpeak <- func_logNormal(x = xVec, mu = 10, sigma = 1, probDensity = F, k = 10)
#' p2 <- plot(x = xVec, y = gpeak)
#'


func_logNormal <- function(x, mu, sigma, probDensity = TRUE, k){
  if(probDensity){
    k <- 1 / (x * sigma * sqrt(2 * pi))
  }else{
    if(missing(k)){
      stop("k not specified")
    }else{
      k <- k * (1 / x)
    }
  }

  #Calculate peak shape
  peak <- k * exp(-0.5 * (((log(x) - log(mu))/sigma)^2))

  peak
}
