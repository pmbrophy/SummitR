#' Calculate a Bi-Gaussian Probability Density or Peak
#'
#' @param x a vector of x-coordinates from which the corrisponding y-coordinates
#'   are calculated
#' @param mu the center of the peak
#' @param sigma1 the standard deviation of the left side gaussian component
#' @param sigma2 the standard deviation of the right side gaussian component
#' @param probDensity Should the function return a probability density `TRUE` or
#'   an exponentially modified gaussian peak `FALSE` with amplitude k? default
#'   is `TRUE`.
#' @param k Amplitude of the peak. Only used when `probDensity == FALSE`
#'
#' @return a vector of y-coordinates the same length as x with names "1" and "2"
#'   corrisponding to the two gaussian components used to generate the curve
#' @export
#'
#' @examples
#' #Probability density of exponentially modified gaussian
#' xVec <- seq(from = 1, to = 100, by = 0.1)
#' pdist <- func_biGaussian(x = xVec, mu = 10, sigma1 = 1, sigma2 = 2, probDensity = TRUE)
#' plot(x = xVec, y = pdist, col = names(yVec))
#'
#' #Exponentially modified gaussian peak
#' gpeak <- func_biGaussian(x = xVec, mu = 10, sigma1 = 1, sigma2 = 2, probDensity = FALSE, k = 10)
#' plot(x = xVec, y = gpeak, col = names(yVec))
#'

func_biGaussian <- function(x, mu, sigma1, sigma2, probDensity = TRUE, k){
  if(probDensity){
    k <- sqrt(2 / pi) / (sigma1 + sigma2)
  }else{
    if(missing(k)){
      stop("k not specified")
    }
  }
  #Location of last index along x for calculating peak1
  x_lessThanMu <- which(x < mu)
  x_mu_diff <- abs(x[x_lessThanMu] - mu)
  x_muLoc <- x_lessThanMu[which.min(x_mu_diff)]

  x_length <- length(x)
  peak <- vector(mode = "numeric", length = x_length)

  #calculate values
  peak[1:x_muLoc] <- k * exp(-0.5 * ((x[1:x_muLoc]-mu)/sigma1)^2)
  peak[(x_muLoc + 1):x_length] <- k * exp(-0.5 * ((x[(x_muLoc + 1):x_length]-mu)/sigma2)^2)

  #Set names for tracking
  names(peak) <- c(rep(x = "1", times = x_muLoc), rep(x = "2", times = (x_length-x_muLoc)))

  peak
}
