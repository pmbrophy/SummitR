#' Calculate Cauchy (Lorentz) probability density function or a Lorentzian Peak
#'
#' @param x a vector of x-coordinates from which the corrisponding y-coordinates
#'   are calculated
#' @param x0 location parameter specifying the location of the peak of the distribution
#' @param gamma scale parameter specifying the half-width at half-maximum (HWHM)
#' @param probDensity Should the function produce a probability density function
#'   `TRUE` or a gaussian peak `FALSE` with amplitude k? default is `TRUE`.
#' @param k amplitude of the peak at location x0
#'
#' @return a vector of y-coordinates the same length as x
#' @export
#'
#' @examples
#' #Probability density function
#' xVec <- seq(from = 1, to = 100, by = 0.1)
#' pdist <- func_lorentzian(x = xVec, x0 = 50, gamma = 1, probDensity = TRUE)
#' plot(x = xVec, y = pdist)
#' #Make a peak
#' lpeak <- func_lorentzian(x = xVec, x0 = 50, gamma = 1, probDensity = FALSE, k = 10)
#' plot(x = xVec, y = lpeak)

func_lorentzian <- function(x, x0, gamma, probDensity = TRUE, k){
  if(probDensity){
    #aka. Caunchy probability density function
    k <- (1/(pi*gamma))
  }else{
    if(missing(k)){
      stop("k not specified")
    }
  }

  denomenator <- ((x - x0)^2) + (gamma^2)

  k*((gamma^2)/denomenator)
}
