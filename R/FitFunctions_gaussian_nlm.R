#' Fit a Gaussian Peak Using Non-Linear Least Squares
#'
#' @param x values of the x-axis
#' @param y values of the y-axis
#' @param init_mu initial guess of mu (the peak center)
#' @param init_sigma initial guess of sigma (the standard deviation)
#' @param init_k initial guess of k (the peak height)
#'
#' @return a list containing a vector `fitPeak` containing the y-values of the
#'   fitted peak and a list `nlsResult` containing the data returned from nlm()
#' @export
#'
#' @details Implementation uses the nls() function to fit a gaussian peak to
#'   experimental data using the nl2sol algorithm from the Port library - see
#'   ?nls() and https://people.sc.fsu.edu/~jburkardt/f_src/minpack/minpack.html
#'
#' @examples
#'  xVec <- seq(from = 1, to = 100, by = 0.1)
#'  gauss <- func_gaussian(x = xVec, mu = 10, sigma = 1, probDensity = FALSE, k = 10)
#'  gauss <- gauss + rnorm(n = length(gauss), mean = 0, sd = 1)
#'  fit <- fit_gaussian_nlm(x = xVec, y = gauss, init_mu = 9, init_sigma = 0.5, init_k = 3)
#'  plot(x = xVec, y = gauss); points(x = xVec, y = fit$fitPeak, col = "red")
#'

#TODO: ensure starting value of guesses is within range of the parameter limits...
fit_gaussian_nlm <- function(x, y, init_mu, init_sigma, init_k){
  #Construct data.frame
  df <- data.frame("x" = x, "y" = y)

  #initial parameters
  initial_par <- c("mu" = init_mu, "sigma" = init_sigma, "k" = init_k)

  #limits on parameters
  lower_par <- c("mu" = 0, "sigma" = 0, "k" = 0)

  #Do the fit
  result <- stats::nls(formula = y ~ k*exp(-0.5*((x-mu)/sigma)^2),
                       data = df,
                       start = initial_par,
                       lower = lower_par,
                       algorithm = "port")

  #Extract information
  result_coefs <- stats::coef(result)

  peak <- func_gaussian(x = x,
                        mu = result_coefs["mu"],
                        sigma = result_coefs["sigma"],
                        probDensity = FALSE,
                        k = result_coefs["k"])

  list(fitPeak = peak, nlsResult = list("coef" = result_coefs, convInfo = result$convInfo))
}
