#' Fit a Gaussian Peak Using quasi-Newtonian optomization method with boxed
#' constraints
#'
#' @param x values of the x-axis
#' @param y values of the y-axis
#' @param init_mu initial guess of mu (the peak center)
#' @param init_sigma initial guess of sigma (the standard deviation)
#' @param init_k initial guess of k (the peak height)
#' @param maxit maximum number of iterations (default = 1000)
#'
#' @return a list containing a vector of y-values of the fitted peak called
#'   `fitPeak` and a list named `optimResult` containing the results from
#'   optim()
#'
#' @export
#'
#' @examples
#' xVec <- seq(from = 1, to = 100, by = 0.1)
#' gauss <- func_gaussian(x = xVec, mu = 10, sigma = 1, probDensity = FALSE, k = 10)
#' gauss <- gauss + rnorm(n = length(gauss), mean = 0, sd = 1)
#' fit <- fit_gaussian_optim(x = xVec,
#'                           y = gauss,
#'                           init_mu = 9,
#'                           init_sigma = 0.5,
#'                           init_k = 3,
#'                           maxit = 1000)
#' plot(x = xVec, y = gauss); points(x = xVec, y = fit$fitPeak, col = "red")
#'
fit_gaussian_optim <- function(x, y, init_mu, init_sigma, init_k, maxit = 1000){
  #Function to be optomized
  f <- function(par){
    #center
    mu <- par[1]
    #width
    sigma <- par[2]
    #height
    k <- par[3]

    #current fit
    #yhat <- k * exp(-0.5 * ((x - mu)/sigma)^2)
    yhat <- func_gaussian(x = x, mu = mu, sigma = sigma, probDensity = FALSE, k = k)

    #Residual of data (y) and fit (yhat)
    #rmse(y = y, yhat = yhat)
    sum((y-yhat)^2)
  }

  #initial parameters
  initial_par <- c("mu" = init_mu, "sigma" = init_sigma, "k" = init_k)

  #Do the fit
  result <- stats::optim(par = initial_par,
                         fn = f,
                         method = "L-BFGS-B",
                         lower = c(0,0,0),
                         control = list("maxit" = maxit))

  #Make the resutant peak
  peak <- func_gaussian(x = x,
                        mu = result$par["mu"],
                        sigma = result$par["sigma"],
                        probDensity = FALSE,
                        k = result$par["k"])

  #Return Data
  result <- list(fitPeak = peak, optimResult = result)

  result
}
