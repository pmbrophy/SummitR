#' Fit N-Gaussian Peaks Using quasi-Newtonian optomization method with boxed constraints
#'
#' @param x values of the x-axis
#' @param y values of the y-axis
#' @param init_mus initial guess of mu (the peak center)
#' @param init_ks initial guess of k (the peak height)
#' @param constant_sigma a constand value for sigma to be applied to each of the N-peaks
#' @param maxit maximum number of iterations (default = 1000)
#'
#' @returna list containing a vector of y-values of the fitted peak called
#'   `fitPeak` and a list named `optimResult` containing the results from
#'   optim()
#'
#' @export
#' @examples
#' xVec <- seq(from = 1, to = 100, by = 0.1)
#' gauss <- multi_gaussian(x = xVec, mus = c(10, 20, 30), sigmas = c(1, 1, 1), probDensity = FALSE, k = c(10, 15, 20))
#' gauss <- gauss + rnorm(n = length(gauss), mean = 0, sd = 1)
#' fit <- fit_nGaussian_optim(x = xVec, y = gauss, init_mus = c(9, 21, 32), constant_sigma =  1, init_ks = c(11, 12, 13), maxit = 10000)
#' plot(x = xVec, y = gauss); points(x = xVec, y = gauss$fitPeak, col = "red)
#'

fit_nGaussian_optim <- function(x, y, init_mus, init_ks, constant_sigma, maxit = 1000){
  #Length of par will be equal to sum(length(init_mus)+length(init_ks))
  #Format of par assumed to be par = c(mu1, mu2, mu3, ... mun, k1, k2, k3 ... kn)
  f <- function(par){
    parLength <- length(par)

    #Centers
    mus <- par[seq(from = 1, to = parLength/2, by = 1)]

    #Heights
    ks <- par[seq(from = (1 + (parLength/2)), to = parLength, by = 1)]

    #Constant Widths
    sigmas <- rep(x = constant_sigma, times = parLength/2)

    #Probability density option (FALSE)
    probDensity <- rep(x = FALSE, times = parLength/2)

    #Calculate the peak
    yhat <- multi_gaussian(x = x, mus = mus, sigmas = sigmas, probDensity = probDensity, ks = ks)

    #Least squares residual of data (y) and fit (yhat)
    sum((y-yhat)^2)
    #rmse(y = y, yhat = yhat)
  }

  #initial parameters: c(mu1, mu2, mu3, ... mun, k1, k2, k3 ... kn)
  initial_par <- c(init_mus, init_ks)

  #Lower parameter limits
  lower_par <- rep(x = 0, times = length(initial_par))

  #Do the fit
  result <- optim(par = initial_par,
                  fn = f,
                  method = "L-BFGS-B",
                  lower = c(0,0,0),
                  control = list("maxit" = maxit))

  #Make the resutant peak
  numParams <- length(result$par)
  peak <- multi_gaussian(x = x,
                         mus = result$par[1:(numParams/2)],
                         sigmas = rep(x = constant_sigma, times = numParams/2),
                         probDensity = rep(x = F, times = numParams/2),
                         ks = result$par[(1+(numParams/2)):numParams])

  #Return Data
  result <- list(fitPeak = peak, optimResult = result)
  result
}
