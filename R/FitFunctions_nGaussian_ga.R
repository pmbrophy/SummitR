#' Fit N-Gaussian Peaks Using genetic algorithm
#'
#' @param x values of the x-axis
#' @param y values of the y-axis
#' @param init_mus initial guess of mu (the peak center)
#' @param init_ks initial guess of k (the peak height)
#' @param constant_sigma a constand value for sigma to be applied to each of the N-peaks
#' @param maxit maximum number of iterations (default = 1000)
#'
#' @return a list containing a vector of y-values of the fitted peak called
#'   `fitPeak` and a list named `optimResult` containing the results from
#'   optim()
#'
#' @export
#' @examples
#' xVec <- seq(from = 1, to = 100, by = 0.1)
#'

fit_nGaussian_ga <- function(x, y, init_mus, init_ks, constant_sigma, maxit = 1000){
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
    -sum((y-yhat)^2)
    #rmse(y = y, yhat = yhat)
  }

  #initial parameters: c(mu1, mu2, mu3, ... mun, k1, k2, k3 ... kn)
  initial_par <- c(init_mus, init_ks)

  #Lower parameter limits
  lower_par <- rep(x = 0, times = length(initial_par))

  #Do the fit
  result <- GA::ga(type = "real-valued",
                   fitness = f,
                   lower = c(1, 1, 1, 1, 0, 0, 0, 0),
                   upper = c(50, 50, 50, 50, 40, 40, 40, 40),
                   crossover = "gabin_spCrossover",
                   optim = TRUE,
                   keepBest = TRUE)

  solution_params <- as.vector(result@solution)
  peak <- multi_gaussian(x = x, mus = solution_params[1:4], sigmas = 2, probDensity = FALSE, ks = solution_params[5:8], returnComponentPks = FALSE)

  list(peak, result)
}
