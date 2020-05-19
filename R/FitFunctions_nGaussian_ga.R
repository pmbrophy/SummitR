#' Fit N-Gaussian Peaks Using genetic algorithm
#'
#' @param x values of the x-axis to be fit
#' @param y values of the y-axis to be fit
#' @param init_mus initial guess of mu (the peak center) - either a single value to be applied globally or a value specifying each peak
#' @param init_ks initial guess of k (the peak height)
#' @param init_sigmas
#' @param lower_mus
#' @param lower_ks
#' @param lower_sigmas
#' @param upper_mus
#' @param upper_ks
#' @param upper_sigmas
#' @param maxit
#'
#' @return
#' @export
#'
#' @examples
fit_nGaussian_ga <- function(x, y, init_mus, init_ks, init_sigmas, lower_mus, lower_ks, lower_sigmas, upper_mus, upper_ks, upper_sigmas, maxit = 1000){
  #Length of par will be equal to sum(length(init_mus)+length(init_ks))
  #Format of par assumed to be par = c(mu1, mu2, mu3, ... mun, k1, k2, k3 ... kn)
  f <- function(par){
    parLength <- length(par)

    #Centers
    mus <- par[seq(from = 1, to = parLength/3, by = 1)]


    #Heights
    ks <- par[seq(from = (1 + (parLength/3)), to = (2 * parLength)/3, by = 1)]

    #Widths
    sigmas <- par[seq(from = (4 + (parLength/3)), to = parLength, by = 1)]

    #Probability density option (FALSE)
    probDensity <- rep(x = FALSE, times = parLength/3)

    #Calculate the peak
    yhat <- multi_gaussian(x = x, mus = mus, sigmas = sigmas, probDensity = probDensity, ks = ks)

    #Least squares residual of data (y) and fit (yhat)
    -sum((y-yhat)^2)
    #rmse(y = y, yhat = yhat)
  }

  #Initial Guess Parameter Checks and Setup
  nMus <- length(init_mus)
  nKs <- length(init_ks)
  nSigmas <- length(init_sigmas)
  nPeaks <- max(nMus, nKs, nSigmas)
  paramSeq <- c(1:nPeaks)

  #Check initial mu values
  if(nMus == 1){
    init_mus <- rep(init_mus, times = nPeaks)
  }else if(nMus != nPeaks){
    stop("number of initial mu values is not 1 and not equal to the number of peaks")
  }
  #Check initial k values
  if(nKs == 1){
    init_ks <- rep(init_ks, times = nPeaks)
  }else if(nKs != nPeaks){
    stop("number of initial k values is not 1 and not equal to the number of peaks")
  }
  #Check initial k values
  if(nSigmas == 1){
    init_sigmas <- rep(init_sigmas, times = nPeaks)
  }else if(nSigmas != nPeaks){
    stop("number of initial sigma values is not 1 and not equal to the number of peaks")
  }

  #initial parameters: c(mu1, mu2, mu3, ... mun, k1, k2, k3 ... kn)
  initial_par <- c(init_mus, init_ks, init_sigmas)
  names(initial_par) <- c(paste0("mu_", paramSeq), paste0("k_", paramSeq), paste0("sigma_", paramSeq))

  #Lower Limit Parameters
  nLowerMus <- length(lower_mus)
  nLowerKs <- length(lower_ks)
  nLowerSigmas <- length(lower_sigmas)

  if(nLowerMus == 1){
    lower_mus <- rep(lower_mus, times = nPeaks)
  }else if(nLowerMus != nPeaks){
    stop("number of initial lower mu values is not 1 (global lower limit for mu) and not equal to the number of peaks")
  }
  if(nLowerKs == 1){
    lower_ks <- rep(lower_ks, times = nPeaks)
  }else if(nLowerKs != nPeaks){
    stop("number of initial lower k values is not 1 (global lower limit for k) and not equal to the number of peaks")
  }
  if(nLowerSigmas == 1){
    lower_sigmas <- rep(lower_sigmas, times = nPeaks)
  }else if(nLowerSigmas != nPeaks){
    stop("number of initial lower sigma values is not 1 (global lower limit for sigma) and not equal to the number of peaks")
  }

  lowerLimits <- c(lower_mus, lower_ks, lower_sigmas)
  names(lowerLimits) <- c(paste0("lower_mu_", paramSeq), paste0("lower_k_", paramSeq), paste0("lower_sigma_", paramSeq))

  #Upper Limit Parameters
  nUpperMus <- length(upper_mus)
  nUpperKs <- length(upper_ks)
  nUpperSigmas <- length(upper_sigmas)

  if(nUpperMus == 1){
    upper_mus <- rep(upper_mus, times = nPeaks)
  }else if(nUpperMus != nPeaks){
    stop("number of initial upper mu values is not 1 (global upper limit for mu) and not equal to the number of peaks")
  }
  if(nUpperKs == 1){
    upper_ks <- rep(upper_ks, times = nPeaks)
  }else if(nUpperKs != nPeaks){
    stop("number of initial upper k values is not 1 (global upper limit for k) and not equal to the number of peaks")
  }
  if(nUpperSigmas == 1){
    upper_sigmas <- rep(upper_sigmas, times = nPeaks)
  }else if(nUpperSigmas != nPeaks){
    stop("number of initial upper sigma values is not 1 (global upper limit for sigma) and not equal to the number of peaks")
  }

  upperLimits <- c(upper_mus, upper_ks, upper_sigmas)
  names(upperLimits) <- c(paste0("upper_mu_", paramSeq), paste0("upper_k_", paramSeq), paste0("upper_sigma_", paramSeq))

  #Do the fit
  result <- GA::ga(type = "real-valued",
                   fitness = f,
                   lower = lowerLimits,
                   upper = upperLimits,
                   crossover = "gabin_spCrossover",
                   optim = FALSE,
                   keepBest = TRUE)

  solution_params <- as.vector(result@solution)

  mu_i <- 1
  mu_f <- nPeaks
  k_i <- mu_f + 1
  k_f <- k_i + nPeaks - 1
  sig_i <- k_f + 1
  sig_f <- sig_i + nPeaks - 1

  peak <- multi_gaussian(x = x,
                         mus = solution_params[mu_i:mu_f],
                         sigmas = solution_params[sig_i:sig_f],
                         probDensity = FALSE,
                         ks = solution_params[k_i:k_f],
                         returnComponentPks = FALSE)

  list(peak = peak, gaResults = result)
}
