#' Formatting/housekeeping function for PeakFunctions output
#'
#' @param x a vector of x-coordinates from which the corrisponding y-coordinates
#'   are calculated
#' @param peaks the peak or peaks to be summed
#' @param probDensity Should the function produce a probability density function
#'   `TRUE` or a gaussian peak `FALSE` with amplitude k? default is `TRUE`.
#' @param returnComponentPks Should the function return a single vector
#'   containing the sum of each individual peak `FALSE` or a data.frame
#'   containing the input vector `x`, each component peak `peak_n`, and the
#'   summed result of the component peaks `peak_sum`
#'
#' @return either a vector or data.frame
#'
#' @examples

multiPeak_sum <- function(x, peaks, probDensity, returnComponentPks){
  numPeaks <- length(peaks)
  sumPeaks <- do.call(what = rbind, peaks)
  sumPeaks <- apply(X = sumPeaks, MARGIN = 2, FUN = sum)

  #OPTION: Normalize the sum such that integrated area == 1
  if(any(probDensity)){
    sumPeaks <- sumPeaks/numPeaks
  }

  #OPTION: Return constituant peaks and sum as data.frame
  if(returnComponentPks){
    peaks <- do.call(what = cbind, peaks)
    if(any(probDensity)){
      peaks <- peaks/3
    }

    sumPeaks <- data.frame(x, peaks, sumPeaks)
    names(sumPeaks) <- c("x", paste0("peak_", c(1:numPeaks)), "peak_sum")
  }
  sumPeaks
}
