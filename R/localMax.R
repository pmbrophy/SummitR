#' Calculate local maximum within a local neighborhood
#'
#' @param vec a vector to be evaluated for the presence of local maxima
#' @param neighborhood number of points to the left and right of a point for
#'   calculating local maxima (see details).
#'
#' @return a logical vector masking the positions of local maxima
#'
#' @export
#'
#' @details `neighborhood = 1` will detect local maxima at position `i` defind
#'   as a point that is greater than both its neighbors to the left `(i-1)` and
#'   right `(i+1)`. Setting `neighborhood > 1` results in detecting local maxima
#'   at a position `i` defined as a point that is greater than all neighboring
#'   points to the left `c(i-1, i-..., i-n)` and right `c(i+1, i+..., i+n)`.
#'
#' @examples
#' dat <- rnorm(n = 100, mean = 0, sd = 1)
#' i <- seq_along(dat)
#' locMax <- localMax(vec = dat, neighborhood = 1)
#'
#' i_locMax <- i[locMax]
#' locMax <-dat[locMax]
#'
#' plot(x = i, y = dat); points(x = i_locMax, y = locMax, col = "red")
#'

localMax <- function(vec, neighborhood){
  vecLength <- length(vec)
  neighborhoodSteps <- c(1:neighborhood)

  isGreater_R <- sapply(FUN = rightShift_greater,
                        X = neighborhoodSteps,
                        vec = vec,
                        vecLength = vecLength)

  isGreater_L <- sapply(FUN = leftShift_greater,
                        X = neighborhoodSteps,
                        vec = vec,
                        vecLength = vecLength)

  Rfast::rowAll(cbind(isGreater_R, isGreater_L))
}



#' Check X-right side neighbors are greater
#'
#' @param X a integer vector of sequence c(1:neighborhood)
#' @param vec the vector of values to be evaluated
#' @param vecLength the length of vec
#'
#' @return logical vector padded with `FALSE` values on the end
#'

rightShift_greater <- function(X, vec, vecLength){
  c(vec[1:(vecLength - X)] > vec[(1+X):vecLength], rep(x = FALSE, times = X))
}

#' Check X-left side neighbors are greater
#'
#' @param X a integer vector of sequence c(1:neighborhood)
#' @param vec the vector of values to be evaluated
#' @param vecLength the length of vec
#'
#' @return logical vector padded with `FALSE` values on the front
#'

leftShift_greater <-function(X, vec, vecLength){
  c(rep(x = F, times = X), vec[(1+X):vecLength] > vec[1:(vecLength - X)])
}
