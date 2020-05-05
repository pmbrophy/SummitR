#' Calculate local minimum within a local neighborhood
#'
#' @param vec a vector to be evaluated for the presence of local minimum
#' @param neighborhood number of points to the left and right of a point for
#'   calculating local minima (see details).
#'
#' @return a logical vector masking the positions of local minima
#'
#' @export
#'
#' @details `neighborhood = 1` will detect local minima at position `i` defind
#'   as a point that is less than both its neighbors to the left `(i-1)` and
#'   right `(i+1)`. Setting `neighborhood > 1` results in detecting local minima
#'   at a position `i` defined as a point that is less than all neighboring
#'   points to the left `c(i-1, i-..., i-n)` and right `c(i+1, i+..., i+n)`.
#'
#' @examples
#' dat <- rnorm(n = 100, mean = 0, sd = 1)
#' i <- seq_along(dat)
#' locMin <- localMin(vec = dat, neighborhood = 1)
#'
#' i_locMin <- i[locMin]
#' locMin <-dat[locMin]
#'
#' plot(x = i, y = dat); points(x = i_locMin, y = locMin, col = "red")
#'

localMin <- function(vec, neighborhood){
  vecLength <- length(vec)
  neighborhoodSteps <- c(1:neighborhood)

  isGreater_R <- sapply(FUN = rightShift_less,
                        X = neighborhoodSteps,
                        vec = vec,
                        vecLength = vecLength)

  isGreater_L <- sapply(FUN = leftShift_less,
                        X = neighborhoodSteps,
                        vec = vec,
                        vecLength = vecLength)

  Rfast::rowAll(cbind(isGreater_R, isGreater_L))

}

#' Check X-right side neighbors are less
#'
#' @param X a integer vector of sequence c(1:neighborhood)
#' @param vec the vector of values to be evaluated
#' @param vecLength the length of vec
#'
#' @return logical vector padded with `FALSE` values on the end
#'

rightShift_less <- function(X, vec, vecLength){
  c(vec[1:(vecLength - X)] < vec[(1+X):vecLength], rep(x = F, times = X))
}

#' Check X-left side neighbors are less
#'
#' @param X a integer vector of sequence c(1:neighborhood)
#' @param vec the vector of values to be evaluated
#' @param vecLength the length of vec
#'
#' @return logical vector padded with `FALSE` values on the front
#'

leftShift_less <- function(X, vec, vecLength){
  c(rep(x = F, times = X), vec[(1+X):vecLength] < vec[1:(vecLength - X)])
}
