#' Bhattacharyya Distance
#'
#' Calcuate Bhattacharyya Distance between two probability density functions.
#' @aliases BhattacharyyaCoef
#' @param P y-values of a distribution
#' @param Q y-values of another distribution
#' @param dx delta x
#' @seealso \code{\link{KLDiv}} \code{\link{JSDiv}} \code{\link{JSDist}}
#' @export
BhattacharyyaDist <- function(P, Q, dx) {
  return(-log(BhattacharyyaCoef(P, Q, dx), base = exp(1)))
}

#' @export
BhattacharyyaCoef <- function(P, Q, dx) {
  return(sum(sqrt(P * Q) * dx))
}
