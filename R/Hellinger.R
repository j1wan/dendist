#' Hellinger Distance
#'
#' Calcuate Hellinger Distance between two probability density functions.
#' @param P y-values of a distribution
#' @param Q y-values of another distribution
#' @param dx delta x
#' @seealso \code{\link{BhattacharyyaDist}} \code{\link{KLDiv}} \code{\link{JSDiv}} \code{\link{JSDist}}
#' @export
HellingerDist <- function(P, Q, dx) {
  return(sqrt(1 - BhattacharyyaCoef(P, Q, dx)))
}
