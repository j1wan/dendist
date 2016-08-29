#' Jensen-Shannon Divergence/Distance
#'
#' Calcuate Jensen-Shannon divergence/distance
#' @aliases JSD JSDist
#' @param P y-values of a distribution
#' @param Q y-values of another distribution
#' @param dx delta x
#' @seealso \code{\link{KLDiv}} \code{\link{BhattacharyyaDist}}
#' @export
JSDiv <- function(P, Q, dx) {
  M <- (P + Q) / 2
  D <- (KLDiv(P, M, dx) + KLDiv(Q, M, dx)) / 2
  if (is.na(D)) {
    return(NA)
  } else if (D < 0) { # floating number fix
    return(0)
  } else {
    return(D)
  }
}

#' @export
JSDist <- function(P, Q, dx) {
  return(sqrt(JSDiv(P, Q, dx)))
}

