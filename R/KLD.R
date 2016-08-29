#' Kullback-Leibler divergence
#'
#' calculate K-L divergence w/ trapezodial approximation
#' @aliases KLD
#' @param P y-values of a distribution
#' @param Q y-values of another distribution
#' @param dx delta x
#' @export
KLDiv <- function(P, Q, dx) {
  n <- length(P)
  R <- P * log(P / Q)
  R[P==0] <- 0
  trapezoid <- (R[1:(n-1)] + R[2:n]) / 2
  return(sum(trapezoid) * dx)
}

