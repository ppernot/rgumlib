#'@title
#' The Arcsine Distribution
#'
#'@description
#' Density, distribution function, quantile function and random generation for
#' the arcsine distribution on the interval from \code{a} to \code{b}. 
#' 
#' @param x,q Vector of quantiles.
#' @param p Vector of probabilities.
#' @param n Number of observations.
#' @param a,b Lower and upper limits of the distribution. Must be finite.
#' 
#' @aliases parcsine qarcsine rarcsine 
#' @examples
#' darcsine(seq(0,1,by=0.1))
#' @export
darcsine = function(x, a=0, b=1) dbeta((x-a)/(b-a),0.5,0.5)

#' @rdname darcsine
#' @examples
#' parcsine(seq(-pi,pi,by=0.1), a=-pi, b=pi)
#' @export
parcsine = function(q, a=0, b=1) pbeta((q-a)/(b-a),0.5,0.5)

#' @rdname darcsine
#' @examples
#' qarcsine(seq(0,1,by=0.1), a=-pi, b=pi)
#' @export
qarcsine = function(p, a=0, b=1) a + (b-a)*qbeta(p,0.5,0.5)

#' @rdname darcsine
#' @examples
#' rarcsine(10)
#' @export
rarcsine = function(n, a=0, b=1) a + (b-a)*rbeta(n,0.5,0.5)
