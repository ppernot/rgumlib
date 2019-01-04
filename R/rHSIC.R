#'@title
#' HSIC SA index
#'
#'@description
#' Hilbert-Schmidt Independence Criterion based SA index 
#' 
#' @param x Vector.
#' @param y Vector.
#
#' @rdname rHSIC
#' @export 
rHSIC = function(x,y) {
  dHSIC::dhsic(x,y)$dHSIC /
    sqrt( dHSIC::dhsic(x,x)$dHSIC *
            dHSIC::dhsic(y,y)$dHSIC  )
}