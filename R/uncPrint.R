#'@title Printing Functions
#'
#'@description Print results with prescribed number of
#'digits.
#'
#'\code{uncPrint()} is used to print the mean value and
#'(enlarged) standard uncertainty.
#'\code{CIPrint()} is used to print coverage intervals 
#'from mean value, standard uncertainty and coverage factor.
#'\code{CIPrint1()} is used to print coverage intervals 
#'from interval limits.

#'@param \code{y} mean value
#'@param \code{uy} standard uncertainty on \code{y}
#'@param \code{p} coverage of confidence interval
#'@param \code{fac} enlargment factor corresponding to \code{p}
#'@param \code{y_low,y_high} lower and upper limits of coverage interval
#'  
#'@return A character string.

#'  
#' @aliases uncPrint CIPrint CIPrint1

#' @rdname gumPrint
#' @examples
#' fExpr = expression(x1+x2)
#' x.mu = c(1,1); names(x.mu)=c('x1','x2')
#' x.u = c(0.1,0.1); names(x.u)=c('x1','x2')
#' x.pdf = c('unif','triangle'); names(x.pdf)=c('x1','x2')
#' S=gumS1(fExpr,x.mu,x.u,x.pdf,x.df=NULL,nrunMax=1000)
#' uncPrint(S$y.mu,S$y.u)
#' @export
uncPrint = function(y, uy) {
  # Controlled precision output (2 digits for uy 
  # and truncate y accordingly)
  ns=floor(log10(uy))
  short_y=round(y/10^ns,1)
  short_uy=round(uy/10^ns,1)
  cat('\n')
  if(ns==0)
    cat(sprintf("Y = %.1f +/- %.1f", short_y, short_uy))
  else
    cat(sprintf("Y = (%.1f +/- %.1f)*10^%d", short_y, short_uy, ns))
}

#' @rdname gumPrint
#' @examples
#' #
#' CIPrint(S$y.mu,S$y.u)
#' @export
CIPrint = function(y, uy, p=0.95, fac=1.96) {
  # Controlled precision output (2 digits for uy 
  # and truncate y accordingly)
  ns=floor(log10(fac*uy))
  short_ylow_cv =round((y-fac*uy)/10^ns,1)
  short_yhigh_cv=round((y+fac*uy)/10^ns,1)
  if(ns==0)
    cat(sprintf("\n%d percent C.I. = [%.1f,%.1f]",
                100.0*p,short_ylow_cv,short_yhigh_cv))
  else
    cat(sprintf("\n%d percent C.I. = [%.1f,%.1f]*10^%d",
                100.0*p,short_ylow_cv,short_yhigh_cv,ns))
}

#' @rdname gumPrint
#' @examples
#' #
#' CIPrint1(S$y.u,S$y.low,S$y.high)
#' @export
CIPrint1 = function(uy, y_low, y_high, p=0.95) {
  # Controlled precision output (2 digits for uy 
  # and truncate y accordingly)
  ns=floor(log10(uy))
  short_ylow_cv =round((y_low)/10^ns,1)
  short_yhigh_cv=round((y_high)/10^ns,1)
  if(ns==0)
    cat(sprintf("\n%d percent C.I. = [%.1f,%.1f]",
                100.0*p,short_ylow_cv,short_yhigh_cv))
  else
    cat(sprintf("\n%d percent C.I. = [%.1f,%.1f]*10^%d",
                100.0*p,short_ylow_cv,short_yhigh_cv,ns))
}
