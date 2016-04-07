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

#'@param y Mean value.
#'@param uy Standard uncertainty on \code{Y}. Should be strictly positive.
#'@param p Coverage of confidence interval.
#'@param fac Enlargment factor corresponding to \code{p}.
#'@param y_low,y_high Lower and upper limits of coverage interval.
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
  if (uy <= 0) {
    cat('Sorry: cannot pretty print with negative or null uncertainty !\n')
    return
  }
  ns=floor(log10(uy))+1
  short_y=round(y/10^ns,2)
  short_uy=round(uy/10^ns,2)
  cat('\n')
  if(ns==0)
    cat(sprintf("Y = %.2f +/- %.2f", short_y, short_uy))
  else
    if(ns <= 2 & ns > 0)
      if(ns == 1)
        cat(sprintf("Y = %.1f +/- %.1f", short_y*10^ns, short_uy*10^ns))
      else
        cat(sprintf("Y = %.0f +/- %.0f", short_y*10^ns, short_uy*10^ns))
    else  
      cat(sprintf("Y = (%.2f +/- %.2f)*10^%d", short_y, short_uy, ns))
}

#' @rdname gumPrint
#' @examples
#' #
#' CIPrint(S$y.mu,S$y.u)
#' @export
CIPrint = function(y, uy, p=0.95, fac=1.96) {
  # Controlled precision output (2 digits for uy 
  # and truncate y accordingly)
  if (uy <= 0) {
    cat('Sorry: cannot pretty print with negative or null uncertainty !\n')
    return
  }
  ns=floor(log10(fac*uy))+1
  short_ylow_cv =round((y-fac*uy)/10^ns,2)
  short_yhigh_cv=round((y+fac*uy)/10^ns,2)
  if(ns==0)
    cat(sprintf("\n%d percent C.I. = [%.2f, %.2f]",
                100.0*p,short_ylow_cv,short_yhigh_cv))
    if(ns <= 2 & ns > 0)
      if(ns == 1)
        cat(sprintf("\n%d percent C.I. = [%.1f, %.1f]",
                    100.0*p,short_ylow_cv*10^ns,short_yhigh_cv*10^ns))
      else
        cat(sprintf("\n%d percent C.I. = [%.0f, %.0f]",
                    100.0*p,short_ylow_cv*10^ns,short_yhigh_cv*10^ns))
    else  
      cat(sprintf("\n%d percent C.I. = [%.2f, %.2f]*10^%d",
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
  if (uy <= 0) {
    cat('Sorry: cannot pretty print with negative or null uncertainty !\n')
    return
  }
  ns=floor(log10(uy))+1
  short_ylow_cv =round((y_low)/10^ns,2)
  short_yhigh_cv=round((y_high)/10^ns,2)
  if(ns==0)
    cat(sprintf("\n%d percent C.I. = [%.2f, %.2f]",
                100.0*p,short_ylow_cv,short_yhigh_cv))
  if(ns <= 2 & ns > 0)
    if(ns == 1)
      cat(sprintf("\n%d percent C.I. = [%.1f, %.1f]",
                  100.0*p,short_ylow_cv*10^ns,short_yhigh_cv*10^ns))
    else
      cat(sprintf("\n%d percent C.I. = [%.0f, %.0f]",
                100.0*p,short_ylow_cv*10^ns,short_yhigh_cv*10^ns))
  else  
    cat(sprintf("\n%d percent C.I. = [%.2f, %.2f]*10^%d",
                100.0*p,short_ylow_cv,short_yhigh_cv,ns))
}