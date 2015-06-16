#'@title
#' Combination of Distributions by Monte Carlo
#'
#'@description
#' Uncertainty propagation by Monte Carlo (GUM-Supp1 [1]).
#' 
#' Both \code{gumS1(adapt=TRUE)} and \code{gumS2} provide adaptative procedures. 
#' \code{gumS1} implements the code proposed in GUM-Supp1 which checks the 
#' convergence of the estimators by blocks. 
#' \code{gumS2} implements the two-stage Stein procedure [2], which has 
#' been shown to have a less erratic stopping criterion than \code{gumS1}.
#' 
#' By default, \code{gumS1} is non-adaptative and performs the user-specified
#' number of simulations. 
#' 
#' @param fExpr An expression or a function object.
#' @param x.mu Named vector of mean values 
#'                    with names compatible with \code{fExpr}.
#' @param x.u Named vector of standard uncertainty values 
#'                   (one of \{\code{x.u}, \code{x.cov}\} mandatory). 
#' @param x.pdf Named vector of pdf types (see \code{\link{PDFs}}).
#' @param x.df Named vector of degrees of freedom for \code{x.pdf}.
#' @param x.cor Named correlation matrix between model parameters.
#' @param x.cov Named variance/covariance matrix 
#'                     between model parameters (one of \{\code{x.u}, 
#'                     \code{x.cov}\} mandatory).  
#' @param nrun Number of runs in each sample packet.
#' @param h1 Number of packets in first step of \code{gumS2}.
#' @param adapt Flag to use the sequential adaptive method.
#' @param p Coverage of confidence interval.
#' @param stdev Flag for adaptative method to converge standard 
#'              deviation (\code{gumS1}; for \code{gumS2} convergence 
#'              is based on the variance).
#' @param interval Flag to converge confidence interval (\code{gumS1}).
#' @param ndig Number of significant figures to converge. 
#' @param nrunMax Maximum number of runs allowed.
#' @param silent Flag to run without printout.
#' @param delFrac Multiplicative factor on  numerical tolerance. 
#'                For compatibility with examples in GUM-Supp1 [1].
#' #' 
#' @return A list containing:
#' \item{y.mu}{mean value of model}
#' \item{y.u}{standard uncertainty of model}
#' \item{y.low}{lower limit of confidence interval}
#' \item{y.high}{upper limit of confidence interval}
#' \item{p}{coverage of confidence interval (same as input)}
#' \item{X}{(matrix) sample of inputs used to converge statistics}
#' \item{Y}{(vector) sample of outputs corresponding to X}
#' 
#' @references [1] Evaluation of measurement data - Supplement 1 to 
#' the "Guide to the expression of uncertainty in measurement" -  
#' Propagation of distributions using a Monte Carlo method. \emph{JCGM} \strong{101}:2008.
#' \href{http://www.bipm.org/utils/common/documents/jcgm/JCGM_101_2008_E.pdf}{PDF}
#' @references  [2] G. W\"ubbeler, P. M. Harris, M. G. Cox and C. Elster (2010) 
#' A two-stage procedure for determining the number of trials 
#' in the application of a Monte Carlo method for uncertainty evaluation.
#' \emph{Metrologia} \strong{47}:317. 
#' \href{http://dx.doi.org/10.1088/0026-1394/47/3/023}{CrossRef}

#'  
#' @aliases gumS1 gumS2

#' @rdname gumMC
#' @examples
#' fExpr = expression(x1+x2)
#' x.mu = c(1,1); names(x.mu)=c('x1','x2')
#' x.u = c(0.1,0.1); names(x.u)=c('x1','x2')
#' x.pdf = c('unif','triangle'); names(x.pdf)=c('x1','x2')
#' S=gumS1(fExpr,x.mu,x.u,x.pdf,x.df=NULL,nrunMax=1000)
#' pairs(cbind(S$X,S$Y))
#' @export
  
gumS1 = function(fExpr,x.mu,x.u,x.pdf,x.df,x.cor=diag(length(x.mu)),x.cov=NULL, 
                 nrun=1000, adapt=FALSE, ndig=1, p=0.95, delFrac=1, stdev=TRUE, 
                 interval=TRUE, silent=FALSE, nrunMax=1e6) {
  
   if (missing(fExpr)) {
     print('\nGUM Supp.1 Monte Carlo Uncertainty Propagation method for model fExpr',quote=F)
     print(" ",quote=F)     
     print("Call   : gumS1(fExpr, x.mu, x.u, x.pdf, x.df, nrun, adapt,",quote=F)
     print("               stdev, ndig, p, delFrac, interval, silent)",quote=F)
     print(" ",quote=F)
     print("-fExpr : (oblig) an expression or a function object",quote=F)
     print("-x.mu  : (oblig) named vector of mean values",quote=F)
     print("         with names compatible with fExpr",quote=F)
     print("-x.u   : (oblig) named vector of standard uncertainty values",quote=F)
     print("-x.pdf : (oblig) named vector of pdf types (norm, unif, stud...) ",quote=F)
     print("-x.df  : (opt.) named vector of degrees of freedom for x.pdf",quote=F)
     print("-x.cor : (matrix, def=NULL) named correlation matrix between model parameters",quote=F)
     print("-x.cov : (one of {x.u, x.cov} oblig) names variance/covariance matrix",quote=F)    
     print("         between model parameters",quote=F) 
     print("-nrun  : (integ, def=1000) number of runs in each sample packet",quote=F)
     print("-adapt : (logical, def=FALSE) whether use the sequential adaptive method",quote=F)
     print("-ndig  : (integ, def=1) number of significant figures to converge",quote=F)
     print("-p     : (real, def=0.95) coverage of confidence interval",quote=F)
     print("-stdev : (logical, def=TRUE) whether to converge standard deviation",quote=F)
     print("-interval : (logical, def=TRUE) whether to converge confidence interval",quote=F)
     print("-silent: (logical, def=FALSE) wether gumCV executes without printout",quote=F) 
     print("-nrunMaxc: (integ, def=1e6) maximum number of runs allowed",quote=F) 
     print(" ",quote=F)
     print('Returns: list(y.mu, y.u, y.low, y.high, p, X, Y)',quote=F)  
     print("-y.mu  : mean value of model",quote=F)
     print("-y.u   : standard uncertainty of model",quote=F)
     print("-y.low : lower limit of confidence interval",quote=F)
     print("-y.high: upper limit of confidence interval",quote=F)
     print("-p     : coverage of confidence interval (same as input)",quote=F)
     print("-X     : (matrix) sample of inputs used to converge statistics",quote=F)
     print("-Y     : (vector) sample of outputs corresponding to X",quote=F)     
     return(invisible())
   }

   # b) coverage interval probability and number of MC trials
   M=nrun
   if(interval) {
     alpha=(1-p)/2
     if (adapt) M=max(ceiling(100/(1-p)),nrun)
   }     
   
   # c)
   h=1
   yav=c()
   if (stdev) uy=c()
   ytot=c()
   xtot=c()
   if(interval) {
     ylow=c()
     yhigh=c()
   } 
   warnMsg=NULL
   
   repeat {
   
      # d) Model evaluations
      sample=xSample(nrun,x.mu,x.u,x.pdf,x.df,x.cor,x.cov)
      y=fVector(sample,fExpr)
     
      ytot=c(ytot,y) 
      xtot=rbind(xtot,sample)
      if(!adapt) break
      if(length(ytot) > nrunMax) {
        warnMsg = paste0('Warning : max. nb of runs imposed by nrunMax=',nrunMax)
        break
      }
      
      # e) statistics
      yav[h]=mean(y)
      if(stdev) uy[h]=sd(y)
      if(interval) {
        ylow[h]=quantile(y,probs=c(alpha),type=8)
        yhigh[h]=quantile(y,probs=c(p+alpha),type=8)
      }
      
      if (h>=2) {
         ydev=sd(yav)/h^0.5
         if(stdev) uydev=sd(uy)/h^0.5
         if(interval) {
           ylowdev=sd(ylow)/h^0.5
           yhighdev=sd(yhigh)/h^0.5
         }
   
         udev=sd(ytot,na.rm=T)
         del= numtol(udev,ndig) * delFrac
         
#          factor = 2
         factor = qt(0.975,h-1)
         if(interval) {
           if(factor*ydev <= del & 
                factor*uydev <= del & 
                factor*ylowdev <= del & 
                factor*yhighdev <= del ) break 
         } else {
           if(stdev) {
             if(factor*ydev <= del & 
                  factor*uydev <= del  ) break 
           } else {
             if(factor*ydev <= del) break              
           }
         }
      }
      h=h+1
   }
   
   # Output results
   if(!silent) {
     cat('\n*** Monte Carlo Uncertainty Propagation:')
     cat(sprintf("\nSample Size = %1.1e",M*h)) 
     if(!is.null(warnMsg)) cat(paste0("\n",warnMsg))
   }
   
   ym_cv=mean(ytot,na.rm=T)
   uy_cv=sd(ytot,na.rm=T)
   if(!silent) uncPrint(ym_cv,uy_cv)
   
   if(interval) { 
     ns=floor(log10(uy_cv))-1     
     ylow_cv=quantile(ytot,na.rm=T,probs=c(alpha),type=8)
     yhigh_cv=quantile(ytot,na.rm=T,probs=c(p+alpha),type=8)
     short_ylow_cv=round(ylow_cv/10^ns,0)
     short_yhigh_cv=round(yhigh_cv/10^ns,0)
     if(!silent) {
       if(ns==0)
         cat(sprintf("\n%d percent C.I. = [%d,%d]",
                     100.0*p,short_ylow_cv,short_yhigh_cv))
       else
         cat(sprintf("\n%d percent C.I. = [%d,%d]*10^%d",
                     100.0*p,short_ylow_cv,short_yhigh_cv,ns))
     }     
   }
   ytot=matrix(ytot,ncol=1)
   colnames(ytot)='Y'
   return(list(y.mu=ym_cv, y.u=uy_cv, y.low=ylow_cv, y.high=yhigh_cv,
               p = p, Y=ytot,X=xtot))   
}

#' @rdname gumMC
#' @examples
#' S=gumS2(fExpr,x.mu,x.u,x.pdf,x.df=NULL)
#' ECIPlot(S$Y)
#' @export

gumS2 = function(fExpr,x.mu,x.u,x.pdf,x.df,x.cor=diag(length(x.mu)),x.cov=NULL, 
                 nrun=100, h1=30,ndig=1, p=0.95, silent=FALSE, nrunMax=1e6) {
  
  if (missing(fExpr)) {
    print('\nGUM Supp.1 Monte Carlo Uncertainty Propagation method for model fExpr',quote=F)
    print('Adaptive procedure base on two-steps Stein method',quote=F)
    print('cf. [Wubbeler et al. (2010) Metrologia 47:317]',quote=F)
    print(" ",quote=F)     
    print("Call  : gumS2(fExpr, x.mu, x.u, x.pdf, x.df, nrun, h1, ndig , p, silent)",quote=F)
    print("-fExpr : (oblig) an expression or a function object",quote=F)
    print("-x.mu  : (oblig) named vector of mean values",quote=F)
    print("         with names compatible with fExpr",quote=F)
    print("-x.u   : (oblig) named vector of standard uncertainty values",quote=F)
    print("-x.pdf : (oblig) named vector of pdf types (norm, unif, stud...) ",quote=F)
    print("-x.df  : (opt.) named vector of degrees of freedom for x.pdf",quote=F)
    print("-x.cor : (matrix, def=NULL) named correlation matrix between model parameters",quote=F)
    print("-x.cov : (one of {x.u, x.cov} oblig) names variance/covariance matrix",quote=F)    
    print("         between model parameters",quote=F) 
    print("-nrun  : (integ, def=100) number of runs in each sample packet",quote=F)
    print("-h1    : (integ, def=30) number of packets in first step",quote=F)
    print("-ndig  : (integ, def=1) number of significant figures to converge",quote=F)
    print("-p     : (real, def=0.95) coverage of confidence interval",quote=F)
    print("-silent: (logical, def=FALSE) wether gumCV executes without printout",quote=F) 
    print("-nrunMax: (integ, def=1e6) maximum number of runs allowed",quote=F) 
    print(" ",quote=F)
    print('Returns: list(y.mu, y.u, y.low, y.high, p, X, Y)',quote=F)  
    print("-y.mu  : mean value of model",quote=F)
    print("-y.u   : standard uncertainty of model",quote=F)
    print("-y.low : lower limit of confidence interval",quote=F)
    print("-y.high: upper limit of confidence interval",quote=F)
    print("-p     : coverage of confidence interval (same as input)",quote=F)
    print("-X     : (matrix) sample of inputs used to converge statistics",quote=F)
    print("-Y     : (vector) sample of outputs corresponding to X",quote=F)     
    return(invisible())
  }
  
  if(!silent) {
    cat('\n*** Monte Carlo Uncertainty Propagation:\n')
  }

  alpha=(1-p)/2
       
  # c)
  yav=c()
  ytot=c()
  xtot=c()

  # 1rst stage
  if(!silent) cat(paste0('1st stage: ',h1,' blocks\n'))
  for (h in 1:h1) {  
    sample=xSample(nrun,x.mu,x.u,x.pdf,x.df,x.cor,x.cov)
    y=fVector(sample,fExpr)
    ytot=c(ytot,y) 
    xtot=rbind(xtot,sample)
    yav[h]=mean(y)
  }

  # Estimate 2nd stage sampling
  s2y = var(yav)
  udev= sd(ytot,na.rm=T)
  del = numtol(udev,ndig)
  factor = qt(1-alpha/2,h1-1)
  h2 = max(floor(s2y*factor^2/del^2)-h1+1,0)

  warnMsg = NULL
  if((h1+h2)*nrun > nrunMax) {
    h2 = min(h2, floor(nrunMax/nrun)-h1)
    warnMsg = paste0('Warning : max. nb of runs imposed by nrunMax=',nrunMax)
  }
  
  # 2nd stage
  if(!silent) {
    cat(paste0('2nd stage: ',h2,' blocks\n'))
    if(!is.null(warnMsg)) cat(paste0("\n",warnMsg))
  }
  for (h in (h1+1):(h1+h2)) {  
    sample=xSample(nrun,x.mu,x.u,x.pdf,x.df,x.cor,x.cov)
    y=fVector(sample,fExpr)
    ytot=c(ytot,y) 
    xtot=rbind(xtot,sample)
    yav[h]=mean(y)
  }
  h=h1+h2
  
  # Output results
  if(!silent) cat(sprintf("\nSample Size = %1.1e",nrun*h))
  
  ym_cv = mean(ytot,na.rm=T)
  uy_cv = sd(ytot,na.rm=T)
  if(!silent) uncPrint(ym_cv,uy_cv)
  
  ns = floor(log10(uy_cv))-1  
  ylow_cv = quantile(ytot,na.rm=T,probs=c(alpha),type=8)
  yhigh_cv = quantile(ytot,na.rm=T,probs=c(p+alpha),type=8)
  short_ylow_cv = round(ylow_cv/10^ns,0)
  short_yhigh_cv = round(yhigh_cv/10^ns,0)
  if(!silent) {
    if( ns == 0 )
      cat(sprintf("\n%d percent C.I. = [%d,%d]",
                  100.0*p,short_ylow_cv,short_yhigh_cv))
    else
      cat(sprintf("\n%d percent C.I. = [%d,%d]*10^%d",
                  100.0*p,short_ylow_cv,short_yhigh_cv,ns))
  }
  
  ytot = matrix(ytot,ncol=1)
  colnames(ytot) = 'Y'
  return(list(y.mu=ym_cv, y.u=uy_cv, y.low=ylow_cv, y.high=yhigh_cv,
              p = p, Y=ytot, X=xtot))   
}

