#'@title
#' Generate Sample
#'
#'@description
#' Generate Monte Carlo sample from prescribed PDF. 
#' All PDFs are parameterized by their mean and standard deviation.
#'
#' @param \code{M} sample size 
#' @param \code{x.mu} named vector of mean values 
#'                    with names compatible with \code{fExpr}
#' @param \code{x.u} named vector of standard uncertainty values 
#'                   (one of {x.u, x.cov} oblig) 
#' @param \code{x.pdf} named vector of pdf types (norm, unif, stud...)
#' @param \code{x.df} named vector of degrees of freedom for \code{x.pdf}
#' @param \code{x.cor} named correlation matrix between model parameters
#' @param \code{x.cov} named variance/covariance matrix 
#'                     between model parameters (one of {x.u, x.cov} oblig)  
#' 
#' @return 
#' \code{X} a MxN matrix of M values for N variables
#' 
#' @details 
#' Available distributions:
#' \itemize{
#' \item{'delta'} {Dirac delta distribution for constants; args = x.mu}
#' \item{'norm'} {Normal; args = x.mu, x.u}
#' \item{'tnorm'} {Truncated normal (positive values); args = x.mu, x.u}
#' \item{'lnorm'} {Lognormal; args = x.mu, x.u}
#' \item{'stud'} {Student's T; args = x.mu, x.u, x.df}
#' \item{'unif'} {Uniform; args = x.mu, x.u}
#' \item{'triangle'} {Symmetric triangular; args = x.mu, x.u}
#' \item{'arcsine'} {Arcsine derivative; args = x.mu, x.u}
#' }
#' 
#' Correlation between variables is described by matrices \code{x.cov}
#' or \code{x.cor}, and enforced by Gaussian copula. 
#' 
#' @aliases PDFs
#' 
#' @rdname xSample
#' @examples
#' x.mu = c(1,1); names(x.mu)=c('x1','x2')
#' x.u = c(0.1,0.1); names(x.u)=c('x1','x2')
#' x.pdf = c('unif','triangle'); names(x.pdf)=c('x1','x2')
#' X=xSample(M=1000,x.mu,x.u,x.pdf)
#' SAPlot(X)
#' @export
xSample = function (M=1000,x.mu,x.u,x.pdf,x.df,
                    x.cor=diag(length(x.mu)),x.cov=NULL) {
  if (missing(M)) {
    print('Generate MC sample from prescribed distribution',quote=F) 
    print(" ",quote=F)     
    print("Call   : xSample(M, x.mu, x.u, x.pdf, x.df, x.cor)",quote=F)
    print("-M     : (integ, def=1000) number of points in sample",quote=F)
    print("-x.mu  : (oblig) named vector of mean values",quote=F)
    print("-x.u   : (oblig) named vector of standard uncertainty values",quote=F)
    print("-x.pdf : (oblig) named vector of pdf types:",quote=F)
    print("         'delta' Dirac delta distribution for constants",quote=F)
    print("         'norm'  Normal",quote=F)
    print("         'tnorm' Truncated normal (positive values)",quote=F)
    print("         'lnorm' Lognormal",quote=F)
    print("         'stud'  Student's T",quote=F)
    print("         'unif'  Uniform",quote=F)
    print("         'triangle' Symmetric triangular",quote=F)
    print("         'arcsine' Arcsine derivative",quote=F)
    print("-x.df  : (opt.) named vector of degrees of freedom for x.pdf",quote=F)
    print("-x.cor : (matrix, def=I) named correlation matrix between model parameters",
          quote=F)
    print("-x.cov : (one of {x.u, x.cov} oblig) names variance/covariance matrix",quote=F)    
    print("         between model parameters",quote=F) 
    return(invisible())
  }
  
  np = length(x.pdf)
  sample = matrix(ncol=np,nrow=M)
  
  # Generate random sample for correlation by Gaussian copula
  if (min(eigen(x.cor, symmetric=TRUE, 
                only.values=TRUE)$values) < sqrt(.Machine$double.eps))
    stop("Correlation matrix is not postitive definite\n")
  z = mvtnorm::rmvnorm(M, mean=rep(0, np), sigma=x.cor)
  z = pnorm(z)
  
  for (ip in 1:np){
    av = x.mu[ip]
    sd = x.u[ip]
    if(!missing(x.df)) nu = x.df[ip]
    pdf=switch(x.pdf[ip],
               delta = paste0(av),
               norm  = 'qnorm(z[,ip],mean=av,sd=sd)',
               tnorm = 'msm::qtnorm(z[,ip],mean=av,sd=sd,lower=.Machine$double.eps)',
               lnorm = 'qlnorm(z[,ip],meanlog=log(av^2/(sd^2+av^2)^0.5),
                        sd=(log(1+sd^2/av^2))^0.5)',
               stud  = 'av+sd*qt(z[,ip],df=nu)',
               unif  = 'qunif(z[,ip],min=av-sd*3^0.5,max=av+sd*3^0.5)',
               triangle = 'triangle::qtriangle(z[,ip],a=av-sd*6^0.5,b=av+sd*6^0.5)',
               arcsine = 'qarcsine(z[,ip],a=av-sd*2^0.5,b=av+sd*2^0.5)',
               default= NULL
    )
    if(is.null(pdf))
      stop(paste0('Unsupported pdf type: ',x.pdf[ip]))
    else
      sample[1:M,ip]=eval(parse(text=pdf))
  }
  colnames(sample)=names(x.mu)
  return(sample)
}

