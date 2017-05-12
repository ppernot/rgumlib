#'@title
#' Generate Sample
#'
#'@description
#' Generate Monte Carlo sample from prescribed PDF. 
#' All PDFs are parameterized by their mean and standard deviation (if needed).
#'
#' @param M Sample size. 
#' @param x.mu Named vector of mean values 
#'             with names compatible with \code{fExpr}.
#' @param x.u Named vector of standard uncertainty values 
#'            (one of \{\code{x.u}, \code{x.cov}\} mandatory).
#' @param x.pdf Named vector of pdf types (see below).
#' @param x.df Named vector of degrees of freedom for \code{x.pdf}.
#' @param x.cor Named correlation matrix between model parameters.
#' @param x.cov Named variance/covariance matrix between model parameters 
#'            (one of \{\code{x.u}, \code{x.cov}\} mandatory).
#' @param tol Numeric tolerance level to check positive-definiteness of 
#'                    \code{x.cor} or \code{x.cov}. 
#'                                 
#' @return 
#' \code{X} A \code{M}*\code{N} matrix of \code{M} values for \code{N} 
#'          variables.
#' 
#' @details 
#' Available distributions:
#' \itemize{
#' \item{'delta'} {Dirac delta distribution (for constants); args = x.mu}
#' \item{'norm'} {Normal; args = x.mu, x.u}
#' \item{'tnorm'} {Truncated normal (positive values); args = x.mu, x.u}
#' \item{'lnorm'} {Lognormal; args = x.mu, x.u}
#' \item{'stud'} {Student's T; args = x.mu, x.u, x.df}
#' \item{'unif'} {Uniform; args = x.mu, x.u}
#' \item{'triangle'} {Symmetric triangular; args = x.mu, x.u}
#' \item{'arcsine'} {Arcsine derivative; args = x.mu, x.u}
#' \item{'pois'} {Poisson; args = x.mu}
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
                    x.cor=diag(length(x.mu)),x.cov=NULL,
                    tol=sqrt(.Machine$double.eps)) {

  np = length(x.pdf)
  sample = matrix(ncol=np,nrow=M)
  
  # Generate random sample for correlation by Gaussian copula
  if(!is.null(x.cov)) x.cor = cov2cor(x.cov) # Use covmat if present
  
  if (min(eigen(x.cor, symmetric=TRUE, only.values=TRUE)$values) < tol)
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
               pois  = 'qpois(z[1:M,ip],lambda=av)',
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

