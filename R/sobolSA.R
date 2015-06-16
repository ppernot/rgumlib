#'@title
#' Sobol Sensitivity Indices
#'
#'@description
#' Compute and plots Sobol first- and total-order sensitivity indices for model \code{fExpr}. 
#' Only for independeninput parameters. This is a wrapper for the \code{soboljansen}
#' function of package \code{sensitivity}.
#' 
#' @param fExpr An expression or a function object.
#' @param x.mu Named vector of mean values 
#'             with names compatible with \code{fExpr}.
#' @param x.u Named vector of standard uncertainty values 
#'            (one of \{\code{x.u}, \code{x.cov}\} mandatory).
#' @param x.pdf Named vector of pdf types (see \code{\link{PDFs}}).
#' @param x.df Named vector of degrees of freedom for \code{x.pdf}.
#' @param M Size of Sobol samples.
#' @param nboot Size of bootstrap to calculate uncertainty on Sobol indices.
#' @param graph Flag to plot bargraphs of Sobol indices.
#' @param cex A graphical parameter.
#' 
#' @return See \code{\link[sensitivity]{soboljansen}}.
#' 
#' @rdname sobolSA
#' @examples
#' fExpr = expression(x1+x2)
#' x.mu = c(1,1); names(x.mu)=c('x1','x2')
#' x.u = c(0.1,0.1); names(x.u)=c('x1','x2')
#' x.pdf = c('unif','triangle'); names(x.pdf)=c('x1','x2')
#' sobolSA(fExpr,x.mu,x.u,x.pdf)
#' @export
sobolSA = function(fExpr,x.mu,x.u,x.pdf,x.df,M=10000,nboot=0,
                   graph=TRUE,cex=1) {  
  # Premier echantillon
  X1 = data.frame(xSample(M,x.mu,x.u,x.pdf,x.df))
  # Deuxieme echantillon
  X2 = data.frame(xSample(M,x.mu,x.u,x.pdf,x.df))
  # Run de la methode de Sobol
  z = sensitivity::soboljansen(model=fVector,X1=X1,X2=X2,nboot=nboot,fExpr=fExpr)
 
  if (graph) {
    # Affichage
    par(cex=cex)
    barplot(height=t(as.matrix(cbind(z$S,z$T),byrow=FALSE,ncol=2)),
            legend.text=c("First Order","Total"),names.arg=rownames(z$S),
            beside=TRUE,col=c("pink","cyan"),cex.axis=2,cex.names=2,
            space=c(0.2,1))
    abline(h=0) 
    grid(col='gray30')
    barplot(height=t(as.matrix(cbind(z$S,z$T),byrow=FALSE,ncol=2)),
            legend.text=c("First Order","Total"),names.arg=rownames(z$S),
            beside=TRUE,col=c("pink","cyan"),cex.axis=2,cex.names=2,
            space=c(0.2,1),add=TRUE,
            main="Sobol sensitivity indices")
    abline(h=0)    
  }
  
  return(z)
}