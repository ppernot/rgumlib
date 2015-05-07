#'@title
#' Plot of cumulated mean and coverage interval limits
#'
#'@description
#' Generates plot from sample to visually check convergence of Monte Carlo sampling.
#' 
#' @param \code{Y} a vector 
#' @param \code{p} coverage of confidence interval
#' @param \code{cex} graphical parameter
#' 
#' @return A plot.

#' @rdname cumPlot
#' @examples
#' fExpr = expression(x1+x2)
#' x.mu = c(1,1); names(x.mu)=c('x1','x2')
#' x.u = c(0.1,0.1); names(x.u)=c('x1','x2')
#' x.pdf = c('unif','triangle'); names(x.pdf)=c('x1','x2')
#' S=gumS1(fExpr,x.mu,x.u,x.pdf,x.df=NULL,nrunMax=1000)
#' cumPlot(S$Y)
#' @export
cumPlot = function (Y, p=0.95, cex=1) {
  # Plot mean and quantiles convergence
  # Similar to coda::cumuplot(), but mean instead of median
  
  par(mar=c(5,5,1,2),cex=cex,cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
  
  freq = ifelse(length(Y) > 1000, round(length(Y)/1000), 1)
  keep = seq(1, length(Y), by=freq) 
  cs = cumsum(Y)/(1:length(Y))  
  plot(keep,cs[keep], col='blue', type='l', lwd=2, xlab='',
       ylim=range(Y), xaxs='i', yaxs='i', ylab='Y',main='')
  abline(h=cs[length(Y)],col='red',lty=2)
  
  probs=c((1-p)/2,1-(1-p)/2)
  cs=matrix(NA,nrow=length(Y),ncol=2)
  for (i in seq(along=Y)) cs[i,1:2] = quantile(Y[1:i],probs)

  matlines(keep,cs[keep,1:2],col='blue',lwd=2,lty=2)
  abline(h=cs[length(Y),1:2],col='red',lty=2)
  
#   keep = which(Y<cs[,1] | Y>cs[,2])
#   points(keep,Y[keep], pch=19, cex=0.5, col='orange')
  
  grid(col='gray30')

  legend('topright',legend=c('Mean',paste0('Quantiles; p=',p)), lwd=2, lty=c(1,2),
         col='blue',cex=1.5)
}

