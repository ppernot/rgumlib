#'@title
#' Plot of Empirical Cumulated Density function and p-percent Coverage Interval
#'
#'@description
#' Generates plot from sample.
#' 
#' @param \code{Y} a vector 
#' @param \code{p} coverage of confidence interval
#' @param \code{cex} graphical parameter
#' 
#' @return A plot.

#' @rdname ECIPlot
#' @examples
#' fExpr = expression(x1+x2)
#' x.mu = c(1,1); names(x.mu)=c('x1','x2')
#' x.u = c(0.1,0.1); names(x.u)=c('x1','x2')
#' x.pdf = c('unif','triangle'); names(x.pdf)=c('x1','x2')
#' S=gumS1(fExpr,x.mu,x.u,x.pdf,x.df=NULL,nrunMax=1000)
#' ECIPlot(S$Y)
#' @export
ECIPlot = function (Y, p=0.95, cex=1) {
  # Plot ECDF and p-CI
  par(mar=c(5,5,1,1),cex=cex,cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
  plot(ecdf(Y), col='blue',lwd=2,xlab='',ylab='ECDF(Y)',
       xlim=range(Y),ylim=c(0,1),xaxs='i', yaxs='i',main='')
  grid(col='gray30')
  mtext('Y',side=1,cex=2*cex,padj=2)
  a = 0.5*(1-p)
  ns=floor(log10(sd(Y)))-1
  for (p in c(a, 0.5, p+a)) {
    Qp=quantile(Y,p=p)
    segments(x0=min(Y)-1,y0=p,x1=Qp,y1=p,col='darkgreen',lwd=2,lty=2)
    segments(x0=Qp,y0=0,x1=Qp,y1=p,col='red',lwd=2,lty=2)
    mtext(p,at=p,side=2,col='darkgreen',srt=0,cex=cex)
    mtext(round(Qp/10^ns,0)*10^ns,at=Qp,side=1,col='red',padj=0.2,cex=cex)
  }
  points(Y,pnorm(Y,mean=mean(Y),sd=sd(Y)),col='orange',pch=19,cex=0.1)
  legend('bottomright',legend=c('ECDF','Norm. Approx.'), lwd=2, lty=1,
         col=c('blue','orange'),cex=1.5)
}
