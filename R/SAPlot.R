#'@title
#' Sensitivity Analysis Plot
#'
#'@description
#' 2D scatterplots of the inputs/outputs sample, histograms and correlation coefficients.
#' 
#' @param \code{X} a MxN matrix of M values for N variables
#' @param \code{cex} graphical parameter
#' 
#' @return A plot.

#' @rdname SAPlot
#' @examples
#' fExpr = expression(x1+x2)
#' x.mu = c(1,1); names(x.mu)=c('x1','x2')
#' x.u = c(0.1,0.1); names(x.u)=c('x1','x2')
#' x.pdf = c('unif','triangle'); names(x.pdf)=c('x1','x2')
#' S=gumS1(fExpr,x.mu,x.u,x.pdf,x.df=NULL,nrunMax=1000)
#' SAPlot(cbind(S$X,S$Y))
#' @export
SAPlot = function(X,cex=1) {
  if (missing(X)) {
    print('Scatterplot and correlation pairs for sample X',quote=F)
    print(" ",quote=F)
    print("Call : SAPlot(X)",quote=F)
    print("-X   : (oblig) a MxN matrix of M values for N variables",quote=F)
    print("-cex : graphical parameter",quote=F)
    return(invisible())
  }
  sdX=apply(X,2,sd) # Identify fixed params to exclude from plot
  par(cex=cex,cex.axis=1.5)
  pairs(X[,sdX != 0], gap=0,
        upper.panel=panel.cor,
        diag.panel =panel.hist,
        lower.panel=panel.smooth )
}
panel.hist <- function(x,...) {
  usr <- par("usr"); on.exit(par(usr))     
  par(usr = c(usr[1:2], 0, 1.5))     
  h <- hist(x, plot = FALSE)     
  breaks <- h$breaks;
  nB <- length(breaks)   
  y <- h$counts; y <- y/max(y)
  grid(col='brown',ny=0)
  rect(breaks[-nB],0,breaks[-1],y,col="orange",...)
}   
panel.cor <- function(x,y,digits=2,prefix="",cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y,method="spearman")
  ra = abs(r)
  txt <- format(c(r,0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)*ra
  text(0.5,0.5,txt,cex = cex.cor,col=ifelse(r>=0,4,2))
}
panel.smooth <- function (x, y, cex = 1.5, col.smooth = "red", 
                          span = 2/3, iter = 3, ...) {
  maxPoints=500
  nP=min(maxPoints,length(x))
  iSamp = seq.int(1,length(x),len=nP)
  x1=x[iSamp]
  y1=y[iSamp]
  green_tr=rgb(unlist(t(col2rgb("darkgreen"))),
              alpha=30,maxColorValue = 255)
  grid(col='brown')
  points(x1, y1, pch = 19, col = green_tr, lwd=0, cex = cex)
}

