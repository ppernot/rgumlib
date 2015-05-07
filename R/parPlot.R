genColors = function(sample) {
  ncols=length(sample)
  co=(    sample -min(sample))/
    (max(sample)-min(sample))
  indx=round(1+(ncols-1)*co)
  cols=fields::two.colors(ncols,start="blue",middle="yellow",end="red")[indx]
  return(cols)
}
paraPlot = function (x, col = 1, lty = 1, pch=19, var.label = FALSE, ...) {
  # Parallel plot (modif from MASS::parcoord)
  rx <- apply(x, 2, range, na.rm = TRUE)
  x <- apply(x, 2, function(x) (x - min(x, na.rm = TRUE))/
               (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  matplot(1:ncol(x), t(x), type = "l", col = col, lty = lty, 
          xlab = "", ylab = "", axes = FALSE, ...)
  matpoints(1:ncol(x), t(x), col = col, pch=pch,cex=0.8)
  axis(1, at = 1:ncol(x), labels = colnames(x))
  for (i in 1:ncol(x)) {
    lines(c(i, i), c(0, 1), col = "grey70")
    if (var.label) 
      text(c(i, i), c(0, 1), labels = format(rx[, i], digits = 3), 
           xpd = NA, offset = 0.3, pos = c(1, 3), cex = 0.7)
  }
  invisible()
}
parPlot = function(X, maxPoints=256) {
  sdX=apply(X,2,sd) # Identify fixed params to exclude from plot
  nP=min(maxPoints,nrow(X))
  iSamp = seq.int(1,nrow(X),len=nP)
  X1=X[iSamp,sdX != 0]
  par(cex=1,cex.axis=1.5)
  colors=genColors(X1[,ncol(X1)])
  paraPlot(X1,col=colors,lwd=1)
}

