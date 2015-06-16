doGUM = function(fExpr) {
  
  # Collect parameters in environment
  params=collectParams(fExpr)
  
  # Combinaison des variances
  G=with(params, gumCV(fExpr=fExpr,x.mu=x.mu,x.u=x.u))
  
  # Monte Carlo adaptatif
  # S = gumS1(nrun=1000, params=params, adapt=TRUE)
  S=with(params, gumS2(fExpr=fExpr,x.mu=x.mu,x.u=x.u,x.pdf=x.pdf,x.df=x.df))
  
  # Analyse de sensibilite :
  ## 1/ Correlation entrees/sortie
  SAPlot(cbind(S$X,S$Y))
  
  ## 2/ Parallel plot
  parPlot(cbind(S$X,S$Y))
  
  ## 3/ Coefficients de Sobol
  z=with(params,sobolSA(fExpr=fExpr,x.mu=x.mu,x.u=x.u,x.pdf=x.pdf,x.df=x.df))
  
  ## 4/ Variance Gradients
  d= vgSA(fExpr=fExpr,x.mu=params$x.mu, x.u=params$x.u, X=S$X, Y=S$Y)
  
  return(list(y.mu=G$y.mu, y.u=G$y.u, anova=G$anova, 
              anovaCov=G$anovaCov, budget=G$budget,
              X=S$X, Y=S$Y))
}

