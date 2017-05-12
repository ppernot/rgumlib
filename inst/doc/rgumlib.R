## ----, echo=FALSE, results='hide'----------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(rgumlib)
# set.seed(1234) # Initialise la graine du RNG

## ------------------------------------------------------------------------
# Expression analytique du modele
fExpr = expression( x1 + 2*x2 )
# or
fExpr = function(x1,x2) x1 + 2*x2 

## ----, tidy=FALSE--------------------------------------------------------
# Variables incertaines
x1 = 1; x1.u = 0.1
x2 = 1; x2.u = 0.1 

## ----, tidy=FALSE--------------------------------------------------------
# Variables incertaines
x1.pdf = "norm"
x2.pdf = "stud"; x2.df = 5 

## ----, echo=TRUE---------------------------------------------------------
# Collect parameters in environment
params = collectParams(fExpr)
str(params)

## ----, echo=TRUE---------------------------------------------------------
G=with(params, 
       gumCV(fExpr=fExpr,x.mu=x.mu,x.u=x.u,silent=FALSE)
       )

## ----, echo=TRUE---------------------------------------------------------
str(G)

## ----, echo=TRUE---------------------------------------------------------
knitr::kable(G$budget)

## ----, echo=TRUE---------------------------------------------------------
S=with(params, 
       gumS1(fExpr=fExpr,x.mu=x.mu,x.u=x.u,x.pdf=x.pdf,x.df=x.df,
             adapt=FALSE, nrun=1000)
       )

## ----, echo=TRUE---------------------------------------------------------
S=with(params, 
       gumS1(fExpr=fExpr,x.mu=x.mu,x.u=x.u,x.pdf=x.pdf,x.df=x.df,
             adapt=TRUE,ndig=1)
       )

## ----, echo=TRUE---------------------------------------------------------
S=with(params, 
       gumS2(fExpr=fExpr,x.mu=x.mu,x.u=x.u,x.pdf=x.pdf,x.df=x.df,
             ndig=1)
       )

## ----, echo=TRUE, fig.show = "hold", fig.cap = "Graphical summaries of Monte Carlo samples: (left) cumulative statstics; (right) empirical cumulative distribution."----
cumPlot(S$Y,cex=0.5)
ECIPlot(S$Y,cex=0.5)

## ----, echo=TRUE---------------------------------------------------------
uncPrint(y=G$y.mu,uy=G$y.u)

## ----, echo=TRUE---------------------------------------------------------
CIPrint(y=G$y.mu,uy=G$y.u,fac=1.96,p=0.95)

## ----, echo=TRUE---------------------------------------------------------
G$anova

## ----, echo=TRUE, fig.cap = "Inputs-output scatterplots and correlations."----
SAPlot(cbind(S$X,S$Y),cex=0.5)

## ----, echo=TRUE---------------------------------------------------------
(CorP = cor(S$X,S$Y, method = "pearson"))
cor(S$X,S$Y, method = "spearman")

## ----, echo=TRUE, fig.show = "hold"--------------------------------------
z=with(params,
       sobolSA(fExpr=fExpr,x.mu=x.mu,x.u=x.u,x.pdf=x.pdf,x.df=x.df,
               graph=TRUE,cex=0.5)
       )
print(z)

## ----, echo=TRUE---------------------------------------------------------
d= vgSA(fExpr=fExpr,x.mu=params$x.mu, x.u=params$x.u, 
        X=S$X, Y=S$Y, silent=TRUE)
knitr::kable(d$budget)

## ----, echo=TRUE---------------------------------------------------------
signif(CorP^2,3)

## ----, tidy=FALSE--------------------------------------------------------
# Expression analytique du modele
fExpr = expression( x1^2 + x2^2 )

# Variables incertaines
x1=0; x1.u=1.0; x1.pdf = "norm"
x2=1; x2.u=0.1; x2.pdf = "norm" 


## ----, echo=TRUE---------------------------------------------------------
# Collect parameters in environment
params=collectParams(fExpr)

## ----, echo=TRUE---------------------------------------------------------
G=with(params, 
       gumCV(fExpr=fExpr,x.mu=x.mu,x.u=x.u,silent=TRUE)
       )
knitr::kable(G$budget)
uncPrint(y=G$y.mu,uy=G$y.u)
# Incertitude élargie
CIPrint(y=G$y.mu,uy=G$y.u,fac=1.96,p=0.95)

## ----, echo=TRUE---------------------------------------------------------
S=with(params, 
       gumS2(fExpr=fExpr,x.mu=x.mu,x.u=x.u,x.pdf=x.pdf,x.df=x.df)
       )

## ----, echo=TRUE, fig.show = "hold"--------------------------------------
cumPlot(S$Y,cex=0.5)
ECIPlot(S$Y,cex=0.5)

## ----, echo=TRUE, fig.cap = "Inputs-output scatterplots and correlation."----
SAPlot(cbind(S$X,S$Y),cex=0.5)

## ----, echo=TRUE, fig.show = "hold"--------------------------------------
z=with(params,
       sobolSA(fExpr=fExpr,x.mu=x.mu,x.u=x.u,x.pdf=x.pdf,x.df=x.df,
               graph=TRUE,cex=0.5)
       )
print(z)

## ----, echo=TRUE---------------------------------------------------------
d= vgSA(fExpr=fExpr,x.mu=params$x.mu, x.u=params$x.u, X=S$X, Y=S$Y, silent=TRUE)
knitr::kable(d$budget)

