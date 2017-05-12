#'@title
#' Variance Gradient Sensitivity Analysis
#'
#'@description
#' Uses Monte Carlo sample to generate Variance Gradients estimates.
#' Only for independent input parameters. 
#' 
#' @param fExpr An expression or a function object.
#' @param x.mu Named vector of mean values 
#'             with names compatible with \code{fExpr}.
#' @param x.u Named vector of standard uncertainty values. 
#' @param X A \code{M}x\code{N} matrix of \code{M} values for 
#'            \code{N} input variables.
#' @param Y A vector of values corresponding to \code{X}.
#' @param budgetTable Flag to compute the budget table.
#' @param silent Flag to run without printout.
#' 
#' @return A list containing:
#' \item{vg}{(vector) variance gradients}
#' \item{budget}{(dataframe) VG budget table, mostly to be printed}
#' 
#' @references Mark Campanelli, Raghu Kacker and R\"udiger Kessel (2013)
#' Variance gradients and uncertainty budgets for nonlinear measurement 
#' functions with independent inputs. \emph{Meas. Sci. Technol.} \strong{24}:25002. 
#'  
#' @rdname vgSA
#' @examples
#' fExpr = expression(x1+x2)
#' x.mu = c(1,1); names(x.mu)=c('x1','x2')
#' x.u = c(0.1,0.1); names(x.u)=c('x1','x2')
#' x.pdf = c('unif','triangle'); names(x.pdf)=c('x1','x2')
#' S=gumS1(fExpr,x.mu,x.u,x.pdf,x.df=NULL,nrunMax=1000)
#' vgSA(fExpr,x.mu,x.u,S$X,S$Y)
#' @export
vgSA = function (fExpr, x.mu, x.u, X, Y, budgetTable = TRUE, silent=FALSE) {
  if (missing(fExpr)) {
    print('\nVariance Gradients Sensitivity Analysis for model fExpr',quote=F)
    print(" ",quote=F)
    print("Call   : vgSA(fExpr,x.mu, x.u, X, Y, budgetTable, silent)",quote=F)
    print("-fExpr : (oblig) an expression or a function object",quote=F)
    print("-x.mu  : (oblig) named vector of mean values",quote=F)
    print("         with names compatible with fExpr",quote=F)
    print("-x.u   : (oblig) named vector of standard uncertainty values",quote=F)
    print("-X     : (matrix) sample of inputs ",quote=F)
    print("-Y     : (vector) sample of outputs corresponding to X",quote=F)
    print("-budgetTable : (logical, def=TRUE) wether the budget table is computed",quote=F) 
    print("-silent: (logical, def=FALSE) wether gumCV executes without printout",quote=F) 
    print(" ",quote=F)
    print('Returns: list(vg, budget)',quote=F)
    print("-gv    : (vector) variance gradients",quote=F)
    print("-budget: (dataframe) uncertainty budget table, mostly to be printed",quote=F)
    return(invisible())
  }
  # Check data consistency
  L=length(x.mu)
  x = as.list(x.mu)
  if(is.null(names(x))) names(x)=paste0('X',1:L)
  var.names= if(class(fExpr)=='function')
    names(formals(fExpr))
  else
    all.vars(fExpr)
  x.names=names(x)
  if(!all(x.names %in% var.names))
    stop('x.mu names do not match fExpr arguments')

  tabDeriv =  fDeriv(X,fExpr)
  Xc = scale(X,center=TRUE,scale=FALSE)
  Yc = matrix(scale(Y,center=TRUE,scale=FALSE),
                ncol=ncol(Xc),nrow=nrow(Xc),byrow=FALSE)
  y.mu = mean(Y)
  varY = var(Y)
  vg = colMeans(Yc*tabDeriv*Xc)/rep(varY,L)
  
  budget = NA
  if(budgetTable) {
    #Build budget table   
    selX = x.u != 0 # exclude constant params
    budget=data.frame(
                      Valeur = sprintf("%.3e",c(x.mu[selX],y.mu)),
                      Inc_Std. = sprintf("%.2e",c(x.u[selX],varY^0.5)),
                      VG = c(sprintf("%.2e",vg[selX]),""),
                      stringsAsFactors = FALSE)      
    rownames(budget) = c(x.names[selX],'Y')
    if(!silent) {
      cat('\n*** Variance Gradients Sensitivity Analysis')
      cat('\n ')
      print(budget,right=FALSE)
    }
  }      
  return( list(vg=vg, budget=budget) )     
}