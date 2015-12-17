#'@title
#' Combination of Variances
#'
#'@description
#' Uncertainty propagation by combination of variances (GUM). 
#' 
#' @param fExpr An expression or a function object. For an expression,
#'              all variables will be considered as uncertain. For a 
#'              function the list of uncertain variables is derived
#'              from the list of arguments.
#' @param x.mu Named vector of mean values 
#'              with names compatible with \code{fExpr}.
#' @param x.u Named vector of standard uncertainty values 
#'             (one of \{\code{x.u}, \code{x.cov}\} mandatory). 
#' @param x.cor Named correlation matrix between model parameters
#' @param x.cov Named variance/covariance matrix 
#'                     between model parameters 
#'                     (one of \{\code{x.u}, \code{x.cov}\} mandatory).  
#' @param budgetTable Flag to controle cmputation of the budget table.
#' @param silent Flag to execute \code{gumCV} without printout.
#' @param tol Numeric tolerance level to check positive-definiteness of 
#'                    \code{x.cor} or \code{x.cov}. 
#' 
#' @return A list containing:
#' \item{y.mu}{mean value of model}
#' \item{y.u}{standard uncertainty of model}
#' \item{anova}{(vector) relative contributions of parameters to y variance}
#' \item{anovaCov}{global relative contribution of parameters covariance to \code{Y} variance}
#' \item{budget}{(dataframe) uncertainty budget table, mostly to be printed}
#' 
#' @references (GUM) Evaluation of measurement data - Guide to the expression of 
#' uncertainty in measurement. JCGM 100:2008. 
#' \url{http://www.bipm.org/utils/common/documents/jcgm/JCGM_100_2008_E.pdf}
#'  
#' @export
#' 

gumCV = function (fExpr, x.mu, x.u, x.cor=diag(length(x.mu)),x.cov=NULL, 
                  budgetTable = TRUE, silent=FALSE, tol=sqrt(.Machine$double.eps)) {

  # Check data consistency
  locL = length(x.mu)  
  locX = as.list(x.mu)

  # Provide default names do input vector, if missing
  if(is.null(names(locX))) names(locX)=paste0('X',1:locL)
  
  # Get variables names in fExpr
  var.names= if(class(fExpr)=='function') names(formals(fExpr)) else all.vars(fExpr)
  
  # Check names compatibility 
  x.names=names(locX)
  if(!all(x.names %in% var.names))
    stop('x.mu variables names not consistent with fExpr')
    
  # Sensitivities and mean value
  if (class(fExpr)=='function') {
    # Derivation numerique
    fun = function (x) do.call(fExpr,as.list(x))
    y.mu = fun(x.mu)
    J = numDeriv::grad(fun,x.mu)
  } else {
    # Derivation symbolique
    df = eval(deriv(fExpr,x.names),locX)
    y.mu  = eval(fExpr,locX)
    J = as.vector(attr(df, "gradient"))
  }

  # Covariance matrix
  if(is.null(x.cov)) {
    if (min(eigen(x.cor, symmetric=TRUE, only.values=TRUE)$values) < tol )
      stop("Supplied correlation matrix is not postitive definite\n")
    V = outer(x.u, x.u, "*")*x.cor
  } else {
    if (min(eigen(x.cov, symmetric=TRUE, only.values=TRUE)$values) < tol )
      stop("Supplied var/covar matrix is not postitive definite\n")
    V = x.cov
  }
     
  # Combinaison des variances
  y.u = (t(J) %*% V %*% J)^0.5
  
  if(!silent & !budgetTable) {
    cat('*** Combinaison of variances:')
    uncPrint(y.mu,y.u) # Pretty print results
    cat('\n ')
  }
  
  # Anova
  yu2  = y.u^2
  j2u2 = J^2 * x.u^2
  anova=rep(NA,length(x.mu))
  names(anova)=x.names
  anovaCov=0
  if(yu2 !=0 ) {
    anova = j2u2/yu2
    anovaCov = 1-sum(anova)
  }
  
  if(budgetTable) {
    #Build budget table   
    selX = x.u != 0 # exclude constant params
    budget=data.frame(
                      Valeur = sprintf("%.3e",c(x.mu[selX],y.mu)),
                      Inc_Std. = sprintf("%.2e",c(x.u[selX],y.u)),
                      J = c(sprintf("%.2e",J[selX]),'<--'),
                      J2.U2 = sprintf("%.2e",c(j2u2[selX],y.u^2)),
                      Contrib. = c(sprintf("%.2f",anova[selX]),""),
                      stringsAsFactors = FALSE)  
    rownames(budget) = c(x.names[selX],'Y')
    
    if(abs(anovaCov) > 2*.Machine$double.eps) {
      # Insert covariances contribution into budget table
      covLine=data.frame(Valeur='', Inc_Std.='', J='',
                         J2.U2 = sprintf("%.2e",yu2-sum(j2u2)),
                         Contrib. = sprintf("%.2f",anovaCov),
                         stringsAsFactors = FALSE)
      l=nrow(budget)
      budget=rbind(budget[-l,],covLine,budget[l,])
      rownames(budget) = c(x.names[selX],'Cov','Y')
      
    } 
    if(!silent) print(budget,right=FALSE)

    } else {
    budget = NA
    
  }      

  return( list(y.mu=y.mu, y.u=y.u[1,1], anova=anova, 
               anovaCov=anovaCov, budget=budget) )     
}
