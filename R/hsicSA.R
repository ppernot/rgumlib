#'@title
#' Hilbert-Schmidt Independence Criterion Sensitivity Analysis
#'
#'@description
#' Uses Monte Carlo sample to generate Hilbert-Schmidt Independence Criterion
#' based SA indices 
#' 
#' @param X A \code{M}x\code{N} matrix of \code{M} values for 
#'            \code{N} input variables.
#' @param Y A vector of values corresponding to \code{X}.
#' @param budgetTable Flag to compute the budget table.
#' @param silent Flag to run without printout.
#' 
#' @return A list containing:
#' \item{hs}{(vector) HSIC sensitivity indices}
#' \item{budget}{(dataframe) VG budget table, mostly to be printed}
#' 
#' @references Da Veiga S. (2015), 
#' Global sensitivity analysis with dependence measures, 
#' \emph{J. Stat. Comp. Sim.}, \strong{85}:1283-1305.
#' 
#' @rdname hsicSA
#' @examples
#' fExpr = expression(x1+x2)
#' x.mu = c(1,1); names(x.mu)=c('x1','x2')
#' x.u = c(0.1,0.1); names(x.u)=c('x1','x2')
#' x.pdf = c('unif','triangle'); names(x.pdf)=c('x1','x2')
#' S = gumS1(fExpr,x.mu,x.u,x.pdf,x.df=NULL,nrunMax=1000)
#' hsicSA(S$X,S$Y,silent=TRUE)
#' @export
#' 
hsicSA = function (X, Y, budgetTable = TRUE, silent=FALSE) {
  if (missing(X) | missing(Y)) {
    print('\nHSIC Sensitivity Analysis',quote=F)
    print(" ",quote=F)
    print("Call   : hsicSA(X, Y, budgetTable, silent)",quote=F)
    print("-X     : (matrix) sample of inputs ",quote=F)
    print("-Y     : (vector) sample of outputs corresponding to X",quote=F)
    print("-budgetTable : (logical, def=TRUE) wether the budget table is computed",quote=F) 
    print("-silent: (logical, def=FALSE) wether gumCV executes without printout",quote=F) 
    print(" ",quote=F)
    print('Returns: list(hs, budget)',quote=F)
    print("-hs    : (vector) HSIC sensitivity indices",quote=F)
    print("-budget: (dataframe) uncertainty budget table, mostly to be printed",quote=F)
    return(invisible())
  }
  
  hs = c()
  for(i in 1:ncol(S$X))
    hs[i] = rHSIC(S$X[,i],S$Y)
  
  budget = NA
  if (budgetTable) {
    budget = data.frame(S = c(sprintf("%.2e", hs)), 
                        stringsAsFactors = FALSE)
    rownames(budget) = colnames(S$X)
    if (!silent) {
      cat("\n*** HSIC Sensitivity Analysis")
      cat("\n ")
      print(budget, right = FALSE)
    }
  }
  return(list(hs = hs, budget = budget))
}