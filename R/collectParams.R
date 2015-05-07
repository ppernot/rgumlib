#'@title
#' Collect Parameters
#'
#'@description
#' Collects from the global environment the list of values
#' necessary to execute \code{GUMCV} and \code{GUMS1/2} on model \code{fExpr}.
#' The names of the variables should be consistent with the parameters of \code{fExpr}.
#' For a variable named \code{'var'}, the mean value is seached in variable \code{var},
#' the standard uncertainty in \code{var.mu}, the pdf in \code{var.pdf}, and
#' the degrees of freedom in \code{var.df}. 
#' 
#' @param \code{fExpr} an expression or a function object
#' @param \code{mc} flag to detect data for Monte Carlo sampling
#' 
#' @return A list containing:
#' \item{x.names}{vector of names for the input parameters}
#' \item{x.mu}{vector of mean values for the input parameters}
#' \item{x.u}{vector of standard uncertainties for the input parameters}
#' \item{x.pdf}{vector of pfd names for the input parameters}
#' \item{x.df}{vector of degrees of freedom for the input parameters}
#' \item{fExpr}{same as input}
#'  
#' @rdname collectParams
#' @examples
#' fExpr = function(V,T,a,c) T*V + V^2/(2*a*c) 
#' 
#' V = 130 /3.6 ; V.u = 4/3.6         ; V.pdf = "norm"
#' T = 1        ; T.u = 0.5 / 3^0.5   ; T.pdf = "unif"
#' a = 5        ; a.u = 0.5           ; a.pdf = "norm"        
#' c = 0.7      ; c.u = 0.2/6         ; c.pdf = "norm"
#' # Collect parameters in environment
#' params=collectParams(fExpr)
#' # GUM
#' G = with(params, 
#'          gumCV(fExpr=fExpr,x.mu=x.mu,x.u=x.u)
#'          )
#' @export
collectParams = function(fExpr,mc=TRUE) {
  if (missing(fExpr)) {
    print('Collects from the global environment the list of values',quote=F)
    print('necessary to execute GUMCV and GUMSx on model fExpr', quote=F)
    print(" ",quote=F)
    print("Call   : collectParams(fExpr,mc)",quote=F)
    print("-fExpr : (oblig) an expression or a function object",quote=F)
    print("-mc    : (logical, def=TRUE) wether Monte Carlo info (pdf & df) is to be collected",
          quote=F)
    print(" ",quote=F)
    print('Returns: list(x.names, x.mu, x.u, x.pdf, x.df, fExpr)',quote=F)
    return(invisible())
  }
  
  if (class(fExpr)=='function')
    par.names =  names(formals(fExpr))
  else
    par.names = all.vars(fExpr)
  
  par.mu   = unlist(mget(par.names,
                         envir=.GlobalEnv,ifnotfound=NA))
  if(any(is.na(par.mu))) stop(paste0('Undefined variable:',
                                     par.names[is.na(par.mu)]))
  
  par.u    = unlist(mget(paste0(par.names,'.u'),
                         envir=.GlobalEnv,ifnotfound=NA))
  if(any(is.na(par.u))) stop(paste0('Undefined uncertainty:',
                             par.names[is.na(par.u)]))
  names(par.u)=par.names
  
  par.df  = unlist(mget(paste0(par.names,'.df'),
                        envir=.GlobalEnv,ifnotfound=Inf))  
  names(par.df)=par.names
  
  if(mc) {
    par.pdf  = unlist(mget(paste0(par.names,'.pdf'),
                           envir=.GlobalEnv,ifnotfound=NA))
    if(any(is.na(par.pdf))) stop(paste0('Undefined pdf:',
                                        par.names[is.na(par.pdf)]))
    names(par.pdf)=par.names    
  } else {
    par.pdf=c()
  }
  
  params=list(x.names=par.names, x.mu=par.mu, x.u=par.u,
              x.pdf=par.pdf, x.df=par.df, fExpr=fExpr)
  return (params)
}

