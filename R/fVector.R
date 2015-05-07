fVector = function(X,fExpr) {
  # Return a vector (1 valued function)
  if(class(fExpr)=='function')
    apply(X, 1, function(x) do.call(fExpr,as.list(x)) ) 
  else
    apply(X, 1, function(x) eval(fExpr,as.list(x)) ) 
}

fDeriv = function(X,fExpr) {
  # Return a matrix
  if(class(fExpr)=='function') {
    fd =  apply(X, 1, 
                function(x) {
                  fun=function(x) do.call(fExpr,as.list(x))
                  numDeriv::grad(fun,x)
                }
              )
  } else {
    fd = apply(X, 1, 
                function(x) {
                  df = eval(deriv(fExpr,colnames(X)),as.list(x))
                  as.vector(attr(df, "gradient"))
                }
              )
  }
  return(matrix(fd,ncol=ncol(X),byrow=TRUE))
}

