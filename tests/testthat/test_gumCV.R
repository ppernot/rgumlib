x.mu = c(1,1)    ; names(x.mu) = c('x1','x2')
x.u  = c(0.1,0.1); names(x.u)  = c('x1','x2')

# Simple expression
fExpr = expression(x1+x2)
S=gumCV(fExpr,x.mu,x.u)
expect_equal(S$y.mu, 2)
expect_equal(S$y.u, 0.1*sqrt(2))
expect_equal(S$anova[['x1']], 0.5)

# Simple function
fExpr = function(x1,x2) x1+x2
S=gumCV(fExpr,x.mu,x.u)
expect_equal(S$y.mu, 2)
expect_equal(S$y.u, 0.1*sqrt(2))
expect_equal(S$anova[['x1']], 0.5)

# Correlation
fExpr = expression(x1+x2)
x.cor = matrix(c(1,-0.5,-0.5,1),ncol=2)
S=gumCV(fExpr,x.mu,x.u,x.cor=x.cor)
expect_equal(S$y.mu    , 2  )
expect_equal(S$y.u     , 0.1)
expect_equal(S$anovaCov, -1 )

# Covariance
x.cov = outer(x.u, x.u, "*")*x.cor
S=gumCV(fExpr,x.mu,x.u,x.cov=x.cov)
expect_equal(S$y.mu    , 2  )
expect_equal(S$y.u     , 0.1)
expect_equal(S$anovaCov, -1 )
