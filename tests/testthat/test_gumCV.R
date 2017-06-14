
fExpr = expression(x1+x2)
x.mu = c(1,1); names(x.mu)=c('x1','x2')
x.u = c(0.1,0.1); names(x.u)=c('x1','x2')
x.pdf = c('unif','triangle'); names(x.pdf)=c('x1','x2')
S=gumCV(fExpr,x.mu,x.u,silent=TRUE)

expect_equal(S$y.mu, 2)
expect_equal(S$y.u, 0.1*sqrt(2))
expect_equal(S$anova[['x1']], 0.5)
