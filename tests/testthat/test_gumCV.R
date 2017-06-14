fExpr = expression(x1+x2)
fFunc = function(x1,x2) x1+x2
x.mu  = c(1,1)          
x.u   = c(0.1,0.1)      
x.pdf = c('norm','norm')
x.df  = c(Inf, Inf)
x.cor = matrix(c(1,-0.5,-0.5,1),ncol=2)
x.cov = outer(x.u, x.u, "*")*x.cor
names(x.u) = names(x.mu) = names(x.pdf) = names(x.df) = c('x1','x2')

test_that('Combination of Variances works for simple expression',
          {
            S = gumCV(fExpr,x.mu,x.u)
            expect_equal(S$y.mu, 2)
            expect_equal(S$y.u , 0.1*sqrt(2))
            expect_equal(S$anova[['x1']], 0.5)
          })

test_that('Combination of Variances works for simple function',
          {
            S = gumCV(fFunc,x.mu,x.u)
            expect_equal(S$y.mu, 2)
            expect_equal(S$y.u , 0.1*sqrt(2))
            expect_equal(S$anova[['x1']], 0.5)
          })


test_that('Combination of Variances works with correlation matrix',
          {
            S=gumCV(fExpr,x.mu,x.u,x.cor=x.cor)
            expect_equal(S$y.mu    , 2  )
            expect_equal(S$y.u     , 0.1)
            expect_equal(S$anovaCov, -1 )
          })

test_that('Combination of Variances works with covariance matrix',
          {
            S=gumCV(fExpr,x.mu,x.u,x.cov=x.cov)
            expect_equal(S$y.mu    , 2  )
            expect_equal(S$y.u     , 0.1)
            expect_equal(S$anovaCov, -1 )
          })

test_that('gumCV is silent when asked for',
          {
            expect_silent(gumCV(fExpr,x.mu,x.u,silent=TRUE))
          })

test_that('gumCV returns error when missing argument(s)',
          {
            expect_error(gumCV(fExpr))
          })