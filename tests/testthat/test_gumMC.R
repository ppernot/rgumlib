fExpr = expression(x1+x2)
x.mu  = c(1,1)          
x.u   = c(0.1,0.1)      
x.pdf = c('norm','norm')
x.df  = c(Inf, Inf)
x.cor = matrix(c(1,-0.9,-0.9,1),ncol=2)
x.cov = outer(x.u, x.u, "*")*x.cor
names(x.u) = names(x.mu) = names(x.pdf) = names(x.df) = c('x1','x2')

test_that('gumS1 results fit Combination of Variances for linear model',
          {
            nrun = 1000
            Sref = gumCV(fExpr,x.mu,x.u)
            S    = gumS1(fExpr,x.mu,x.u,x.pdf,x.df,nrun=nrun)
            expect_equal(S$y.mu, Sref$y.mu, 
                         tolerance = 3*Sref$y.u/sqrt(nrun))
            expect_equal(S$y.u, Sref$y.u, 
                         tolerance = 3/sqrt(2*(nrun-1)), 
                         scale = Sref$y.u)
          })

test_that('gumS1 results fit Combination of Variances for linear model
          with correlation matrix',
          {
            nrun  = 1000
            Sref = gumCV(fExpr,x.mu,x.u,x.cor=x.cor)
            S    = gumS1(fExpr,x.mu,x.u,x.pdf,x.df,x.cor=x.cor,nrun=nrun)
            expect_equal(S$y.mu, Sref$y.mu, 
                         tolerance = 3*Sref$y.u/sqrt(nrun))
            expect_equal(S$y.u, Sref$y.u, 
                         tolerance = 3/sqrt(2*(nrun-1)), 
                         scale = Sref$y.u)
          })

test_that('gumS1 results fit Combination of Variances for linear model
          with covariance matrix',
          {
            nrun  = 1000
            Sref = gumCV(fExpr,x.mu,x.u,x.cov=x.cov)
            S    = gumS1(fExpr,x.mu,x.u,x.pdf,x.df,x.cov=x.cov,nrun=nrun)
            expect_equal(S$y.mu, Sref$y.mu, 
                         tolerance = 3*Sref$y.u/sqrt(nrun))
            expect_equal(S$y.u, Sref$y.u, 
                         tolerance = 3/sqrt(2*(nrun-1)), 
                         scale = Sref$y.u)
          })

test_that('gumS1 results fit Combination of Variances for linear model
           and adaptative algorithm',
          {
            Sref = gumCV(fExpr,x.mu,x.u,x.cov=x.cov)
            S    = gumS1(fExpr,x.mu,x.u,x.pdf,x.df,x.cov=x.cov,
                         adapt = TRUE)
            nrun = length(S$Y)
            expect_equal(S$y.mu, Sref$y.mu, 
                         tolerance = 3*Sref$y.u/sqrt(nrun))
            expect_equal(S$y.u, Sref$y.u, 
                         tolerance = 3/sqrt(2*(nrun-1)), 
                         scale = Sref$y.u)
          })

test_that('gumS1 results fit Combination of Variances for linear model
           and adaptative algorithm with 2 digits requirement',
          {
            Sref = gumCV(fExpr,x.mu,x.u,x.cov=x.cov)
            S    = gumS1(fExpr,x.mu,x.u,x.pdf,x.df,x.cov=x.cov,
                         adapt = TRUE, ndig = 2, nrunMax = 1e5)
            nrun = length(S$Y)
            expect_equal(S$y.mu, Sref$y.mu, 
                         tolerance = 3*Sref$y.u/sqrt(nrun))
            expect_equal(S$y.u, Sref$y.u, 
                         tolerance = 3/sqrt(2*(nrun-1)), 
                         scale = Sref$y.u)
          })

test_that('gumS2 results fit Combination of Variances for linear model
           with 2 digits requirement',
          {
            Sref = gumCV(fExpr,x.mu,x.u,x.cov=x.cov)
            S    = gumS2(fExpr,x.mu,x.u,x.pdf,x.df,x.cov=x.cov,
                         ndig = 2, nrunMax = 1e5)
            nrun = length(S$Y)
            expect_equal(S$y.mu, Sref$y.mu, 
                         tolerance = 3*Sref$y.u/sqrt(nrun))
            expect_equal(S$y.u, Sref$y.u, 
                         tolerance = 3/sqrt(2*(nrun-1)), 
                         scale = Sref$y.u)
          })