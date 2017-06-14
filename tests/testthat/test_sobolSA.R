fExpr = expression(x1+x2)
x.mu  = c(1,1)          
x.u   = c(0.1,1.0)      
x.pdf = c('norm','norm')
x.df  = c(Inf, Inf)
names(x.u) = names(x.mu) = names(x.pdf) = names(x.df) = c('x1','x2')

test_that('sobolSA results fit Combination of Variances for linear model',
          {
            nrun = 1000
            Sref = gumCV(fExpr,x.mu,x.u,silent=TRUE)
            SA   = sobolSA(fExpr,x.mu,x.u,x.pdf,x.df,graph=FALSE)
            expect_equal(SA$S['x1',],Sref$anova[['x1']], 
                         tolerance = 3/sqrt(nrun))
            expect_equal(SA$T['x1',],Sref$anova[['x1']], 
                         tolerance = 3/sqrt(nrun))
          })

