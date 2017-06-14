nrun = 10000
tolFactor=3 # 99% proba

x.mu  = 1          
x.u   = 1      
x.df  = Inf

for (x.pdf in c('norm','unif','triangle','arcsine'))
  test_that('xSample generates sample with prescribed distribution 
             mean and standard deviation',
            {
              X = xSample(M=nrun,x.mu,x.u,x.pdf,x.df)
              hist(X)
              expect_equal(mean(X), x.mu, tolerance = tolFactor*x.u/sqrt(nrun))
              expect_equal(sd(X), x.u, tolerance = tolFactor/sqrt(2*(nrun-1)), 
                           scale = x.u)
            }
  )

x.pdf = 'stud'
x.df  = 10
test_that('xSample generates sample with prescribed Student distribution',
          {
            X = xSample(M=nrun,x.mu,x.u,x.pdf,x.df)
            hist(X)
            expect_equal(mean(X), x.mu, tolerance = tolFactor*x.u/sqrt(nrun))
            expect_equal(sd(X), x.u*sqrt(x.df/(x.df-2)), 
                         tolerance = tolFactor/sqrt(2*(nrun-1)), 
                         scale = x.u*sqrt(x.df/(x.df-2)))
          }
)
