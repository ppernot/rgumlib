fExpr = expression(x1+x2)
fFunc = function(x1,x2) x1+x2
nrun = 1000

# Direct method
x.mu  = c(1,1)          
x.u   = c(0.1,0.1)      
x.pdf = c('norm','norm')
x.df  = c(Inf, Inf)
x.cor = matrix(c(1,-0.9,-0.9,1),ncol=2)
x.cov = outer(x.u, x.u, "*")*x.cor
names(x.u) = names(x.mu) = names(x.pdf) = names(x.df) = c('x1','x2')

# collectParams method (variables have to be in Global env.)
x1 <<- 1 ; x1.u <<- 0.1; x1.pdf <<- 'norm'; x1.df <<- Inf
x2 <<- 1 ; x2.u <<- 0.1; x2.pdf <<- 'norm'; x2.df <<- Inf
paramsE = collectParams(fExpr)
paramsF = collectParams(fFunc)


test_that('collectParams properly gathers data for expression model',
          {
            S  = gumS1(fExpr,x.mu,x.u,x.pdf,x.df,nrun=nrun,
                       silent=TRUE)

            S1 = gumS1(paramsE$fExpr,paramsE$x.mu,paramsE$x.u,
                       paramsE$x.pdf,paramsE$x.df,nrun=nrun,
                       silent=TRUE)
            
            expect_equal(S1$y.mu, S$y.mu,
                         tolerance = 3*S$y.u/sqrt(nrun))
            expect_equal(S1$y.u, S$y.u,
                         tolerance = 3/sqrt(2*(nrun-1)),
                         scale = S$y.u)
          })



test_that('collectParams properly gathers data for expression model',
          {
            S  = gumS1(fFunc,x.mu,x.u,x.pdf,x.df,nrun=nrun,
                       silent=TRUE)
            
            S1 = gumS1(paramsF$fExpr,paramsF$x.mu,paramsF$x.u,
                       paramsF$x.pdf,paramsF$x.df,nrun=nrun,
                       silent=TRUE)
            
            expect_equal(S1$y.mu, S$y.mu,
                         tolerance = 3*S$y.u/sqrt(nrun))
            expect_equal(S1$y.u, S$y.u,
                         tolerance = 3/sqrt(2*(nrun-1)),
                         scale = S$y.u)
          })

