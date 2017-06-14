test_that('ECIPlot identical to reference plot',
          {
            set.seed(1234)
            S = rnorm(1000)
            png(filename = "testECIPlot.png")
            ECIPlot(S)
            dev.off()
            expect_true(
              visualTest::isSimilar(file = "testECIPlot.png",
                        fingerprint = "80B5333BF521BF30",
                        threshold = 0.1
              )
            )
          })
