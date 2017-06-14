test_that('SAPlot identical to reference plot',
          {
            set.seed(1234)
            S = matrix(rnorm(300),ncol=3)
            png(filename = "testSAPlot.png")
            SAPlot(S)
            dev.off()
            # getFingerprint("testSAPlot.png")
            expect_true(
              visualTest::isSimilar(file = "testSAPlot.png",
                        fingerprint = "FC349BC2A7C381C9",
                        threshold = 0.1
              )
            )
          })

