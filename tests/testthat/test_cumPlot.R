# test_that('cumPlot identical to reference plot',
#           {
#             set.seed(1234)
#             S = rnorm(1000)
#             png(filename = "testCumPlot.png")
#             cumPlot(S)
#             dev.off()
#             # getFingerprint("testCumPlot.png")
#             expect_true(
#               visualTest::isSimilar(file = "testCumPlot.png",
#                         fingerprint = "80F0FF03F887FA18",
#                         threshold = 0.1
#               )
#             )
#           })
# 
