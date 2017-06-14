# test_that('parPlot identical to reference plot',
#           {
#             set.seed(1234)
#             S = matrix(rnorm(300),ncol=3)
#             png(filename = "testParPlot.png")
#             parPlot(S)
#             dev.off()
#             # getFingerprint("testParPlot.png")
#             expect_true(
#               visualTest::isSimilar(file = "testParPlot.png",
#                         fingerprint = "B6C48D93CE99813B",
#                         threshold = 0.1
#               )
#             )
#           })
# 
