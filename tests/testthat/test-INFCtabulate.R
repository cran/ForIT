test_that("INFCtabulate works", {
  tbls0 <- INFCtabulate(c("ABIAL", "ACRCA"), print_tab = F)
  tbls <- structure(list(EPPOcode = c("ABIAL", "ACRCA"),
                         quantity = c("vol", "vol"),
                         pag = c(33, 231), table = list(structure(c(4.9, 21.1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 7.7,
                                                                    32.4, 72.3, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 43.8,
                                                                    97.8, 172.6, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 123.3,
                                                                    218, 339.2, 486.9, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 263.4,
                                                                    410.2, 589, 800.1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 481.1,
                                                                    691.2, 939.1, 1224.9, 1548.5, NA, NA, NA, NA, NA, NA, NA, NA,
                                                                    NA, 793.3, 1078.2, 1406.5, 1778.3, 2193.7, NA, NA, NA, NA, NA,
                                                                    NA, NA, NA, 895.5, 1217.2, 1588.1, 2008.2, 2477.5, 2995.9, 3563.6,
                                                                    4180.4, NA, NA, NA, NA, NA, NA, 1356.3, 1769.7, 2238.1, 2761.2,
                                                                    3339.3, 3972.2, 4660, NA, NA, NA, NA, NA, NA, NA, 1951.3, 2467.9,
                                                                    3045, 3682.7, 4380.8, 5139.5),
    .Dim = c(13L, 10L), .Dimnames = list(
      dbh.cm = c("5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55", "60", "65"),
      htot.m = c("5", "8", "11", "14", "17", "20", "23", "26", "29", "32"))),

    structure(c(6.3, 20.2, NA, NA, NA, NA, NA, NA, 9.1, 31.4, 68.4, NA, NA,
                NA, NA, NA, NA, 42.5, 93.5, 164.9, NA, NA, NA, NA, NA, NA, 118.5,
                209.3, 326.2, 468.9, NA, NA, NA, NA, NA, 253.8, 395.7, 569, 773.9,
                NA, NA, NA, NA, 298.3, 465.2, 669.2, 910.2, 1188.3, NA, NA, NA,
                NA, NA, 769.3, 1046.5, 1366.3),
              .Dim = 8:7, .Dimnames = list(
                dbh.cm = c("5", "10", "15", "20", "25", "30", "35", "40"),
                htot.m = c("5", "8", "11", "14", "17", "20", "23"))))),
    row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame")
    )
  testthat::expect_equal(tbls0, tbls)
})
