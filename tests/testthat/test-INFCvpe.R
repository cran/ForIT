test_that("INFCvpe works", {

  vol <- INFCvpe("ACRCA", dbh.cm = 22, htot.m = 14) # pag 24
  testthat::expect_equal(round(as.numeric(vol), 1), 253.0)

  vol <- INFCvpe(c("ACRCA", "ALUCO"), dbh.cm = c(22, 15), htot.m = c(14, 16))
  testthat::expect_equal(round(as.numeric(vol), 1), c(253.0, 139.6))

  vol <- INFCvpe("ACRCA", dbh.cm = c(22, 15), htot.m = 14)
  testthat::expect_equal(round(as.numeric(vol), 1), c(253, 118.5))

  testthat::expect_equal(attributes(vol), list(
    pag = c(231L, 231L),                   # pag. 24, 25 |
    quantity = c("vol", "vol"),
    wrv = c(2.271e-05,  2.271e-05),        # sqrt()
    Var_ea = c(33.171823, 6.346804),       #   5.76      |
    Var_ie = c(1075.8828,  231.6868),      #  32.80      |
    InDomain = c(TRUE, TRUE)
  ))
})
