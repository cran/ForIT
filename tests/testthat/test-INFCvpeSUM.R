library(dplyr)
library(purrr)
# test table ----
tst <- function()
  tibble(
    UC = c("U1", "U1", "U1", "U1", "U1", "U1", "U1", "U1", "U1", "U1", "U2",
           "U2", "U2", "U2", "U2", "U2"),
    specie = c("ACROP","ACROP", "ACROP", "ACROP", "ACROP", "ACROP", "ACROP",
               "ACROP", "ACROP", "ACROP", "ABIAL", "ABIAL", "FAUSY", "FAUSY",
               "FAUSY", "ABIAL"),
    d130 = c(10, 15, 20, 30, 32, 24, 36, 40, 8, 18, 38, 52, 25, 30, 12, 5),
    h_dendro = c(7, 9, 12, 20, 21, 18, 21, 22, 8, 12, 21, 28, 16, 18, 10, 32)
  )

# summarize ----
## no grouping ----
test_that("INFCvpe_summarise works 0", {
  out <- tst() %>%
    INFCvpe_summarise("specie", "d130", "h_dendro") %>%
    mutate(across(c("est", "cihw"), ~round(.x, 3))) %>%
    as_tibble()
  expect_equal(out,
               tibble(quantity = "vol",
                      n = 16L,
                      n_out = 1L,
                      est = 9777.068,
                      cihw = 914.075,
                      p = 0.95)
  )
})

## grouping 1 ----
test_that("INFCvpe_summarise works 1", {
  out <- tst() %>%
    group_by(UC) %>%
    INFCvpe_summarise("specie", "d130", "h_dendro") %>%
    mutate(across(c("est", "cihw"), ~round(.x, 3))) %>%
    as_tibble()
  expect_equal(out,
               tibble(UC = c("U1", "U2"),
                      quantity = c("vol", "vol"),
                      n = c(10L, 6L),
                      n_out = c(0L, 1L),
                      est = c(4622.971, 5154.097),
                      cihw = c(567.457, 716.607),
                      p = c(0.95, 0.95)
               )
  )
})

# SUM functions ----
## grouping 1 ----
test_that("INFCvpe_SUM functions work 1", {
  out <- tst() %>%
    group_by(UC) %>%
    summarise(
              n_stems = n(),
              OoD = INFCvpe_OutOfDomain(specie, d130, h_dendro),
              dw4 = INFCvpe_sum(specie, d130, h_dendro, quantity = "dw4"),
              dw4_ConfInt = INFCvpe_ConfInt(specie, d130, h_dendro, quantity = "dw4")
              ) %>%
    mutate(across(c("dw4", "dw4_ConfInt", "OoD"), ~round(.x, 3))) %>%
    as_tibble()
  expect_equal(out, tibble(
                      UC = c("U1", "U2"),
                      n_stems = c(10L, 6L),
                      OoD = c(0,1),
                      dw4 = c(3339.843, 2890.876),
                      dw4_ConfInt = c(667.459, 694.152)
                      )
  )
})

## grouping 2 ----
test_that("INFCvpe_SUM functions work 2", {
  out <- tst() %>%
    group_by(UC, specie) %>%
    summarise(.groups = "drop",
      n_stems = n(),
      OoD = INFCvpe_OutOfDomain(specie, d130, h_dendro),
      dw4 = INFCvpe_sum(specie, d130, h_dendro, quantity = "dw4"),
      dw4_ConfInt = INFCvpe_ConfInt(specie, d130, h_dendro, quantity = "dw4")
    ) %>%
    mutate(across(c("dw4", "dw4_ConfInt", "OoD"), ~round(.x, 3))) %>%
    as_tibble()
  expect_equal(out, tibble(
                       UC = c("U1", "U2", "U2"),
                       specie = c("ACROP", "ABIAL", "FAUSY"),
                       n_stems = c(10L, 3L, 3L),
                       OoD = c(0, 1, 0),
                       dw4 = c(3339.843, 2035.332, 855.544),
                       dw4_ConfInt = c(667.459, 670.587, 179.333)
                      )
  )
})
rm(tst)
