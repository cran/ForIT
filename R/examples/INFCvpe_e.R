# COMPARE WITH Tabacchi (2011a) page 25 ----
(v <- INFCvpe("ACRCA", dbh.cm = 22, htot.m = 14))
# [1] 252.9581
# attr(,"pag")
# [1] 231
# attr(,"wrv")
# [1] 2.271e-05
# attr(,"Var_ea")
# [1] 33.17182
# attr(,"Var_ie")
# [1] 1075.883
# attr(,"InDomain")
# [1] TRUE

# Standard Error of the Estimate
see <- sqrt(attr(v, "Var_ie"))
# Degrees of freedom
df <- INFCcatalog$n_oss[INFCcatalog$pag == attr(v, "pag")] -
      INFCcatalog$n_par[INFCcatalog$pag == attr(v, "pag")]
# confidence level
p <- 95/100
# Confidence Interval Half Width
cihw <- qt(1-(1-p)/2, df) * see
cat(" *** Volume confidence interval (p = ", p*100, "%) is [", round(v, 1),
    " +/- ", round(cihw, 1), "] dm^3\n", sep = "")

# ESTIMATION OF PHYTOMASS ----
Quantities[5,] %>% as.data.frame()
#   quantity         quantity_definition
# 1      dw4 phytomass of the whole tree [kg]
tree_phy <- INFCvpe(c("ACRCA", "ALUCO"),
                    dbh.cm = c(22, 15),
                    htot.m = c(14, 16),
                    quantity = "dw4")
tree_phy
# [1] 185.1291  87.7970
# attr(,"pag")
# [1] 231 245
# attr(,"wrv")
# [1] 3.142e-05 2.104e-05
# attr(,"Var_ea")
# [1] 45.89002  9.12407
# attr(,"Var_ie")
# [1] 1488.5135  281.8072
# attr(,"InDomain")
# [1] TRUE TRUE

# PROCESSING A TALLY DATA-FRAME ----
tst_vol <- ForIT_test_data %>%
  dplyr::mutate(vol = INFCvpe(specie, d130, h_dendro),
                OutOfDomain = !attr(vol, "InDomain"))
tst_vol %>%
  dplyr::filter(OutOfDomain)
tst_vol %>%
  dplyr::filter(UC == "U1")

# SUMS AND direct ACCUARACY AGGREGATION (instead of via ?INFCvpeSUM) ----
df <- function(pag) return(
  INFCcatalog %>%
    dplyr::right_join(tibble::tibble(pag = !!pag), by = "pag") %>%
    dplyr::transmute(df = n_oss - n_par) %>%
    purrr::pluck(1)
)
p <- 95/100
tst_vol %>%
  dplyr::mutate(cihw = qt(1-(1-p)/2,
                          df(attr(vol, "pag"))) *
                  sqrt(attr(vol, "Var_ie"))
  ) %>%
  dplyr::filter(!OutOfDomain) %>%
  dplyr::group_by(specie) %>%
  dplyr::summarise(.groups = "drop",
                   est = sum(vol),
                   cihw = sqrt(sum(cihw^2)),
  ) %>%
  dplyr::left_join(INFCspecies %>% dplyr::select(EPPOcode, pag),
                   by = c("specie" = "EPPOcode")) %>%
  dplyr::left_join(INFCcatalog %>% dplyr::select(pag, section),
                   by = "pag") %>% 
  dplyr::select(-c(specie, pag)) %>% 
  dplyr::rename(specie = section) %>% 
  dplyr::mutate(dplyr::across(c("est", "cihw"), ~round(.x, 1))) %>% 
  dplyr::arrange(specie) %>%
  dplyr::select(specie, est, cihw) -> tab
tab[c(2,1,3),] %>% 
  t()
rm(tst_vol, tab, df)

