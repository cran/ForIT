\dontrun{
Sezione <- function(EPPOcodes){
  # retrive 'Sezione' name, decoding EPPO codes
  INFCspecies %>% 
    dplyr::filter(EPPOcode %in% EPPOcodes) %>%
    dplyr::left_join(INFCcatalog,by = "pag")%>% 
    dplyr::select(section) %>% 
    purrr::pluck(1)
}

tst <- ForIT_test_data %>%
  dplyr::filter(UC != "U0") 
# select Tabachi et al. example data

tst %>%
  dplyr::group_by(specie) %>%
  INFCvpe_summarise("specie", "d130", "h_dendro") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(specie = Sezione(specie),
                dplyr::across(c("est", "cihw"), ~round(.x, 1))
  ) %>%
  dplyr::select(specie, est, cihw) %>%
  dplyr::arrange(specie) %>%
  dplyr::slice(2, 1, 3) %>% 
  t() %>% 
  provideDimnames(base = list(dimnames(.)[[1]], ""), unique=FALSE)
# Compare ForIT (ver 2) output
## specie "Aceri"  "Abete bianco" "Faggio"
## est    "4623.0" "4044.2"       "1079.4"
## cihw   "567.5"  "661.2"        "275.4" 
# with 'Tabella 2' in Tabacchi et al. (2011, pag. 27)
## specie "aceri"  "abete bianco" "faggio"
## est    "4623.0" "4044.2"       "1079.4"
## cihw   "567.4"  "662.4"        "279.2"

# Using 'INFCvpe_summarise()'

## Overall totals
tst %>%
  INFCvpe_summarise("specie", "d130", "h_dendro", quantity = c("vol", "dw4"))

## Group by dbh class ('cld')
tst %>%
  dplyr::mutate(cld = ceiling(d130/5)*5) %>%
  dplyr::group_by(UC, specie, cld) %>%
  INFCvpe_summarise("specie", "d130", "h_dendro")

## Group by sampling unit ('UC')
tst %>%
  dplyr::group_by(UC) %>%
  INFCvpe_summarise("specie", "d130", "h_dendro", quantity = "dw4")

# Using 'INFCvpeSUM' aggregation functions

## Esitmate 'dw4' phytomass, by sampling unit ('UC')
tst %>%
  dplyr::group_by(UC) %>%
  dplyr::summarise(
    n_stems = dplyr::n(),
    OoD = INFCvpe_OutOfDomain(specie, d130, h_dendro),
    dw4 = INFCvpe_sum(specie, d130, h_dendro, quantity = "dw4"),
    dw4_ConfInt = INFCvpe_ConfInt(specie, d130, h_dendro, quantity = "dw4")
  )

## Esitmate volume, by sampling unit ('UC')
tst %>%
  dplyr::group_by(UC) %>%
  dplyr::summarise(
    n_stems = dplyr::n(),
    OoD = INFCvpe_OutOfDomain(specie, d130, h_dendro),
    vol = INFCvpe_sum(specie, d130, h_dendro),
    vol_ConfInt = INFCvpe_ConfInt(specie, d130, h_dendro)
  )

rm(tst, Sezione)
}