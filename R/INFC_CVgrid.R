#' 'ForIT'-package Auxiliary pre-computed data - Ver.2
#'
#' A large dataset used by 'INFCaccuracyPlot0()' to speed the production of a
#' fine resolution CV surface for the plots.
#'
#' @format A data frame with 167560 rows and 7 variables:
#' \describe{
#'   \item{pag}{see 'INFCcatalog' primary key}
#'   \item{quantity}{see 'Quantities'}
#'   \item{dbh}{trunk diameter at breast height (dbh), in cm}
#'   \item{h_tot}{tree height, in m}
#'   \item{cv_ie}{coefficient of variation for an individual estimate}
#'   \item{cv_ea}{coefficient of variation for the estimate of an average}
#'   \item{est}{estimated value for the selected 'quantity'. 
#'   See 'INFCvpe()' for more details}
#'   \item{n_par}{number of parameters that the function requires}
#'   \item{inD}{is the (dbh, htot) point within the function domain?}
#' }
#'
#' @details
#' The dataset is produced by the following code.
#'```
#'  Populate_INFC_CVgrid <- function() {
#'  INFCcatalog %>%
#'    select(pag) %>%
#'    inner_join(INFCspecies %>%
#'                 select(pag, EPPOcode),
#'               by = "pag") %>%
#'    group_by(pag) %>%
#'    summarise(EPPOcode = first(EPPOcode),
#'              .groups = "drop") %>%
#'    inner_join(Quantities %>% select(quantity), by = character()) %>%
#'    mutate(grid.k = pmap(list(pag, EPPOcode, quantity), compute_grid0)) %>%
#'    select(-EPPOcode) %>%
#'    unnest(cols = c(grid.k)) %>%
#'    return()
#'  }
#'```
#'
"INFC_CVgrid"
