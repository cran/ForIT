#' Retrieve the 'pag' key to access INFCcatalog
#'
#' This function resolves species EPPOcodes retriving the 'pag' value, the page 
#' number in the original publication, used as a key by the package to archive 
#' the values of the parameters of the estimation functions.
#'
#' @noRd
#'
INFCpag <- function(EPPOcode, cod.na.error = T) {
  out <- tibble::tibble(EPPOcode) %>%
    dplyr::left_join(INFCspecies %>% dplyr::select(EPPOcode, pag)
                     , by = "EPPOcode")
  #quando la pagina è na esce err, se c è piu una o piu righe di err si genera un warning
  err <- out %>%
    unique() %>% #elimina duplicati
    dplyr::filter(pag %>% is.na()) #filtra pagina per na

  if (nrow(err) > 0) {
    print("ATTENTION: there are species codes not included in INFC list")
    print(err)
    # NO opzione di continuare?
    if (cod.na.error){
      stop("EPPOcode/s not recognized.
             Look-up INFCspecies table to retrieve recognized codes.
             Set 'cod.na.error = FALSE' to ignore)")
    }
  }
  return(out$pag)
}

#' verify if estimation point lays within the applicability domain of the function
#'
#' The function returns a (vector of) logical/s: TRUE in correspondence of a (d, h)
#' point for which the tables in the publication are not blank.
#'
#' @noRd
#'
InD0 <- function(d, h, pag){
  dom <- INFCf_domains %>%
    dplyr::filter(pag == !!pag)

  if(h <= (min(dom$htot.m)-.5) | h > (max(dom$htot.m)+.5)) return(FALSE)

  dom %>%
    dplyr::mutate(sh = abs(h - htot.m)) %>%
    dplyr::filter(sh == min(sh)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::mutate(ind1 = (d > (dbh.min-.5) & d <= (dbh.max+.5))) %>%
    dplyr::select(ind1) %>%
    unlist() %>%
    return()
}

#' verify if estimation point lays within the applicability domain of the function
#'
#' The function returns a (vector of) logical/s: TRUE in correspondence of a (d, h)
#' point for which the tables in the publication are not blank.
#'
#' @noRd
#'
InDomain <- function(d, h, pag){
  tibble::tibble(d, h, pag) %>%
    dplyr::transmute(InD = purrr::map2_lgl(d, h, InD0, pag = pag)) %>%
    unlist()
}

