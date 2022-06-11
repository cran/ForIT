#' Reproduce the tables that Tabacchi et al. published in 2011
#'
#' The tables published in the work on which this package is based, convey a very
#' relevant part of the information produced: printed numbers serve as reference to
#' verify that coded functions return expected results and, more specifically, empty
#' spaces in the printed tables signal function applicability domain. In other words,
#' measurement data used to estimate function coefficients values, cover only the
#' portion of the \code{(dbh, htot)} plane where numbers are printed.
#'
#' @param EPPOcode tree species code defined by the
#'  EPPO database \url{https://gd.eppo.int/search}.
#'  Lookup 'INFCspecies' dataframe to retrieve recognized codes.
#' @param quantity for each species (or species group) different
#'  quantities can be estimated. Quantity's definitions and Ids are exposed by
#'  the 'Quantities' dataframe. Default value is "vol", estimation of timber volume.
#' @param dbh.by ...
#' @param htot.by increment value between rows
#'  (respectively columns) expressed in 'cm' (respectively 'm') for \code{dbh}
#'  (respectively total height, \code{htot}). Default 5 cm (respectively 1 m).
#' @param digits number of decimal digits to expose in the table.
#'  Default one decimal digit.
#' @param print_tab defaults to TRUE in order to produce a text output. If set
#'  to FALSE no printing will occur (see 'return')
#' @return
#'  Function principal output is the printout of the volume or phytomass tables.
#'  If print_tab = FALSE, the function will only return a tibble with a list column
#'  containing the tabulation of the required estimation equation/s.
#'  Using default values, tables will be identical (or anyway similar) to the 
#'  corresponding published tables, with white (NA) cells delimiting the domain of
#'  applicability of the equation.
#'
#' @examples
#' \dontrun{
#'   INFCtabulate(c("ABIAL", "ACRCA"),
#'                 quantity = "vol",
#'                 dbh.by = 5,
#'                 htot.by = 3,
#'                 digits = 1)
#'   # EPPO code:  ABIAL  - pag =  33  - quantity =  vol
#'   #     htot.m
#'   # dbh.cm    7    10    13    16     19     22     25     28     31
#'   # 9  23.2  32.4    NA    NA     NA     NA     NA     NA     NA
#'   # 14   NA  77.9 100.2    NA     NA     NA     NA     NA     NA
#'   # 19   NA 142.3 183.3 224.3     NA     NA     NA     NA     NA
#'   # 24   NA    NA 291.1 356.4  421.8  487.2     NA     NA     NA
#'   # 29   NA    NA    NA 518.9  614.3  709.8  805.2     NA     NA
#'   # 34   NA    NA    NA 711.6  842.8  974.0 1105.2 1236.4     NA
#'   # 39   NA    NA    NA    NA 1107.2 1279.8 1452.5 1625.1 1797.7
#'   # 44   NA    NA    NA    NA     NA 1627.2 1847.0 2066.7 2286.5
#'   # 49   NA    NA    NA    NA     NA 2016.3 2288.8 2561.4 2833.9
#'   # 54   NA    NA    NA    NA     NA     NA 2778.0 3109.0 3439.9
#'   # 59   NA    NA    NA    NA     NA     NA 3314.4 3709.5 4104.6
#'   # 64   NA    NA    NA    NA     NA     NA     NA 4363.1 4828.0
#'   
#'   
#'   # ---
#'   #   EPPO code:  ACRCA  - pag =  231  - quantity =  vol
#'   # htot.m
#'   # dbh.cm  7.5  10.5  13.5  16.5   19.5   22.5
#'   # 9.5  26.8  36.8    NA    NA     NA     NA
#'   # 14.5   NA  83.6 106.9    NA     NA     NA
#'   # 19.5   NA 149.7 192.0 234.3  276.6     NA
#'   # 24.5   NA    NA 302.2 369.0  435.7     NA
#'   # 29.5   NA    NA    NA 534.2  631.0  727.8
#'   # 34.5   NA    NA    NA 729.9  862.4  994.8
#'   # 39.5   NA    NA    NA    NA 1129.9 1303.5
#'   # ---
#'   }
#'
#' @export
#'
INFCtabulate <-
  function(EPPOcode,
           quantity = "vol",
           dbh.by = 5,
           htot.by = 3,
           digits = 1,
           print_tab = T) {
    # Retrieve the key to functions data 'pag'
    #  (and eventually stop with ERROR if species cod is not recognized!)
    out0 <- tibble::tibble(EPPOcode, quantity) %>%
      dplyr::mutate(pag = INFCpag(EPPOcode, cod.na.error = T))

    # Internal functions
    ## compute the matrix of 'InDomain' observations
    dom_boundary2matrix <- function(pag,
                                    xstep = 5,
                                    ystep = xstep) {
      dom <- INFCf_domains %>%
        dplyr::filter(pag == !!pag)
      d.min <- min(dom$dbh.min); d.max <- max(dom$dbh.max)
      h.min <- min(dom$htot.m); h.max <- max(dom$htot.m)



      mat <- expand.grid(
        dbh.cm = seq(floor(d.min), ceiling(d.max), xstep), #arrotonda al valore intero precedente o successivo
        htot.m = seq(floor(h.min), ceiling(h.max), ystep)
      ) %>%
        dplyr::mutate(ind = purrr::map2_lgl(dbh.cm, htot.m, InDomain, pag = pag)) %>%
        dplyr::filter(ind) %>%
        dplyr::select(dbh.cm, htot.m)
    }
    ## reshape df table to matrix
    tab_df2mat <- function(tab_df) {
      tabmat <- tab_df %>%
        tidyr::pivot_wider(names_from = htot.m, values_from = qty) %>%
        dplyr::select(-dbh.cm) %>%
        as.matrix() %>%
        round(digits) %>%
        `dimnames<-`(list(
          dbh.cm = tab_df$dbh.cm %>% unique(),
          htot.m = tab_df$htot.m %>% unique()
        ))
    }

    # main
    # - retrive 'domain/s' from Catalog, transform to matrix
    out_dom <- out0 %>%
      dplyr::select(pag) %>%
      unique() %>%
      dplyr::mutate(mat = purrr::map(pag, dom_boundary2matrix,
                                     xstep = dbh.by, ystep = htot.by
      )) %>%
      dplyr::select(pag, mat)

    # -
    out <- out0 %>%
      dplyr::left_join(out_dom, by = "pag") %>%
      dplyr::mutate(
        q = purrr::pmap(
          list(EPPOcode, quantity, mat),
          function(s, q, m) {
            dplyr::mutate(m,qty = INFCvpe(s, m$dbh.cm, m$htot.m, q)
            )
          }
        ),
        table = purrr::map(q, tab_df2mat)
        # attributes(tbl) <- c(attributes(tbl),
        #       EPPOcode = EPPOcode, quantity = quantity, pag = pag)
      ) %>%
      dplyr::select(-c(mat, q))
    if (print_tab) {
      for (t in 1:nrow(out)) {
        cat("EPPO code: ", out$EPPOcode[t], " - pag = ", out$pag[t],
            " - quantity = '", quantity[t], "'\n")
        print(out$table[[t]])
        cat("   ---\n")
      }
    } else return(out)
  }
