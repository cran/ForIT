#' Estimate tree bole volume or phytomass for stems groups, with associated accuracy info
#'
#' Cumulative estimation of the volume or phytomass of groups of trees is just
#' the summation of the values computed with \code{INFCvpe()}, but the computation of
#' accuracy estimates is improved using these summation functions.\cr
#' Two approaches are available.
#' \itemize{
#' \item Via \code{INFCvpe_summarise()} that processes and returns a data frame
#' \item or by following aggregation functions within a standard \code{summarise()}:
#'       \itemize{
#'       \item \code{INFCvpe_sum()}
#'       \item \code{INFCvpe_ConfInt()}
#'       \item \code{INFCvpe_OutOfDomain()}
#'       }
#' }
#'
#'Functions developed following Tabacchi et al. (2011), pages 23-26.
#'
#' @seealso [INFCvpe()] to compute individual estimates, with detailed accuracy evaluation
#'
#' @param in.data A dataframe (or tibble) containing tally data to be matched with
#'                  "EPPOcode_C", "dbh_C" and "htot_C" arguments
#' @param EPPOcode_C A string, the name of the column in \code{in.data} with the
#'                 species EPPO codes
#' @param dbh_C A string, the name of the column in \code{in.data} with the
#'            breast height diameter values
#' @param h_tot_C A string, the name of the column in \code{in.data} with the
#'              tree total height values
#' @param quantity (default = \code{"vol"}) A character vector specifying required
#'                 quantity/ies: \code{c("vol", "dw1" : "dw4")}.
#'                 Use \code{qantities()} to decode definitions.
#'                 \itemize{
#'                 \item for \code{INFCvpe_summarise()}, if \code{length(unique(quantity)) > 1},
#'                 rows in in.data will be replicated for each value.
#'                 \item for \code{INFCvpe_SUM functions}, length(quantity) must be 1
#'                 }
#' @param p (default \code{p = 95\%}) probability used to compute \code{cihw}
#'         (with length = 1 or length = length(dbh))
#'
#' OPZIONE 2  \code{INFCvpe_SUM functions}:
#'
#' @param EPPOcode A character vector with the species EPPO codes
#'         (with length = 1 or length = length(dbh))
#' @param dbh A numeric vector with the brest height diameter values
#' @param h_tot A numeric vector with the tree total height values
#'         (with length = 1 or length = length(dbh))
#'
#' @return \itemize{
#'         \item \code{INFCvpe_summarise()} returns a dataframe (tibble) with
#'          the grouping columns defined with \code{group_by()}, and the following columns:
#'            \itemize{
#'            \item \code{quantity}: as additional grouping column,
#'            \item \code{n} : number of trees in the group,
#'            \item \code{n_out} : the number of \code{(dbh, htot)} pairs that are 'out of the domain',
#'            \item \code{est} : the estimated value,
#'            \item \code{cihw} : confidence interval half width
#'            \item \code{p}: probability used computing \code{cihw}
#'            }
#'         \item \code{INFCvpe_SUM} - the functions of this family return a numeric vector,
#'         aggregating rows within the same group,
#'         \itemize{
#'            \item \code{INFCvpe_sum()} returns the sum of the estimated quantities,
#'            \item \code{INFCvpe_ConfInt()} returns 'confidence interval half width',
#'            \item \code{INFCvpe_OutOfDomain()} returns the number of 'out of domain'
#'               \code{(dhb, h_tot)} pairs included in the summation
#'            }
#'        }
#'
#' @example R/examples/INFCvpeSUM_e.R
#'
#' @name INFCvpeSUM
NULL

#' @export
#' @rdname INFCvpeSUM
INFCvpe_summarise<- function(in.data, EPPOcode_C, dbh_C, h_tot_C, quantity = "vol", p = .95 ){
  in.data %>%
    dplyr::mutate(
      EPPO = !!rlang::sym(EPPOcode_C),
      X2 := !!rlang::sym(dbh_C), # https://tidyeval.tidyverse.org/sec-why-how.html
      h = !!rlang::sym(h_tot_C),
      X1 = X2^2 * h
    ) %>%
    INFCvpe_agg(quantity, p) %>%
    return()
}

#' @export
#' @rdname INFCvpeSUM
INFCvpe_sum<- function(EPPOcode, dbh, h_tot, quantity = "vol" ){
  if (length(quantity) > 1)
    stop(paste("Only one 'quantity' is computed, too many required:",
               paste(unique(quantity), collapse =", ")))
  dplyr::tibble(EPPO = EPPOcode, X1 = dbh^2*h_tot, X2 = dbh, h = h_tot) %>%
    INFCvpe_agg(quantity, p = 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(est) %>%
    purrr::pluck(1) %>%
    return()
}

#' @export
#' @rdname INFCvpeSUM
INFCvpe_ConfInt<- function(EPPOcode, dbh, h_tot, quantity = "vol", p = .95 ){
  if (length(quantity) > 1)
    stop(paste("Only one 'quantity' is computed, too many required:",
               paste(unique(quantity), collapse =", ")))
  dplyr::tibble(EPPO = EPPOcode, X1 = dbh^2*h_tot, X2 = dbh, h = h_tot) %>%
    INFCvpe_agg(quantity, p) %>%
    dplyr::ungroup() %>%
    dplyr::select(cihw) %>%
    purrr::pluck(1) %>%
    return()
}

#' @export
#' @rdname INFCvpeSUM
INFCvpe_OutOfDomain<- function(EPPOcode, dbh, h_tot){
  dplyr::tibble(EPPO = EPPOcode, X1 = dbh^2*h_tot, X2 = dbh, h = h_tot) %>%
    INFCvpe_agg(quantity = "vol", p = 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(n_out) %>%
    purrr::pluck(1) %>%
    return()
}

##### common kernel ######

INFCvpe_agg<- function(in.data, quantity, p){

  if (dplyr::tibble(quantity = unique(quantity)) %>%
      dplyr::anti_join(Quantities, by = "quantity") %>%
      nrow() > 0)
    stop(paste("ERROR in quantity value:", paste(unique(quantity), collapse =", ")))

  if (identical(sort(quantity), sort(unique(quantity))))
    in.data <- in.data %>%
      dplyr::inner_join(dplyr::tibble(quantity = quantity), by = character())
  else
    if (length(quantity) == nrow(in.data))
      in.data <- in.data %>%
        dplyr::bind_cols(quantity)
    else
      stop(paste("ERROR in quantity contents:", paste(quantity, collapse =", ")))
    in.data %>%
      dplyr::mutate(pag = INFCpag(EPPO, cod.na.error = F)) %>%
      # dplyr::left_join(INFCcatalog %>% dplyr::select(pag, dom), by = "pag") %>%
      # # Nota - da valutare se non è opportuno spostare in InDomain
      # #        modificando la firma in (d, h, pag)
      # FATTO 15/7/2021
      dplyr::group_by(quantity, EPPO, .add = TRUE) %>%
      dplyr::summarise(.groups = "drop_last",
                       n = dplyr::n(),
                       n_out = sum(!purrr::pmap_lgl(list(X2, h, pag), InDomain)),
                       D = list(c(n, sum(X1), sum(X2))),
                       W = sum(X1^2)  #  eq [24] e poi inizio pag 24
      ) %>%
      dplyr::mutate(pag = INFCpag(EPPO, cod.na.error = F)) %>%
      dplyr::left_join(INFCparam, by = c("pag", "quantity")) %>%
      dplyr::left_join(INFCcatalog %>% dplyr::select(pag, n_par, n_oss), by = "pag") %>%
      dplyr::mutate(
        D = purrr::map2(D, n_par, ~.x[1:.y]),
        est = purrr::map2_dbl(D, bm, ~(.x %*% .y)), # estimated value
        cihw = purrr::pmap_dbl(list(D, vcm, W, wrv, p, n_oss, n_par),
                        function(D, vcm, W, wrv, p, n_oss, n_par) {
                          sqrt(D %*% as.matrix(vcm) %*% D + W * wrv) *
                            qt(1-(1-p)/2, n_oss-n_par)
                        })  # confidence interval half width
      ) %>%
      dplyr::select(-c(EPPO, pag, D, bm, vcm, W, wrv, n_oss, n_par)) %>%
      dplyr::group_by(quantity, .add = TRUE) %>%
      dplyr::summarise(.groups = "keep",
                       n = sum(n), n_out = sum(n_out),
                       est = sum(est), cihw = sqrt(sum(cihw^2)),
                       p = p) %>%
      return()
}
