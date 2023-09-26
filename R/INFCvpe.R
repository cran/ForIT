#' Estimate bole volume or tree phytomass for individual stems, with associated accuracy info
#'
#' Using the functions developed for INFC 2005 (the 2005 Italian national forest inventory),
#' stem volume or tree compartment phytomass are estimated for each
#' \code{(EPPOcode, dbh.cm, htot.m)} input tuple.
#' Accompaining the main value, accuracy estimates are returned, as attributes.
#' The functions are documented in Tabacchi et al. (2011a)
#'
#' @details Output value will have following added attributes with estimates accuracy evaluations
#'   for each stem:\enumerate{
#'     \item \code{pag} - page number, referred to original source
#'     \item \code{wrv} - weighted residual variance
#'     \item \code{Var_ea} - variance for an estimated average\cr
#'     or variance for 'confidence interval' estimation,
#'     see \code{prediction.lm(.., interval = "confidence")}
#'     \item \code{Var_ie} - variance for an individual estimate\cr
#'     or 'prediction variance', (see \code{prediction.lm(.., interval = "prediction")} and
#'     Freese, 1964 - in:Tabacchi, 2011
#'     \item \code{InDomain} - logical indicating whether the (dbh, htot) point lies
#'     out of the domain explored by the experimental data (see 'INFCtabulate()')
#'   }
#'
#' @seealso [INFCvpe_summarise()] and functions related to [INFCvpe_sum()] to produce
#' estimates of aggregates with better accuracy evaluation
#'
#' @param EPPOcode Character vector of tree species code, as defined in EPPO database,
#'                 (See \code{INFCspecies()} and  \url{https://gd.eppo.int} )
#' @param dbh.cm   Numeric vector of stem/s breast height diameter (in cm)
#' @param htot.m   Numeric vector of tree total height/s (in m). Length equal to dbh.cm vector or one.
#'                 In this case same value will be replicated for all dbh.cm entries
#' @param quantity (default = \code{vol}) Character vector specifying required quantity,
#'                 one of \code{c("vol", "dw1" : "dw4")}. Use \code{qantities()} to retrieve
#'                 codes definitions. Length equal to dbh.cm vector or one.
#'                 In this case same value will be replicated for all dbh.cm entries
#'
#' @return The functions returns a Numeric vector of the same length
#'        of the \code{dbh.cm} argument, with accuracy info as attributes
#' @export
#' @example R/examples/INFCvpe_e.R
#'
INFCvpe <-
  function(EPPOcode, dbh.cm, htot.m, quantity = "vol") {

    # Verify that required quantities are recognised
    if (!all(quantity %in% Quantities$quantity)) {cat("ERROR - Unrecognized esitimation quantity")}

    assemble_D_0 <- function(x1, x2, n = 2) {
      # assemble the vector of the variable values, with appropriate length
      x <- c(1, x1)
      if(n == 3) x <- c(x, x2)
      attributes(x) <- list(dim = c(1, n))
      return(x)
    }

    # estimate required quantit-y/-ies
    out <- tibble::tibble(EPPOcode, dbh.cm, htot.m, quantity)  %>%
      # find text<pag> with param values for the required specie
      dplyr::left_join(dplyr::select(INFCspecies, EPPOcode, pag), #da INFCspecied prende EPPOcode e n pagina
                       by = c("EPPOcode")) %>%
      # read <param> values for the required <quantity
      dplyr::left_join(INFCparam,  by = c("pag", "quantity")) %>% #da INFCparam legge pagina per la quantità richiesta
      # read model specifications and domain boundaries
      dplyr::left_join(INFCcatalog, by = "pag") %>%
      # compute value, estimation error and reliability
      dplyr::mutate(D_0 = purrr::pmap(list(dbh.cm^2*htot.m, dbh.cm, n_par), assemble_D_0)
                    , T_0 = purrr::map2_dbl(D_0, bm,  ~ .x %*% .y)
                    # Variance of estimated average                                           [19]
                    , Var_ea = purrr::pmap_dbl(list(D_0, vcm),
                                               function(x, v, s) (x %*% as.matrix(v) %*% t(x) ))
                    # Variance of individual estimated average,                           in [22]
                    , Var_ie = purrr::pmap_dbl(list(D_0, vcm, wrv),
                                               function(x, v, s) (x %*% as.matrix(v) %*% t(x) + x %*% t(x) * s))
                    # (D_0[1,] %*% mvc %*% t(t(D_0[1,]))) + (saq*D_0[1,]^2)             #rel. [22]
                    , ind = purrr::pmap_lgl(list(dbh.cm, htot.m, pag), InDomain)
      )
    #  Progetto: per punti !ind fornire d0, h0, q0 valori del punto interno più vicino + delta_q%
    # Check that all species have been associated
    if(out$pag %>% is.na %>% any) {print("Warning: unrecognized species codes!")}

    out_col <- out$T_0
    attributes(out_col) <- list(pag = out$pag
                                , quantity = out$quantity
                                , wrv = out$wrv
                                , Var_ea = out$Var_ea
                                , Var_ie = out$Var_ie
                                , InDomain = out$ind)
    return(out_col)
  }
