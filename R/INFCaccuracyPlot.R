#' Plotting the accuracy and reliability region (or 'domain') of the volume and
#' phytomass estimation functions
#'
#' Volume and phytomass functions are tabulated in Tabacchi et al. (2011a).
#' The tabulation covers a limited region of the \code{dbh} by \code{h_tot} rectangle.
#' This region is the "domain" of the reliable estimates, based on the
#' distribution of the sample trees used to calibrate the functions.
#' The coefficient of variation (CV = standard_deviation / estimate) is computed
#' and plotted (as 'filled contours') for the whole rectangular area,
#' the limits of the region of reliable estimates (the "domain"), is
#' superimposed as a light colored line. Function output is a \code{ggplot} object that can be used
#' by its self or as a background on top of which the user can plot his/her data to
#' verify eventual accuracy or reliability problems.\cr
#' Two functions are available.
#' \itemize{
#' \item \code{INFCaccuracyPlot()} - allows the plots to be fully customized but, beware,
#'                   all values required for the 'fill' will be computed and,
#'                   at finer resolution, the process can be slow.
#' \item \code{INFCaccuracyPlot0()} - produces, much faster, the plots at the finest
#'                   resolution, using pre-calculated values stored in
#'                   a specific auxiliary dataframe (see \code{\link{INFC_CVgrid}}),
#'                   necessarily leaving less customization freedom.
#' }
#' (** \code{compute_grid0()} - is an internal function exported for the sake of
#'                   the \code{Populate_INFC_CVgrid()} function **)
#'
#' @param EPPOcod A string, one of the EPPO tree species codes listed in
#'          \code{INFCspecies} table (Reference: \url{https://gd.eppo.int} )
#' @param quantity (optional) A string specifying the quantity to be estimated,
#'                 one of \code{c("vol", "dw1" : "dw4")}.
#'                 Lookup \code{Qantities} table to retrieve codes definitions.
#'                 Defaults to "vol".
#' @param ie.Var (optional) Logical. Choose variance estimator:
#'          \itemize{
#'            \item \code{TRUE} = variance for an 'individual estimate'
#'            \item \code{FALSE} = variance for the 'estimate of an average' value
#'           Default \code{ie.Va = FALSE}}
#' @param cv.ul (optional) Numeric. Cutoff CV level for the plot. Defaults to 0.1
#' @param fixed (optional) Logical. Contour plot breaks:
#'          \itemize{
#'            \item \code{TRUE} = break levels are fixed
#'            \item \code{FALSE} = breaks are tailored to the specific case
#'            (in order to split the values within the range of interest \[0, \code{cv.ul}\],
#'            into equally numerous bins)
#'           Default \code{fixed = TRUE}}
#' @param plot.est (optional) Logical. Add the 'estimated quantity' layer as
#'          contour lines. Default \code{plot.est = FALSE}
#' @param dbh.step (optional) Numeric. Computation with smaller step produces a
#'          plot with better resolution but increases consistently computation time
#'          (see \code{INFCaccuracyPlot0()}). To reduce computation time, the step
#'          defaults to 5.
#' @param htot.step (optional) Numeric. As for dbh.
#' @param dbh.buf (optional) Numeric. Extra space in the plot beyond the 'domain'.
#'          Default: 1
#' @param htot.buf (optional) Numeric. As for dbh.
#' @param pag for the internal function \code{compute_grid0()}
#'
#' @return \code{INFCaccuracyPlot} The function returns a ggplot object.
#'
#' @example R/examples/INFCaccuracyPlot_e.R
#'
#' @name PlottingINFCaccuracy
NULL

#' @export
#' @rdname PlottingINFCaccuracy
INFCaccuracyPlot <- function(EPPOcod,
                             quantity = "vol",
                             ie.Var = FALSE,
                             # Scelta numeratore del CV, default Var_ea
                             cv.ul = 0.1,
                             # Ultimo livello di CV% da considerare
                             fixed = TRUE,
                             # Breaks fissi ovvero calcolati se FALSE
                             plot.est = FALSE,
                             # Add estimated quantity contur lines. REQUIRES 'metR' package!!
                             dbh.step = 5,
                             htot.step = dbh.step,
                             dbh.buf = 1,
                             htot.buf = dbh.buf) {

  accuracyPlot(EPPOcod, quantity, ie.Var, customPlot = TRUE,
               fixed, cv.ul = cv.ul,
               dbh.step, htot.step, dbh.buf, htot.buf,
               plot.est)
}

#' @export
#' @rdname PlottingINFCaccuracy
INFCaccuracyPlot0 <- function(EPPOcod,
                              quantity = "vol",
                              ie.Var = FALSE, # Scelta numeratore del CV, default Var_ea
                              cv.ul = 0.1,
                              # Ultimo livello di CV% da considerare
                              fixed = TRUE,
                              # Breaks fissi ovvero calcolati se FALSE
                              plot.est = FALSE
                              # Add estimated quantity contur lines.
                              # REQUIRES 'metR' package!!
)
{
  accuracyPlot (EPPOcod, quantity, ie.Var, customPlot = FALSE,
                cv.ul, fixed,
                dbh.step = NA,
                htot.step = NA,
                dbh.buf = NA,
                htot.buf = NA,
                plot.est)
}

#' @export
#' @rdname PlottingINFCaccuracy
# --- internal function, exported for teh sake of 'Populate_INFC_CVgrid()'---
compute_grid0 <- function(pag, EPPOcod, quantity){
  system(paste0("echo 'now processing: ", pag, "-", quantity, "| '"))
  compute_grid(pag, EPPOcod, quantity = quantity,
               dbh.step = 1,
               htot.step = 1,
               dbh.buf = 1,
               htot.buf = 1)
}

# NOT exported functions

compute_grid <- function(pag, EPPOcod, quantity, ie.Var,
                         dbh.step, htot.step, dbh.buf, htot.buf){
  bbx.EPPO <- INFCf_domains %>%
    dplyr::filter(pag == !!pag) %>%
    dplyr::summarise(dbh.min = min(dbh.min), dbh.max = max(dbh.max),
              htot.min = min(htot.m), htot.max = max(htot.m)) %>%
    purrr::as_vector()
  d.grid <- seq(bbx.EPPO["dbh.min"], bbx.EPPO["dbh.max"], dbh.step)
  if(max(d.grid)<  bbx.EPPO["dbh.max"]) d.grid <- c(d.grid,  bbx.EPPO["dbh.max"])
  d.grid <- c(min(d.grid) - dbh.buf, d.grid, max(d.grid) + dbh.buf)
  h.grid <- seq(bbx.EPPO["htot.min"],bbx.EPPO["htot.max"], htot.step)
  if(max(h.grid)<  bbx.EPPO["htot.max"]) h.grid <- c(h.grid,  bbx.EPPO["htot.max"])
  h.grid <- c(min(h.grid) - htot.buf, h.grid, max(h.grid) + htot.buf)

  dropAttributes <- function(x) {
    attributes(x) <- NULL
    return(x)
  }

  grid.k <- tidyr::expand_grid(dbh = d.grid, h_tot = h.grid) %>%
    dplyr::mutate(est = INFCvpe(
      EPPOcod,
      dbh.cm = dbh,
      htot.m = h_tot,
      quantity = quantity
    )) %>%
    dplyr::bind_cols(attributes(.$est) %>%
                       tibble::as_tibble()) %>%
    dplyr::select(-c(pag, wrv)) %>%
    dplyr::mutate(
      est = dropAttributes(est),
      # Var = ie.Var * Var_ie+!ie.Var * Var_ea,
      # Var = case_when(ie.Var  ~ Var_ie, TRUE  ~ Var_ea),
      cv_ie = sqrt(Var_ie) / est,
      cv_ea = sqrt(Var_ea) / est
    ) %>%
    dplyr::select(dbh, h_tot, cv_ie, cv_ea, est)
}

accuracyPlot <- function(EPPOcod, quantity, ie.Var, customPlot,
                         cv.ul, fixed,
                         dbh.step, htot.step, dbh.buf, htot.buf,
                         plot.est) {

  if (length(EPPOcod) != 1) {
    stop("only one EPPOcode accepted")
  }
  Tpag <- INFCpag(EPPOcod)
  if (Quantities %>% dplyr::filter(quantity == !!quantity) %>% nrow != 1) {
    rlang::abort(paste0("Unknown <quantity>: '", quantity, "' - See 'Quantities'"))
  }

  data <- INFCcatalog %>%
    dplyr::filter(pag == Tpag) %>%
    dplyr::select(section, n_oss) %>%
    dplyr::inner_join(INFCspecies %>%
                        dplyr::filter(EPPOcode == EPPOcod) %>%
                        dplyr::select(PrefName),
                      by = character())

  # Prepare function domain limits for the plot
  dom2 <- INFCf_domains %>%        # start with upper left
    dplyr::filter(pag == Tpag)
  dom2 <- dom2 %>%
    dplyr::filter(htot.m == min(htot.m), dbh.min == min(dbh.min)) %>%
    dplyr::select(htot.m, dbh.max = dbh.min) %>%
    dplyr::bind_rows(dom2) %>%   # add the body
    dplyr::bind_rows(dom2 %>%    # complete with lower right
                       dplyr::filter(htot.m == max(htot.m), dbh.max == max(dbh.max)) %>%
                       dplyr::select(htot.m, dbh.min = dbh.max))
  coo.min <- dom2 %>%             # left side
    dplyr::transmute(h_tot=htot.m, dbh=dbh.min) %>%
    tidyr::drop_na()
  coo.max <- dom2 %>%             # right side
    dplyr::transmute(h_tot=htot.m, dbh=dbh.max) %>%
    tidyr::drop_na()

  # Prepare (or retrieve) plot background data
  if (customPlot) {
  grid.k <- compute_grid(Tpag, EPPOcod, quantity, ie.Var,
                         dbh.step, htot.step, dbh.buf, htot.buf)
  } else {
    grid.k <- INFC_CVgrid %>%
      dplyr::filter(pag == Tpag, quantity == !!quantity) %>%
      # dplyr::select(grid.k) %>%
      # purrr::pluck(1, 1)
      dplyr::select(-c(pag, quantity))
  }
  grid.k <- grid.k %>%
    dplyr::transmute(dbh, h_tot, est,
              cv = dplyr::case_when(ie.Var  ~ cv_ie, TRUE  ~ cv_ea))

  # Background breaks
  brk <- c(-0, 0.01, 0.015, 0.02, 0.03)   # set su cui focalizzare attenzione

  if (!fixed) {
    # per avere classi di cv di uguale numerosità entro il range di interesse
    brk.arg <- grid.k %>%
      dplyr::filter(cv > 0, cv < cv.ul) %>% # cv fuori range ignorati
      dplyr::select(cv) %>% na.omit()
    brk <-
      c(0, quantile(brk.arg$cv, probs = seq(0 , 1 , length.out = length(brk) +
                                              1)), max(grid.k$cv)) %>%
      round(5)
  } else {
    fe.ul <- 1.2    #  fattore di espansione ultimo livello
    ## ultimo livello = cv.ul , da controllare che sia coerente e ragionevole
    cv.ul <-
      dplyr::if_else(cv.ul < brk[length(brk)], brk[length(brk)] * fe.ul, cv.ul)
    cv.ul <- dplyr::if_else(cv.ul > 1, 1, cv.ul)
    brk <- c(-Inf, brk, cv.ul, Inf)
  }

  pal <- c("#000000", rev(RColorBrewer::brewer.pal(length(brk) - 1, "RdYlGn")))
  # Dipendenza da considerare!!
  lbl <- paste(brk * 100, "%")
  lbl[1] <- " "  # "<0"
  lbl[length(lbl) - 1] <- paste(">", lbl[length(lbl) - 1])

  # Produce the graph
  cvg <- ggplot2::ggplot(grid.k, ggplot2::aes(y = dbh, x = h_tot)) +
    ggplot2::geom_contour_filled(ggplot2::aes(z = round(cv,4)),
                        breaks = brk, show.legend = T) +
    ggplot2::geom_step(
      data = coo.min,
      ggplot2::aes(y = dbh, x = h_tot),
      color = "grey",
      size = 1
    )+
    ggplot2::geom_step(
      data = coo.max,
      ggplot2::aes(y = dbh, x = h_tot),
      color = "grey",
      size = 1
    ) +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_x_continuous(position = "top") +
    ggplot2::labs(
      title = data$PrefName,
      subtitle = paste0("(Sez.: ", data$section,
                        " - n.oss = ", data$n_oss, ")"),
      caption = paste0(
        "Coefficiente di variazione (CV%) della stima di valori ",
        dplyr::if_else(ie.Var, "'individuali", "'medi'"),
        " di '",
        quantity,
        "' (vedi 'Quantity'", ")"
      )) +
    ggplot2::scale_fill_manual(
      name = paste0("CV% (", dplyr::if_else(ie.Var, "ie", "ea"), ")"),
      values = pal,
      labels = lbl,
      drop = FALSE,
      guide = ggplot2::guide_legend(label.vjust = 1.3)
    )

  if (plot.est) {
    # eventualmente da usare per plottare 'est'
    cvg <- cvg +
      ggplot2::geom_contour(ggplot2::aes(z = est), linetype = "dotted", colour = "cyan") +
      metR::geom_text_contour(
        ggplot2::aes(z = est),
        size = 3,
        label.placer = metR::label_placer_fraction(frac = 0.5)
      )
  }

  return(cvg)
}
