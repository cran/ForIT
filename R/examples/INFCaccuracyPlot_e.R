\dontrun{
INFCaccuracyPlot("FRXAN") # 'INFCaccuracyPlot()' can be slow because
#       it computes all the CV values needed to fill backgroud plot,
#              hence default values are set to a coarser resolution.
\donttest{
  INFCaccuracyPlot("FRXAN",
                   dbh.step = 1,
                   htot.step = 1) 
  # computing with high resolution is slow
}

INFCaccuracyPlot0("FRXAN") 
# 'INFCaccuracyPlot0()' is quick, it uses stored values
INFCaccuracyPlot0("FRXAN", "dw4")
INFCaccuracyPlot0("FRXAN", "dw4", ie.Var = TRUE) 
# deafult fixed break values are not alwais optimal
INFCaccuracyPlot0("FRXAN", "dw4", ie.Var = TRUE, fixed = FALSE, cv.ul=.9) 
# tailoring can improve
INFCaccuracyPlot0("FRXAN", plot.est = TRUE) 
# 'quantity' estimation iso-lines can be superimposed


background <- INFCaccuracyPlot0("ACROP", plot.est = TRUE)
foreground <-  ForIT_test_data %>%
  dplyr::filter(specie == "ACROP") %>%
  dplyr::mutate(vol = INFCvpe(specie, d130, h_dendro)) %>%
  ggplot2::geom_point(map = ggplot2::aes(h_dendro, d130, size = vol))
background + foreground  # Adding a custom foreground
rm(background, foreground)

INFCaccuracyPlot0("ABIAL") 
# high resolution and quick, using pre-calculated backgroung values
INFCaccuracyPlot("ABIAL") 
# default values produce a coarser resolution
}