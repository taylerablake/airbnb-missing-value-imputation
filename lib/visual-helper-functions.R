barChartHelper <- function(v, vnam) {
  #define a ggplot2 histogram 
  p <- ggplot(data.frame(v = v), aes(x = v)) + 
    geom_bar(col = "white") +
    xlab(vnam) +
    ylab("n") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1, vjust = 1))
  p
}
barChartVisual <- function(v, vnam, doEval = TRUE) {
  #define the call
  thisCall <- call("barChartHelper", v = v, vnam = vnam)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}
barChartVisual <- visualFunction(barChartVisual,
                                   description = "Bar plots using ggplot",
                                   classes = setdiff(allClasses(), 
                                                     c("numeric", 
                                                       "integer", 
                                                       "Date")))

mosaicPlotHelper <- function(v, vnam) {
  df <- data.frame(v = v)
  #define a ggmosaic 
  p <- ggplot(df, aes(x = v)) +
    geom_mosaic(aes(x = product(v), fill=v), na.rm=TRUE) +
    labs(x=vnam, y = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            axis.text.y = element_blank(),
            panel.grid = element_blank()) +
      scale_fill_tableau() +
      guides(fill = "none")
  df <- group_by(df, v) %>%
    dplyr::count() %>%
    ungroup %>%
    mutate(prop = n/sum(n))
  group_props <- ggplot_build(p)$data %>%
    as.data.frame %>%
    mutate(x.position = (xmax + xmin) / 2,
           y.position = (ymax + ymin) / 2) %>%
    inner_join(., df %>%
                 dplyr::select(v, prop) %>%
                 distinct, by = c("label" = "v"))
  p + geom_text(data = group_props,
                 aes(x = x.position,
                     y = rep(1.05, nrow(group_props)),
                     label = paste0(round(prop, 3)*100, "%")),
                 inherit.aes = FALSE,
                size = 3, angle = 45) +
    scale_y_continuous(limits = c(0, 1.1), expand = c(0, 0))
}
mosaicPlotVisual <- function(v, vnam, doEval = TRUE) {
  #define the call
  thisCall <- call("mosaicPlotHelper", v = v, vnam = vnam)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}
mosaicPlotVisual <- visualFunction(mosaicPlotVisual,
                               description = "Mosaic plots using ggplot",
                               classes = setdiff(allClasses(), 
                                                 c("numeric", 
                                                   "integer", 
                                                   "Date")))

histogramHelper <- function(v, vnam) {
  #define a ggplot2 histogram 
  p <- ggplot(data.frame(v = v), aes(x = v)) + 
    geom_histogram(col = "white", bins = 20) +
    xlab(vnam) +
    theme_minimal()
  p
}
histogramVisual <- function(v, vnam, doEval = TRUE) {
  #define the call
  thisCall <- call("histogramHelper", v = v, vnam = vnam)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}
#Make it a proper visualFunction:
histogramVisual <- visualFunction(histogramVisual, 
                               description = "ggplot2 style histogram",
                               classes = c("numeric", "integer", "logical", "Date"))

