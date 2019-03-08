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
    inner_join(., df, by = c("label" = "v"))
  p + geom_text(data = group_props,
                 aes(x = x.position,
                     y = rep(1.05, 2),
                     label = paste0(round(prop, 3)*100, "%")),
                 inherit.aes = FALSE) +
    ylim(0,1.1)
}

histogramHelper <- function(v, vnam) {
  #define a ggplot2 histogram 
  p <- ggplot(data.frame(v = v), aes(x = v)) + 
    geom_histogram(col = "white") +
    xlab(vnam) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  p
}


