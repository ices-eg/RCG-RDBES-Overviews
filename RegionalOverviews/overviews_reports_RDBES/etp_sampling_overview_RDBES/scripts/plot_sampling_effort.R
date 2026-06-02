
plot_sampling_heatmap <- function(data, region_name) {
  
  ggplot(
    dplyr::filter(data, EcoRegion == region_name), 
    aes(y = factor(SDctry), x = factor(gear))
  ) +
    facet_wrap(EcoRegion ~ DEyear, ncol = 3) +
    
    geom_tile(aes(fill = CE.Ntrips)) +
    
    geom_point(
      aes(size = NtripsSamp),
      shape = 1, stroke = 1, na.rm = TRUE
    ) +
    
    scale_fill_distiller(
      palette = "Spectral",
      direction = -1,
      trans = "log10",
      na.value = "transparent"
    ) +
    
    labs(
      x = 'Gear',
      y = 'Country',
      fill = "Total effort (trips)",
      size = "No. trips sampled",
      title = paste("Sampling At Sea -", region_name)
    ) +
    
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
