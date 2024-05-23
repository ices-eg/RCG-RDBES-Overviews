pointsMap_func2 <- function(df,
                            title, 
                            var_name,
                            facet_name,
                            var_spatial_name,
                            plot_labels = FALSE,
                            saveResults = FALSE,
                            outputPath,
                            Catch_group_name = NA,
                            extraShp_name = NA,
                            newVarName = NA,
                            addToTitle = NA,
                            RCGregion = NA,
                            spatial_dataset_name,
                            spatial_dataset_var_name) {
  # Marta Szymańska
  # NMFRI
  # msuska@mir.gdynia.pl
  # edited by:
  # Iga Gaca
  # NMFRI
  # igaca@mir.gdynia.pl
  
  ######################################
  # load packages #
  ######################################
  
  require(rlang)
  require(ggplot2)
  require(sf)
  require(rnaturalearth)
  require(dplyr)
  require(viridis)
  require(ggrepel)
  var_name = 'CLscientificWeight_1000ton'
  facet_name = 'CLyear'
  var_spatial_name = 'CLlandingLocation'
  spatial_dataset_name = 'Harbours'
  spatial_dataset_var_name = 'Harbour'
  plot_labels = FALSE
  saveResults = FALSE
  
  Catch_group_name = NA
  extraShp_name = 'StatRectshp'
  newVarName = NA
  addToTitle = NA
  RCGregion = 'BA'
  # load worldmap
  ne_countries <- ne_countries(scale = "medium", returnclass = "sf")
  
  ######################################
  # check parameters
  ######################################
  
  if (is.null(df)) {
    stop('The chosen data set is empty')
  }
  
  
  if (!var_name %in% colnames(df)) {
    stop(paste(
      'The given column --->',
      var_name,
      '<--- is not present in the df dataset'
    ))
  }
  
  if (!var_spatial_name %in% colnames(df)) {
    stop(
      paste(
        'The given column --->',
        var_spatial_name,
        '<--- is not present in the df dataset'
      )
    )
  }
  
  if (!is.na(facet_name) & !facet_name %in% colnames(df)) {
    stop(paste(
      'The given column --->',
      facet_name,
      '<--- is not present in the df dataset'
    ))
  }
  
  if (!exists(spatial_dataset_name)) {
    stop(paste(spatial_dataset_name,
               '<--- was not found'))
  }
  
  if (!is.na(extraShp_name)) {
    if (!exists(extraShp_name)) {
      stop(paste(extraShp_name,
                 '<--- was not found'))
    }
  }
  ######################################
  # transform names into symbols - to use it inside tidyverse
  ######################################
  
  var <- as.symbol(var_name)
  var_spatial <- as.symbol(var_spatial_name)
  
  spatial_dataset <- eval_tidy(as.symbol(spatial_dataset_name))
  spatial_dataset_var <- as.symbol(spatial_dataset_var_name)
  
  extraShp <- eval_tidy(as.symbol(extraShp_name))
  
  if (!is.na(facet_name)) {
    facet <- as.symbol(facet_name)
  } else {
    facet <- NA
  }
  
  ######################################
  # combine  df with shp
  ######################################
  
  df_spatial <- spatial_dataset %>%
    full_join(df, by = setNames(as.character(var_spatial), as.character(spatial_dataset_var_name)), keep = TRUE)
  
  # Rename variables so that it works for any parameters
  df_spatial <- df_spatial %>%
    rename(var = !!var, var_spatial = !!var_spatial, facet = !!facet) %>%
    mutate(var_name = var_name, var_spatial_name = var_spatial_name, facet_name = facet_name) %>%
    filter(!is.na(var))
  
  # Calculate map boundaries
  min_lon <- min(df_spatial$lon, na.rm = TRUE) - 2
  max_lon <- max(df_spatial$lon, na.rm = TRUE) + 2
  min_lat <- min(df_spatial$lat, na.rm = TRUE) - 2
  max_lat <- max(df_spatial$lat, na.rm = TRUE) + 2
  
  
  
 
  
  # add info about records without with given var_spatial but shp  missing
#   df_spatial %>%  filter((st_is_empty(.) | is.na(geometry)) & !is.na(var_spatial)) %>% as.data.frame() %>%group_by(facet) %>%
#     summarise(missing_shp_pr = sum(pr), missing_shp_n_distinct = n_distinct(var_spatial)) %>%
#     select(facet, missing_shp_pr, missing_shp_n_distinct)-> missing_shp
# #  should we inform the users which var_spatial were not found in the shp?
#   title = paste(ifelse(!is.na(var_name_new), var_name_new, var_name),
#                 ' by ',
#                 var_spatial_name,
#                 sep = ''
#   )
# 
#   if (unique(df_spatial_toMap$analysis_type) == 'sum') {
#     title  = paste('Sum',
#                    ' of '
#                    , title)
#   }else{
#     title = paste(unique(df_spatial_toMap$analysis_type) ,
#                   ' ',
#                   title)
#   }
  #
  # # If there is any additional information to the title
  if(!is.na(addToTitle)){ title = paste(title, ' (',addToTitle, ')', sep ='')}
  # 
  
  plt <- ggplot() +
    geom_sf(data = ne_countries, fill = "antiquewhite", linewidth = 0.5) +
    coord_sf(crs = "+init=epsg:4326", ylim = c(min_lat, max_lat), xlim = c(min_lon, max_lon), expand = FALSE) +
    labs(title = title, x = 'Longitude', y = 'Latitude') +  # Adjust title as needed
    theme_classic() +
    theme(
      text = element_text(color = "#22211d"),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "aliceblue", color = NA),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      panel.border = element_rect(colour = "black", fill = NA, size = 1.5),
      panel.grid.major = element_line(color = gray(.8), linetype = 'dashed', size = 0.5)
    ) 
  
  # Add points
  plt <- plt +
    geom_point(data = df_spatial, aes(x = lon, y = lat, size = var, color = var), alpha = 0.75) +
    scale_color_gradient(low = "yellow", high = "navy", name = "Landings (1000 t)") +
    scale_size(range = c(0, 20), guide = FALSE) +
    viridis::scale_fill_viridis(option = "viridis", begin = 1, end = 0, name = "Landings (1000 t)")
  
  #Add labels
  if (plot_labels) {
    plt <- plt + ggrepel::geom_label_repel(
      data = df_spatial,
      aes(x = lon, y = lat, label = as.character(Harbour)),  # Convert Harbour to character
      box.padding = unit(0.2, "lines"),
      point.padding = unit(0.2, "lines"),
      color = 'black',
      segment.color = gray(0.3),
      size = 2,
      fontface = 'bold',
      arrow = arrow(length = unit(0.02, "npc"))
    )
  }

  # Add facet if necessary
  if (!is.na(facet_name)) {
    plt <- plt + facet_wrap(vars(facet))
  }

  return(plt)
}

