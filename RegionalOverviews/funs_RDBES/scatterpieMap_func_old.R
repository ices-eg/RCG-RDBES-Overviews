# function base on  script scaterrpieMap_func.R (RCGs repository)(M.Szymańska)

#dev. notes:
#- fixed issue with facet wrap in pie charts (K.Krakowka)
#- added option of facet to the general function (K.Krakowka)
#- converded to RDBES data format (K.Krakówka)

scatterpieMap_func = function(df,
                              var,
                              groupBy,
                              groupBy2,
                              facet,
                              func,
                              type_of_threshold = 'none',
                              value_of_threshold = NA,
                              points_coord,
                              plot_labels = FALSE,
                              Catch_group_name = NA,
                              addExtraShp = FALSE,
                              extraShp = NA,
                              newVarName = NA,
                              addToTitle = NA,
                              color_palette = NA,
                              filter_ON = FALSE,
                              filter_column  = NA,
                              filter_type = NA,
                              filter_threshold = NA,
                              filter_func = NA) {
  require(rlang)
  require(ggplot2)
  require(sf)
  require(rnaturalearth)
  require(ggforce)
  
  source('../../funs_RDBES/group_func_old.R')
  
  var_name <- var
  var <- as.symbol(var_name)
  groupBy_name <- groupBy
  groupBy <- as.symbol(groupBy_name)
  groupBy2_name <- groupBy2
  groupBy2 <- as.symbol(groupBy2_name)
  
  if(!is.na(facet)){
    facet_name <- facet
    facet <- as.symbol(facet)  
  } else {
    facet_name <- NA
    facet <- NA
  }
  
  func_name <- func
  func <- eval_tidy(as.symbol(func))
  
  if(filter_ON == TRUE){
    filter_column_name <- filter_column
    filter_column <- as.symbol(filter_column_name)
    
    group_func(df,
               var = var_name,
               groupBy = filter_column_name,
               facet = facet_name,
               func = filter_func,
               type_of_threshold = filter_type,
               value_of_threshold = filter_threshold) -> filteredVariable
    
    df %>% 
      filter(!!filter_column %in% (filteredVariable[[1]] %>% pull(!!filter_column))) %>% 
      droplevels() -> df 
  }
  
  grouping_result <- group_func(df, var_name, groupBy_name, groupBy2 = groupBy2_name, facet_name, func_name, type_of_threshold = type_of_threshold, 
                                value_of_threshold =  value_of_threshold, Catch_group_name = Catch_group_name, groupBy2spread = TRUE)  
  tdf <- grouping_result[[1]]
  if (is.null(tdf)) stop('The chosen data set is empty')
  missing_entries <- grouping_result[[2]]
  
  tdf %>% left_join(points_coord) -> mdf
  
  mdf %>% mutate(var = !!var,
                 groupBy = !!groupBy,
                 groupBy2 = !!groupBy2,
                 facet = !!facet) -> mdf
  
  mdf %>% 
    filter((is.na(lat) | is.na(lon)) & !is.na(groupBy)) %>% 
    distinct(groupBy, pr) %>% 
    summarise(pr = sum(pr), n = n_distinct(groupBy)) %>% 
    as.data.frame() %>% 
    select(pr, n) -> missing_value
  
  if (nrow(missing_value) > 0 & (missing_value$pr != 0 & missing_value$n != 0)) {
    missing_caption <- paste(
      '\n', missing_value$n, ' ', groupBy_name,
      ' with missing coordinates (', 
      ifelse(missing_value$pr <= 0.005 & missing_value$pr > 0, '~0', round(missing_value$pr, 2)),
      '% of ', ifelse(is.na(newVarName), var_name, newVarName),
      ') - not presented on the map.', sep = '')
    message(missing_caption)
  } else {
    missing_caption <- ''
  }
  
  xlim <- range(mdf[!is.na(mdf$lat) & !is.na(mdf$lon), ]$lon) + c(-5, 5)
  ylim <- range(mdf[!is.na(mdf$lat) & !is.na(mdf$lon), ]$lat) + c(-4, +4)
  
  if(unique(df$Region) != 'NSEA'){
    if(abs(xlim[2] - xlim[1]) > (3/2)*abs(ylim[2] - ylim[1])){
      diff <- (2/3*abs(xlim[2] - xlim[1]) - abs(ylim[2] - ylim[1])) / 2
      ylim[1] <- ylim[1] - diff
      ylim[2] <- ylim[2] + diff
    } else if((2/3)*abs(xlim[2] - xlim[1]) < abs(ylim[2] - ylim[1])){
      diff <- (3/2*abs(ylim[2] - ylim[1]) - abs(xlim[2] - xlim[1])) / 2
      xlim[1] <- xlim[1] - diff
      xlim[2] <- xlim[2] + diff 
    }
  } else {
    if(abs(xlim[2] - xlim[1]) > (5/2)*abs(ylim[2] - ylim[1])){
      diff <- (2/5*abs(xlim[2] - xlim[1]) - abs(ylim[2] - ylim[1])) / 2
      ylim[1] <- ylim[1] - diff
      ylim[2] <- ylim[2] + diff
    } else if((2/5)*abs(xlim[2] - xlim[1]) < abs(ylim[2] - ylim[1])){
      diff <- (5/2*abs(ylim[2] - ylim[1]) - abs(xlim[2] - xlim[1])) / 2
      xlim[1] <- xlim[1] - diff
      xlim[2] <- xlim[2] + diff 
    } 
  }
  
  mdf2 <- mdf %>% filter(!is.na(lat) & !is.na(lon)) %>% filter(lon >= -180 & lon <= 180 & lat >= -90 & lat <= 90)
  
  m <- ne_countries(scale = "medium", returnclass = "sf")
  
  if (groupBy_name == "CLstatisticalRectangle") groupBy_name <- "Statistical Rectangle"
  if (groupBy_name == "AreaMap") groupBy_name <- "Area"
  if (groupBy2_name == "CLvesselFlagCountry") groupBy2_name <- "Country"
  
  if (func_name %in% c('sum')) {
    if (func_name == "sum") func_name <- "Sum"
    title <- paste(func_name, ' of ', ifelse(is.na(newVarName), var_name, newVarName), ' by ', groupBy_name, sep = '')
  } else {
    title <- paste(func_name, ' ', ifelse(is.na(newVarName), var_name, newVarName), ' by ', groupBy_name, sep = '')
  }
  
  if (!is.na(Catch_group_name) & Catch_group_name != 'NULL') title <- paste(title, ' (', Catch_group_name, ')', sep = '')
  if (!is.na(addToTitle)) title <- paste(title, ' (', addToTitle, ')', sep = '')
  
  if ((type_of_threshold == 'percent' & value_of_threshold == 100) | type_of_threshold == 'none') {
    subtitle <- 'All data'
  } else if (type_of_threshold == 'percent') {
    subtitle <- paste('Including ', groupBy_name, 's accounting for ', value_of_threshold, '% of ', ifelse(is.na(newVarName), var_name, newVarName), sep = "")
  } else {
    subtitle <- paste('Displaying top ', value_of_threshold, ' ', groupBy_name, 's', sep = "")
  }
  
  unique_bys <- mdf2 %>% distinct(!!groupBy2) %>% nrow()
  if (length(color_palette) == 1 && is.na(color_palette)) {
    color_palette <- scales::hue_pal()(unique_bys)
    names(color_palette) <- sort(unique(mdf2[[groupBy2_name]]))
  }
  
  radius <- 0.3
  radiusMultiply <- ifelse(groupBy_name %in% c('Area', 'AreaMap','FishingGround', 'Division'), 4,
                           ifelse(groupBy_name %in% c('Harbour', 'LandingCountry', 'FlagCountry'), 3, 1))
  if (unique(df$Region) == 'BS') radiusMultiply <- radiusMultiply * 2 / 3
  
  pie_data <- mdf2 %>%
    rename(value = !!var, group = !!groupBy, subgroup = !!groupBy2) %>%
    group_by(group, facet) %>%
    mutate(total = sum(value),
           frac = value / total,
           ymax = cumsum(frac) * 2 * pi,
           ymin = (cumsum(frac) - frac) * 2 * pi) %>%
    ungroup() %>%
    mutate(radius = total / max(total, na.rm = TRUE) * radiusMultiply * radius)
  
  p <- ggplot() +
    geom_sf(data = m, fill = "antiquewhite") +
    coord_sf(crs = "+init=epsg:4326", xlim = xlim, ylim = ylim, expand = FALSE) +
    geom_arc_bar(data = pie_data,
                 aes(x0 = lon, y0 = lat, r0 = 0, r = radius,
                     start = ymin, end = ymax, fill = subgroup),
                 color = "black", size = 0.3) +
    scale_fill_manual(values = color_palette, name = groupBy2_name) +
    labs(
      title = title,
      subtitle = subtitle,
      x = 'Longitude',
      y = 'Latitude',
      fill = groupBy2_name
    ) +
    theme_classic() +
    theme(
      text = element_text(color = "#22211d"),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "aliceblue", color = NA),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      panel.border = element_rect(colour = "black", fill = NA, size = 1.5),
      panel.grid.major = element_line(color = gray(.8), linetype = 'dashed', size = 0.5)
    ) 
  
  if (!is.na(facet)) {
    p <- p + facet_wrap(as.formula(paste("~", facet)))
  }
  
  if (plot_labels) {
    p <- p + geom_text(data = pie_data %>% distinct(group, lon, lat),
                       aes(x = lon, y = lat, label = group),
                       size = 3, color = 'grey22', fontface = "italic")
  }
  
  return(p)
}