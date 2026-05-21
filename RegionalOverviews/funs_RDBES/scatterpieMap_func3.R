# work in progress
# better version of scatterpieMap_func
# for now only checked for Baltic

scatterpieMap_func_new <- function(df,
                               var,
                               groupBy,
                               groupBy2,
                               facet,
                               func,
                               type_of_threshold = "none",
                               value_of_threshold = NA,
                               points_coord,
                               plot_labels = FALSE,
                               Catch_group_name = NA,
                               addExtraShp = FALSE,
                               extraShp = NA,
                               newVarName = NA,
                               addToTitle = NA,
                               color_palette = NA,
                               filter_column = NA,
                               filter_type = NA,
                               filter_threshold = NA,
                               filter_func = NA) {
  
  require(rlang)
  require(ggplot2)
  require(sf)
  require(dplyr)
  require(ggforce)
  require(rnaturalearth)
  require(scales)
  
  # CRS (może być 3035 / 2180 / cokolwiek metrycznego)
  # set parameteres for each region, for now only BALTIC
  #crs_use <- 3035 
  crs_use <- "+proj=laea +lat_0=56 +lon_0=18"

  var_name <- var
  var_sym <- sym(var)
  groupBy_name <- groupBy
  group_sym <- sym(groupBy)
  groupBy2_name <- groupBy2
  subgroup_sym <- sym(groupBy2)
  func_name <- func
  
  facet_flag <- !is.na(facet)
  facet_sym <- if (facet_flag) sym(facet) else NULL
  facet_name <- if (facet_flag) facet else NULL
  
  if (groupBy_name == "CLstatisticalRectangle" | groupBy_name == "CEstatisticalRectangle") groupBy_name <- "Statistical Rectangle"
  if (groupBy_name == "AreaMap") groupBy_name <- "Area"
  if (groupBy_name == "CLlandingLocation") groupBy_name <- "Harbour"
  if (groupBy2_name == "CLvesselFlagCountry" | groupBy2_name == "CEvesselFlagCountry") groupBy2_name <- "FlagCountry"
  
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
  
  if (nrow(df) == 0) stop("Empty dataset")
  
  # INPUT SPATIAL DATA

  points_coord <- st_transform(points_coord, crs_use)
  
  if (addExtraShp && !is.null(extraShp)) {
    extraShp <- st_transform(extraShp, crs_use)
  }
  
  # JOIN DATA

  mdf <- df %>%
    left_join(points_coord) %>%
    mutate(
      value = !!var_sym,
      group = !!group_sym,
      subgroup = !!subgroup_sym,
      facet = if (facet_flag) !!facet_sym else NA
    ) %>%
    filter(!is.na(lon), !is.na(lat))
  
  # PROJECT TO CRS
  pts_sf <- st_as_sf(mdf, coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    st_transform(crs_use)
  
  xy <- st_coordinates(pts_sf)
  mdf$X <- xy[,1]
  mdf$Y <- xy[,2]
  
  # BASE MAP
  m <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_transform(crs_use)
  
  # UNIVERSAL SCALE 
  map_w <- diff(range(mdf$X, na.rm = TRUE))
  map_h <- diff(range(mdf$Y, na.rm = TRUE))
  
  map_size <- min(map_w, map_h)
  
  # set parameteres for each region, for now only BALTIC
  radius_base = map_size * ifelse(groupBy_name %in% c('Area', 'AreaMap','FishingGround', 'Division'), 0.08,
         ifelse(groupBy_name %in% c('Harbour', 'LandingCountry', 'FlagCountry'), 0.06, 0.035))
  
  radius_min  <- map_size * 0.002    
  
  dx <- map_w * 0.05                 # padding
  dy <- map_h * 0.05
  
  xlim <- range(mdf$X, na.rm = TRUE) + c(-dx, dx)
  ylim <- range(mdf$Y, na.rm = TRUE) + c(-dy, dy)
  
  # PIE DATA
  pie_data <- mdf %>%
    group_by(group, facet, X, Y) %>%
    mutate(
      total = sum(value, na.rm = TRUE),
      frac = value / total,
      ymax = cumsum(frac) * 2 * pi,
      ymin = (cumsum(frac) - frac) * 2 * pi
    ) %>%
    ungroup() %>%
    mutate(
      radius = pmax(
        sqrt(total / max(total, na.rm = TRUE)) * radius_base,
        radius_min
      )
    ) %>% 
    arrange(X, Y, group, facet, ymin)
  
  # COLORS SAFE
  vals <- unique(mdf[[groupBy2]])
  
  if (is.null(color_palette) || length(color_palette) == 0 || all(is.na(color_palette))) {
    pal <- hue_pal()(length(vals))
    names(pal) <- sort(vals)
    color_palette <- pal
  } else if (is.data.frame(color_palette)) {
    color_palette <- setNames(color_palette[,2], color_palette[,1])
  }
  
  # PLOT
  p <- ggplot()
  
  # extra shapefile (POD MAPĄ)
  if (addExtraShp) {
    p <- p + geom_sf(data = extraShp, fill = NA, color = "grey60")
  }
  
  # base map
  p <- p +
    geom_sf(data = m, fill = "antiquewhite", color = "grey40")
  
  # pies
  p <- p +
    geom_arc_bar(
      data = pie_data,
      aes(
        x0 = X, y0 = Y,
        r0 = 0, r = radius,
        start = ymin, end = ymax,
        fill = subgroup,
        group = interaction(X, Y, group)
      ),
      color = "grey20", size = 0.5
    ) +
    scale_fill_manual(values = color_palette, name = groupBy2_name)+
    labs(
      title = title,
      subtitle = subtitle,
      x = 'Longitude',
      y = 'Latitude',
      fill = groupBy2_name
    )
  
  # view
  p <- p +
    coord_sf(
      crs = crs_use,
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    ) +
    theme_classic()
  
  # facet
  if (facet_flag) {
    p <- p + facet_wrap(as.formula(paste("~", facet)))
  }
  
  # labels
  if (plot_labels) {
    p <- p +
      geom_text(
        data = pie_data %>% distinct(group, X, Y),
        aes(x = X, y = Y, label = group),
        size = 3
      )
  }
  
  return(p)
}