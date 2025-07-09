###################################################################
# loadMaps
###################################################################
#
# This script loads the shape files and preparations needed for the generation 
# of the case studies overview in the RDBES data format. 
# 
###################################################################
# Authors: 
# - Kasia Krakówka [first draft]
# 
# 
# Dev. notes: 
#
# - 08.07.2025 first draft 
###################################################################

# Print start message
cat("[1]    Loading maps")

## Load UNLOCODE rData file

load("RegionalOverviews/data/UNLOCODE.rData")

## LOAD DATA NEEDED FOR MAPS (shp, rdata, ...) FOR ALL RCG.
UNLOCODE %>%
  mutate(Harbour = loCode) |>
  filter(!is.na(Harbour)) |>
  select(Harbour, lat, lon) -> Harbours

if (params$region =='BA'){
  # LOAD PREPARE DATA NEEDED FOR MAPS (shp, rdata, ...) ONLY FOR RCG BA
  StatRectshp  = sf::st_read("RegionalOverviews/data/shapefiles/RCG_BA_ICESrect.shp")# for BA maps on DIVISIONS level -> WATCH OUT ...28.1/...28.2
  
  FAOshp  = sf::st_read("RegionalOverviews/data/shapefiles/RCG_BA_FAOareas.shp") %>% filter(F_LEVEL == 'SUBDIVISION') # for BA maps on DIVISIONS level -> WATCH OUT ...28.1/...28.2
}

# PREPARE DATA NEEDED FOR MAPS (shp, rdata, ...)
StatRectshp %>% mutate(CLstatisticalRectangle = ICESNAME) -> StatRectshp
StatRectshp = cbind(StatRectshp,  sf::st_coordinates(sf::st_centroid(StatRectshp$geometry))) %>% rename(lon = X, lat = Y)

FAOshp = cbind(FAOshp,  sf::st_coordinates(sf::st_centroid(FAOshp$geometry))) %>% rename(lon = X, lat = Y)
FAOshp %>% mutate(AreaMap = F_CODE) -> FAOshp

if(params$region=='BA'){ # fixed wrong calculation of centroid of 27.3.d.30
  FAOshp = FAOshp %>%   
    mutate(lon = ifelse(F_CODE=='27.3.d.30', 19.5 ,lon), lat =  ifelse(F_CODE=='27.3.d.30',62 ,lat))
}


# Print end message
if(is_empty(warnings())){
  cat("\n")
  cat(green('       \u2713'), paste0(" - Completed")) 
  cat("\n")
  cat("\n")
} 
