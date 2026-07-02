#########################################################################################
# Harbours
#########################################################################################

UNLOCODE %>%
  mutate(Harbour = loCode) %>%
  filter(!is.na(Harbour)) %>%
  select(Harbour, lat, lon) %>% 
  mutate(lat = ifelse(Harbour=='MRNDB',20.909,
                      ifelse(Harbour == 'MAAGA',30.4215,
                             ifelse(Harbour == 'FKPSY', -51.6875,
                                    ifelse(Harbour == 'AOBUG',-12.591,
                                           ifelse(Harbour == 'AOLOB', -12.333, 
                                                  ifelse(Harbour == 'MAVIL', 23.6775, 
                                                         ifelse(Harbour == 'ESLPA', 28.141, 
                                                                ifelse(Harbour == 'NAWVB', -22.94, 
                                                                       ifelse(Harbour == 'MRNKC', 17.989, 
                                                                              ifelse(Harbour == "SNDKR", 14.681, 
                                                                                     ifelse(Harbour == "AOLAD",-8.7665, 
                                                                                            ifelse(Harbour == "GNCKY", 9.517, 
                                                                                                   ifelse(Harbour == "CGPNR", -4.7864475, 
                                                                                                          ifelse(Harbour == "MALAR", 35.201735,
                                                                                                                 ifelse(Harbour == "ISHAF", 64.0665,
                                                                                                                        ifelse(Harbour == "PEGSM",  -13.8053346,
                                                                                                                               lat)))))))))))))))),
         lon = ifelse(Harbour=='MRNDB',-17.0415,
                      ifelse(Harbour == 'MAAGA', -9.634,
                             ifelse(Harbour == 'FKPSY',-57.86,
                                    ifelse(Harbour == 'AOBUG',13.379 ,
                                           ifelse(Harbour == 'AOLOB', 13.574,
                                                  ifelse(Harbour == 'MAVIL', -15.933,
                                                         ifelse(Harbour == 'ESLPA', -15.41635,
                                                                ifelse(Harbour == 'NAWVB', 14.502,
                                                                       ifelse(Harbour == 'MRNKC', -16.0305,
                                                                              ifelse(Harbour == "SNDKR",  -17.4275, 
                                                                                     ifelse(Harbour == "AOLAD",  13.273, 
                                                                                            ifelse(Harbour == "GNCKY", -13.7115, 
                                                                                                   ifelse(Harbour == "CGPNR", 11.833135, 
                                                                                                          ifelse(Harbour == "MALAR", -6.1476475,
                                                                                                                 ifelse(Harbour == "ISHAF", -21.9755,
                                                                                                                        ifelse(Harbour == "PEGSM",  -76.319038,
                                                                                                                               lon))))))))))))))))) -> Harbours
Harbours[which(Harbours$Harbour=="SCPOV"),]$lat <- -4.62
Harbours[which(Harbours$Harbour=="NGPHC"),]$lat <- 4.77
Harbours[which(Harbours$Harbour=="PABLB"),]$lat <- 8.95
Harbours[which(Harbours$Harbour=="PEPAI"),]$lat <- -5.0833
Harbours[which(Harbours$Harbour=="PECLL"),]$lat <- -12.0559
Harbours[which(Harbours$Harbour=="UYMVD"),]$lat <- -34.8927
Harbours[which(Harbours$Harbour=="CIABJ"),]$lat <- 5.3165
Harbours[which(Harbours$Harbour=="PEPIO"),]$lat <- -13.80
Harbours[which(Harbours$Harbour=="MUPLU"),]$lat <- -20.14365
Harbours[which(Harbours$Harbour=="GAPOG"),]$lat <- -0.687084
Harbours[which(Harbours$Harbour=="GLJHS"),]$lat <- 66.932038
Harbours[which(Harbours$Harbour=="YTMAM"),]$lat <- -12.779578
Harbours[which(Harbours$Harbour=="ZACPT"),]$lat <- -33.900709

Harbours[which(Harbours$Harbour=="SCPOV"),]$lon <- 55.45
Harbours[which(Harbours$Harbour=="NGPHC"),]$lon <- 7.03
Harbours[which(Harbours$Harbour=="PABLB"),]$lon <- -79.55
Harbours[which(Harbours$Harbour=="PEPAI"),]$lon <- -81.1166
Harbours[which(Harbours$Harbour=="PECLL"),]$lon <- -77.1554
Harbours[which(Harbours$Harbour=="UYMVD"),]$lon <- -56.2302
Harbours[which(Harbours$Harbour=="CIABJ"),]$lon <- -4.0267
Harbours[which(Harbours$Harbour=="PEPIO"),]$lon <- -76.27
Harbours[which(Harbours$Harbour=="MUPLU"),]$lon <- 57.51629
Harbours[which(Harbours$Harbour=="GAPOG"),]$lon <- 8.777561
Harbours[which(Harbours$Harbour=="GLJHS"),]$lon <- -53.691972
Harbours[which(Harbours$Harbour=="YTMAM"),]$lon <- 45.2342
Harbours[which(Harbours$Harbour=="ZACPT"),]$lon <- 18.436373

Harbours[which(Harbours$Harbour=="ESOZL"),]$lon <- -13.452

# let's check if everything is ok
Harbours %>% 
  filter(Harbour %in% unique(cl_rcg$CLlandingLocation)) %>% 
  filter(is.na(lon) | is.na(lat)) -> HarboursMissing


if(nrow(HarboursMissing)>0){
  message(paste('\n -------> There are some harbours with missing coordinates: \n \n', 
                paste(unique(HarboursMissing$Harbour), collapse = ', '), '\n', sep = ''))
}

#########################################################################################
# Divisions
#########################################################################################

Divisionshp  = sf::st_read("../../data/shapefiles/RCG_LDF_FAOareas.shp") %>%  filter(
  F_LEVEL == 'DIVISION'  # potential problem with 34.2.0 because it's subarea not division
)
Divisionshp %>%
  mutate(Division = F_CODE) -> Divisionshp
Divisionshp = cbind(Divisionshp,  sf::st_coordinates(sf::st_centroid(Divisionshp$geometry))) %>% rename(lon = X, lat = Y)

#########################################################################################
# FAO major areas
#########################################################################################

FAOshp  = sf::st_read("../../data/shapefiles/RCG_LDF_FAOareasMajor.shp") 
FAOshp %>%
  mutate(Area = F_CODE) -> FAOshp
FAOshp = cbind(FAOshp,  sf::st_coordinates(sf::st_centroid(FAOshp$geometry))) %>% rename(lon = X, lat = Y)

#########################################################################################
# FAO areas
#########################################################################################

Subareashp = sf::st_read("../../data/shapefiles/RCG_LDF_FAOareas.shp")  %>% 
  group_by(Subarea = F_SUBAREA) %>% 
  summarise(geometry = st_union(geometry))
Subareashp = cbind(Subareashp,  sf::st_coordinates(sf::st_centroid(Subareashp$geometry))) %>% rename(lon = X, lat = Y)
