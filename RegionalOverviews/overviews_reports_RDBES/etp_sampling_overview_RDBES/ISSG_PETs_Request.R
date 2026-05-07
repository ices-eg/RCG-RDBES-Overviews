
# The ISSG PETs wants to use RDBES data to analyse the coverage of the sampling onboard 
# to identify high risk fisheries for PETs which may be potentially under sampled. 
# To that aim, they need an extraction showing the total effort (CE) and the sampling effort (CS), by ecoregion and metier lv6, 
# for the last two years. 
# The answer to this request was one of the tasks defined for the ISSG Catch and Effort Overview in the period 2023-2024.



rm(list=(ls()))

library (stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)
library(readxl)
library(ggplot2)
library(patchwork)
library(data.table)

options(scipen = 999)
options(tibble.width = Inf)

gc()

## ======================= #
## Set params   -------------------------------- ####
## ======================= #

year_start <- 2023
year_end   <- 2025
# BA
#CEfilepath <- "RegionalOverviews/data_RDBES/002_prepared/20260416/RCG_BA/RDBES_RCG_BA_CE_2021_2025_prepared_20260416.Rdata"
#CSfilepath <- "RegionalOverviews/data_RDBES/002_prepared/20260503/RCG_BA/RDBES_RCG_BA_CS_2023_2025_prepared_20260507.Rdata"
#target_region <- 'RCG_BA' #  

# NA
#CEfilepath <- "RegionalOverviews/data_RDBES/002_prepared/20260416/RCG_NA/RDBES_RCG_NA_CE_2021_2025_prepared_20260416.Rdata"
#CSfilepath <- "RegionalOverviews/data_RDBES/002_prepared/20260503/RCG_NA/RDBES_RCG_NA_CS_2023_2025_prepared_20260503.Rdata"
#target_region <- 'RCG_NA' #  

# NSEA
CEfilepath <- "RegionalOverviews/data_RDBES/002_prepared/20260416/RCG_NSEA/RDBES_RCG_NSEA_CE_2021_2025_prepared_20260416.Rdata"
CSfilepath <- "RegionalOverviews/data_RDBES/002_prepared/20260503/RCG_NSEA/RDBES_RCG_NSEA_CS_2023_2025_prepared_20260503.Rdata"
target_region <- 'RCG_NSEA' #   

# Read Data  ------------------------------------------------------------------- ####
# ecoregion
ecoregion <- read.csv("RegionalOverviews/data/ecoregion_subareaICES.csv", header=T)

# sampling
load(CSfilepath)
head(cs_rcg)

# effort 
load(CEfilepath)
head(ce_rcg)

# Prepare data    -------------------------------------------------------------- ####

# .. Sampling     ---- ####

# check:
cs_rcg %>% 
  group_by( DEhierarchy, FTsampType) %>%
  summarise(NsampScheme = n_distinct(DEsampScheme)) %>%
  pivot_wider(names_from = FTsampType, values_from = NsampScheme)

# Some Hierarchies do not have FTsampType (OnShre/AtSea). We assign them to the most ussual sampling type according to each Hierarchy
cs <- cs_rcg %>%
  mutate( FTsampType = case_when(
    DEhierarchy %in% c(5, 7, 8, 9)  ~ "OnShore",  # Guidelines: Hierarchies 5,7,8,& 9 are most used for on-shore sampling, but can be used for at-sea sampling as well.
    DEhierarchy == 13               ~ "AtSea",    # Guidelines: Hierarchy 13 is most used for at-sea sampling, but can be used for on-shore sampling as well.
    TRUE                            ~ FTsampType ) ) 

# Filter
cs <- cs %>%
  filter(FTsampType == "AtSea") %>%
  select(
    FTid, FOid, LEid, DEhierarchy, DEyear,
    SDctry, DEsampScheme, FTsampType,
    FOgear, FOmetier6, FOarea, AreaMap,
    SSobsTyp, SSid, SAid, FMid, BVid
  )


# add ecoregion
cs <- cs %>% left_join(ecoregion, by = c("AreaMap" = "ICESsubarea"))
cs %>% filter(is.na(EcoRegion)) 

cs <- cs %>%
  mutate( EcoRegion = case_when(
    FOarea %in% c("27.4")  ~ "Greater North Sea",  
    TRUE                   ~ EcoRegion ) ) 


# add fractional trips
cs <- cs %>% group_by(FTid) %>% mutate(FTidFrac = 1/length(FTid)) %>% ungroup()

# Check reported data
cs %>% 
  group_by( DEhierarchy, FTsampType) %>%
  summarise(NsampScheme = n_distinct(DEsampScheme, na.rm=T),
            nFMid       = n_distinct(FMid, na.rm=T),
            nBVid       = n_distinct(BVid, na.rm=T)) 

cs %>% 
  group_by( SDctry, DEsampScheme, DEhierarchy, FTsampType) %>%
  summarise(nrows       = n(),
            nSSid       = n_distinct(SSid, na.rm=T),
            nSAid       = n_distinct(SAid, na.rm=T),
            nFMid       = n_distinct(FMid, na.rm=T),
            nBVid       = n_distinct(BVid, na.rm=T),
            .groups = "drop") %>% 
  filter(nFMid == 0 & nBVid == 0) %>%
  as.data.frame()

# Baltic:
# There are two sampling schemes without any individual measured:
# SDctry          DEsampScheme DEhierarchy FTsampType nFMid nBVid
# 1     DK  DNK_EM_PETS_sampling           1      AtSea     0     0
# 2     SE SWE_CommEMAtSea_RouCF           1      AtSea     0     0
    # cs_rcg %>% filter(DEsampScheme =="DNK_EM_PETS_sampling") %>% distinct(SSobsTyp)  # SSobsTyp == "Imagery"
    # cs_rcg %>% filter(DEsampScheme =="DNK_EM_PETS_sampling" & SSobsTyp == "Imagery")  # H1. se para en SS/SA
    # 
    # cs_rcg %>% filter(DEsampScheme =="SWE_CommEMAtSea_RouCF") %>% distinct(SSobsTyp)  # SSobsTyp == NA y "Imagery"
    # cs_rcg %>% filter(DEsampScheme =="SWE_CommEMAtSea_RouCF" & is.na(SSobsTyp))       # H1. se para en FO
    # cs_rcg %>% filter(DEsampScheme =="SWE_CommEMAtSea_RouCF" & SSobsTyp == "Imagery") # H1. se para en SS/SA

# NA:
# AZTI has two differnet samplin gschemes for discards and PETs sampling: 
# ESP-AZTI_DCF_Onboard_Sampling and ESP-AZTI_DCF_OnboardPETs_Sampling. 
# The sampling effort will be doubled if you sum all lines. For this analysys ESP-AZTI_DCF_Onboard_Sampling is removed
  cs %>% filter(DEsampScheme =="ESP-AZTI_DCF_Onboard_Sampling" ) 
  cs <- cs %>% filter(DEsampScheme !="ESP-AZTI_DCF_Onboard_Sampling" ) 

# NSEA:
# se juntan las dos acsuisticas anteriores
    
    
# check mareas con diferente metier/area/ecoregion en la misma marea
sum(cs$FTidFrac)
n_distinct(cs$FTid)

# res <- cs %>% group_by(FTid, DEhierarchy, DEyear, SDctry, DEsampScheme, FTsampType) %>%   # , FTsampler
#   summarise(Nmetier6     = n_distinct(FOmetier6),
#             Narea        = n_distinct(FOarea),
#             Necoregion   = n_distinct(EcoRegion)) %>%
#   ungroup()
# 
# res %>% filter(is.na(FTid)) %>% distinct(DEhierarchy, SDctry, DEsampScheme)
# res %>% filter(Nmetier6   > 1)
# res %>% filter(Narea      > 1)
# res %>% filter(Necoregion > 1) 
#write.table(res, file = "RegionalOverviews/overviews_reports_RDBES/etp_sampling_overview_RDBES/results/CheckTripsDiffAreaMetierEcoregion.csv", sep=",", dec=".", row.names = F)


# .. Effort     ---- ####

head(ce_rcg)

# Add ecoregion
ce <- ce_rcg %>% left_join(ecoregion, by = c("AreaMap" = "ICESsubarea"))
ce %>% filter(is.na(EcoRegion)) %>% distinct(CEarea, AreaMap)

ce <- ce %>%
  mutate( EcoRegion = case_when(
    CEarea %in% c("27.3.a")  ~ "Greater North Sea",  
    TRUE                     ~ EcoRegion ) ) 


# Add gear
ce$CEgear                       <- substr(ce$CEmetier6,1,3)
ce$CEgear[ce$CEgear=="PS_"] <- "PS"

# summary table
sumCE <- ce %>% filter(CEyear %in% c(2023:2025)) %>%
  group_by( CEyear , CEvesselFlagCountry, CEgear, CEmetier6, CEarea , EcoRegion) %>%
  summarize(Ntrips         = sum(CEnumberOfFractionalTrips),
            OffFishingDays = sum(CEofficialFishingDays),
            .groups = "drop")

# Write table     
#write.table(sumCE, file = "RegionalOverviews/overviews_reports_RDBES/etp_sampling_overview_RDBES/results/PETs_RDBES_Effort.csv", sep=",", dec=".", row.names = F)



# Merge data      -------------------------------------------------------------- ####
TableSum_2 <- cs %>% group_by(DEyear,	SDctry,	EcoRegion,	FOgear) %>% #, FTsampler
  summarise(Ntrips = sum(FTidFrac, na.rm=T), .groups = "drop")  %>%
  mutate(across(c(DEyear, SDctry, EcoRegion, FOgear), as.character))

sumCE_2 <- sumCE %>% group_by(CEyear, CEvesselFlagCountry, CEgear, EcoRegion) %>%
  summarize(CE.Ntrips         = sum(Ntrips, na.rm=T),
            CE.OffFishingDays = sum(OffFishingDays, na.rm=T), .groups = "drop") %>%
  mutate(across(c(CEyear, CEvesselFlagCountry, EcoRegion, CEgear), as.character))

PETsTable <- TableSum_2 %>% full_join(sumCE_2, 
                                      by = c("DEyear"      = "CEyear",
                                             "SDctry"      = "CEvesselFlagCountry",
                                             "FOgear"      = "CEgear",
                                             "EcoRegion"   = "EcoRegion"))


PETsTable %>% filter(is.na(Ntrips))
PETsTable %>% filter(is.na(CE.Ntrips) | is.na(CE.OffFishingDays))

PETsTable %>% filter(is.na(Ntrips))
PETsTable %>% filter(Ntrips == 0 | CE.Ntrips == 0 )

# PETsTable <- PETsTable %>%
#   mutate(  Ntrips            = replace_na(Ntrips, 0),
#            CE.Ntrips         = replace_na(CE.Ntrips, 0),
#            CE.OffFishingDays = replace_na(CE.OffFishingDays, 0) )


write.table(PETsTable, file = paste("RegionalOverviews/overviews_reports_RDBES/etp_sampling_overview_RDBES/results/PETsRDBES",
                                    year_start, year_end, target_region, ".csv", sep="_"), 
            sep=",", dec=".", row.names = F)


# observations
# FTarrivalDate, FTdepartureDate, FTarrivalTime and FTdepartureTime are sometimes empty. Therefore, the number of days at sea is not a good metric to measure sampling

# Hierarchy 7,8,9 and 13 do not have FT table and therefore do not have FTsampType (OnShre/AtSea). We assign them to the most ussual sampling type according to each Hierarchy
# Hierarchy H4 does not have the FO table and therefore we don't have information about the metiers and the area

# cs prepared data generated with createRDBESEstObject() does not pick up the variables:
  # FTsampler and OSampler (control authority/ cameras/ observers/ self-sampling).
  # SAnumberTotal and SAnumberSampled (the number of individuals samped, which is relevant for ETP species)

# There are two sampling schemes without any individual measured:
  # SDctry          DEsampScheme DEhierarchy FTsampType nFMid nBVid
  # 1     DK  DNK_EM_PETS_sampling           1      AtSea     0     0  # SSobsTyp == "Imagery". H1. stops in SS/SA
  # 2     SE SWE_CommEMAtSea_RouCF           1      AtSea     0     0  # SSobsTyp == NA & "Imagery". H1. NA: stops in FO. "Imagery" stops in SS/SA

# AZTI has ESP-AZTI_DCF_Onboard_Sampling and ESP-AZTI_DCF_OnboardPETs_Sampling. the sampling effort may be doubled if you sum all lines

#♥ en palangre no se distingu ealtura de bajura. nuestrso muestreos son de bajura


# Plots      -------------------------------------------------------------- ####
PETsTable %>% filter(is.na(SDctry))
PETsTable %>% filter(is.na(FOgear))
cs %>% filter(is.na(FOgear)) %>% distinct(DEhierarchy)  # hierarchy 4 does not provide info on the gear


# Plot by ecoregion

ecoregions <- unique(PETsTable$EcoRegion)

for (i in ecoregions) {
  
  p1 <- ggplot(
    dplyr::filter(PETsTable, EcoRegion == i), 
    aes(y = factor(SDctry), x = factor(FOgear))) +
  facet_wrap( EcoRegion ~ DEyear, ncol = 3) + #
  
  geom_tile(aes(fill = CE.Ntrips)) +   # ← SIN na.rm
  
  geom_point(aes(size = Ntrips),
             shape = 1, stroke = 1, na.rm = TRUE) +
  
  scale_fill_distiller(
    palette = "Spectral",
    direction = -1,
    trans = "log10",
    na.value = "transparent"   # ← color para CE.Ntrips == NA //options: "grey90", "white", "transparent"
  ) +
  
  labs(
    x ='Gear',
    y = 'Country',
    fill = "Total effort (trips)",
    size = "No. trips sampled",
    title = paste("Sampling At Sea - ", i)
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
windows()
  print(p1)
  
  
ggsave(
  filename = paste("RegionalOverviews/overviews_reports_RDBES/etp_sampling_overview_RDBES/results/plotSamplingAtSea",
                   target_region, i,".png", sep="_"),
  plot = p1,
  width = 12,
  height = 8,
  dpi = 300   )

print(paste(i, "done")) 
}
