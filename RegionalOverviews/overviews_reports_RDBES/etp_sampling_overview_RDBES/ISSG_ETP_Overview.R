# The ISSG PETs wants to use RDBES data to analyse the coverage of the sampling onboard 
# to identify high risk fisheries for PETs which may be potentially under sampled. 
# To that aim, they need an extraction showing the total effort (CE) and the sampling effort (CS), by ecoregion and metier lv6, 
# for the last two years. 
# The answer to this request was one of the tasks defined for the ISSG Catch and Effort Overview in the period 2023-2024.

# Load libraries  -------------------------------------------------------------- ##

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
library(readxl)
library(DT)

options(scipen = 999)
options(tibble.width = Inf)


# Set parameters libraries  --------------------------------------------------- ##

# years


year_start <- 2023
year_end   <- 2025

# RDBES Data Paths

# BA
# data_dir_CE <- "RegionalOverviews/data_RDBES/002_prepared/20260416/RCG_BA/RDBES_RCG_BA_CE_2021_2025_prepared_20260416.Rdata"
# data_dir_CS <- "RegionalOverviews/data_RDBES/002_prepared/20260503/RCG_BA/RDBES_RCG_BA_CS_2023_2025_prepared_20260507.Rdata"
# data_dir_SL <- 
# target_region <- 'RCG_BA' #

# NA
data_dir_CE <- "RegionalOverviews/data_RDBES/002_prepared/20260416/RCG_NA/RDBES_RCG_NA_CE_2021_2025_prepared_20260416.Rdata"
data_dir_CS <- "RegionalOverviews/data_RDBES/002_prepared/20260503/RCG_NA/RDBES_RCG_NA_CS_2023_2025_prepared_20260503.Rdata"
data_dir_SL <- "RegionalOverviews/data_RDBES/001_raw/RCG_NANSEA/HSL_NANSEA_2026_05_06_122750996/"
target_region <- 'RCG_NA' #

# NSEA
# data_dir_CE <- "RegionalOverviews/data_RDBES/002_prepared/20260416/RCG_NSEA/RDBES_RCG_NSEA_CE_2021_2025_prepared_20260416.Rdata"
# data_dir_CS <- "RegionalOverviews/data_RDBES/002_prepared/20260503/RCG_NSEA/RDBES_RCG_NSEA_CS_2023_2025_prepared_20260503.Rdata"
# data_dir_SL <- "RegionalOverviews/data_RDBES/001_raw/RCG_NANSEA/HSL_NANSEA_2026_05_06_122750996/"
# target_region <- 'RCG_NSEA' #   



## Load data  ------------------------------------------------------------------####
source("RegionalOverviews/overviews_reports_RDBES/etp_sampling_overview_RDBES/scripts/Load_data.R")

## Load functions  ------------------------------------------------------------------####
source("RegionalOverviews/overviews_reports_RDBES/etp_sampling_overview_RDBES/scripts/plot_sampling_effort.R")

## Check and correct data       ----------------------------------------------- ####

# Check Sampling Type by Hierarchy:
cs_rcg %>% 
  group_by( DEhierarchy, FTsampType) %>%
  summarise(NsampScheme = n_distinct(DEsampScheme)) %>%
  pivot_wider(names_from = FTsampType, values_from = NsampScheme)

  # Correct
  # Some Hierarchies do not have FTsampType (OnShre/AtSea). We assign them to the most ussual sampling type according to each Hierarchy
  cs <- cs_rcg %>%
    mutate( FTsampType = case_when(
      DEhierarchy %in% c(5, 7, 8, 9)  ~ "OnShore",  # Guidelines: Hierarchies 5,7,8,& 9 are most used for on-shore sampling, but can be used for at-sea sampling as well.
      DEhierarchy == 13               ~ "AtSea",    # Guidelines: Hierarchy 13 is most used for at-sea sampling, but can be used for on-shore sampling as well.
      TRUE                            ~ FTsampType ) ) 

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

# NSEA:
# se juntan las dos acsuisticas anteriores


# Correct:
cs %>% filter(DEsampScheme =="ESP-AZTI_DCF_Onboard_Sampling" ) 
cs_prep <- cs %>% filter(DEsampScheme !="ESP-AZTI_DCF_Onboard_Sampling" ) 



## ETP table       ------------------------------------------------------------- ####

# Prepare CS data  ##

# Filter
cs_tab <- cs_prep %>%
  filter(FTsampType == "AtSea") %>%
  select(
    FTid, FOid, LEid, DEhierarchy, DEyear,
    SDctry, DEsampScheme, FTsampType,
    FOgear, FOmetier6, FOarea, AreaMap,
    SSobsTyp, SSid, SAid, FMid, BVid
  )

# add ecoregion
cs_tab <- cs_tab %>% left_join(ecoregion, by = c("AreaMap" = "ICESsubarea"))
cs_tab %>% filter(is.na(EcoRegion)) 

cs_tab <- cs_tab %>%
  mutate( EcoRegion = case_when(
    FOarea %in% c("27.4")  ~ "Greater North Sea",  
    TRUE                   ~ EcoRegion ) ) 

# add fractional trips
cs_tab <- cs_tab %>% group_by(FTid) %>% mutate(FTidFrac = 1/length(FTid)) %>% ungroup()


# Prepare CE data  ##

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
            .groups = "drop")


# Merge data     ---- ###

TableSum_2 <- cs_tab %>% group_by(DEyear,	SDctry,	EcoRegion,	FOgear) %>% #, FTsampler
  summarise(NtripsSamp = round(sum(FTidFrac, na.rm=T), digits=1), .groups = "drop")  %>%
  mutate(across(c(DEyear, SDctry, EcoRegion, FOgear), as.character))

sumCE_2 <- sumCE %>% group_by(CEyear, CEvesselFlagCountry, CEgear, EcoRegion) %>%
  summarize(CE.Ntrips         = round(sum(Ntrips, na.rm=T), digits = 1), .groups = "drop") %>%
  mutate(across(c(CEyear, CEvesselFlagCountry, EcoRegion, CEgear), as.character))

PETsTable <- TableSum_2 %>% full_join(sumCE_2, 
                                      by = c("DEyear"      = "CEyear",
                                             "SDctry"      = "CEvesselFlagCountry",
                                             "FOgear"      = "CEgear",
                                             "EcoRegion"   = "EcoRegion"))

sort(unique(PETsTable$EcoRegion))



## Species List   ------------------------------------------------------------- ####

cs_list <- cs_rcg %>%
  mutate( FTsampType = case_when(
    DEhierarchy %in% c(5, 7, 8, 9)  ~ "OnShore",  # Guidelines: Hierarchies 5,7,8,& 9 are most used for on-shore sampling, but can be used for at-sea sampling as well.
    DEhierarchy == 13               ~ "AtSea",    # Guidelines: Hierarchy 13 is most used for at-sea sampling, but can be used for on-shore sampling as well.
    TRUE                            ~ FTsampType ) ) 

cs_list_temp <- cs_list %>%
  filter(FTsampType == "AtSea") %>%
  distinct( DEhierarchy, DEyear, SDctry, DEsampScheme, FTsampType, FOarea, AreaMap, SLspeclistName, SLcatchFrac, SSuseCalcZero) %>%
  left_join(ecoregion, by = c("AreaMap" = "ICESsubarea")) %>%
  left_join(select(SL, SLyear, SLcountry, SLspeciesListName, SLcatchFraction , SLid),
            by = c("DEyear" = "SLyear", "SDctry" = "SLcountry", "SLspeclistName" = "SLspeciesListName", "SLcatchFrac" = "SLcatchFraction")) 

cs_sinSL <- cs_list_temp %>%
  filter(is.na(SLid))    # some registers do not have species list reported

cs_conSL <- cs_list_temp %>%
  filter(!is.na(SLid)) %>%   
  left_join(select(IS, SLid, ISid, ISspeciesCode), by = c("SLid" = "SLid"), relationship = "many-to-many") %>%  # some sampling schemes use the same species list
  left_join(select(AsfisWorms, X3A_CODE, Scientific_name, AphiaID ),
            by = c("ISspeciesCode" = "AphiaID")) %>%
  left_join(select(etpList, Scientific_name, Ecoregion, Taxon, ETP_Common_name = Common_name), by = c("Scientific_name" = "Scientific_name", "EcoRegion" = "Ecoregion") )

cs_spList <- bind_rows(cs_conSL, cs_sinSL)


ETPsum <- cs_spList %>% 
  group_by(DEyear, SDctry, DEsampScheme, EcoRegion, SLspeclistName, SLcatchFrac) %>%
  mutate(Nsp  = n_distinct(ISspeciesCode),
         SSuseCalcZero = paste(unique(SSuseCalcZero), collapse = "_")) %>%
  group_by(DEyear, SDctry, DEsampScheme, EcoRegion, SLspeclistName, SLcatchFrac, SSuseCalcZero, Taxon, Nsp) %>%
  summarise(NspETP = n_distinct(ISspeciesCode[!is.na(ETP_Common_name)])) %>%
  pivot_wider(names_from = "Taxon", values_from =  "NspETP", values_fill = 0) %>%
  select(-'NA')



## Rmarkdown ------------------------------------------------------------------ ####

## Source the .rmd file producing the overview
rmdReport <- file.path("RegionalOverviews/overviews_reports_RDBES/etp_sampling_overview_RDBES/multiannual_ISSG_ETP_overview_RDBES_template.Rmd")
rmarkdown::render(
  rmdReport,
 # params = params,
  output_file = paste0('ISSG_ETP_Overview_', year_start ,'_', year_end, '_', target_region, '.html'), # reports saved into results folder
  envir = new.env(parent = globalenv()),
  encoding = 'UTF-8'
)
