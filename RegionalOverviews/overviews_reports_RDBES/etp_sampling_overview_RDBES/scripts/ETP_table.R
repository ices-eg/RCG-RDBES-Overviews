
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


# .. Prep Effort     ---- ####

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


# .. Merge data     ---- ####

TableSum_2 <- cs %>% group_by(DEyear,	SDctry,	EcoRegion,	FOgear) %>% #, FTsampler
  summarise(Ntrips = sum(FTidFrac, na.rm=T), .groups = "drop")  %>%
  mutate(across(c(DEyear, SDctry, EcoRegion, FOgear), as.character))

sumCE_2 <- sumCE %>% group_by(CEyear, CEvesselFlagCountry, CEgear, EcoRegion) %>%
  summarize(CE.Ntrips         = sum(Ntrips, na.rm=T), .groups = "drop") %>%
  mutate(across(c(CEyear, CEvesselFlagCountry, EcoRegion, CEgear), as.character))

PETsTable <- TableSum_2 %>% full_join(sumCE_2, 
                                      by = c("DEyear"      = "CEyear",
                                             "SDctry"      = "CEvesselFlagCountry",
                                             "FOgear"      = "CEgear",
                                             "EcoRegion"   = "EcoRegion"))

