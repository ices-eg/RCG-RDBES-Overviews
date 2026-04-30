
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

setwd("C:\\Use\\0_Lucia\\6_Working groups\\RCG_ISSG\\2023-2024\\Catch Effort and Sampling Overviews\\PETs\\")

# Read Ecoregion    ------------------------------------------------------------ ####
ecoregion <- read.csv("Data/ecoregion_subareaICES.csv", header=T)

# Read Effort Data     --------------------------------------------------------- ####
#ce <- read.table("Data\\CE Effort RDBES RCG NANASEA Baltic year 2021-2023 2024_05_21\\CommercialEffort.csv", sep=",", dec=".", header=T, stringsAsFactors = F)  ## da error
ce <- data.table::fread("Data\\CE Effort RDBES RCG NANASEA Baltic year 2021-2023 2024_05_21\\CommercialEffort.csv", stringsAsFactors=FALSE, verbose=FALSE, sep=",", na.strings="NULL",quote = "")

# Read FishingTrip to identify sampling at sea   ------------------------------- ####

FishingTripFiles <- list.files(path = "Data\\CS Sample RDBES RCG NANASEA Baltic year 2023 2024_05_23", pattern = "FishingTrip.csv",  recursive = TRUE, full.names = TRUE)
FishingTripInfo <- data.frame(file = FishingTripFiles, hierarchy = gsub(".*?(H\\d{1,2}).*", "\\1", FishingTripFiles) )
FishingTripAll <- data.frame()
for (i in 1:length(FishingTripInfo$file)) {
  # Read the file and assign it a unique name
  temp <- read.table(FishingTripInfo$file[i], sep=",", dec=".", header=T, stringsAsFactors = F)
  if (nrow(temp)>0) {temp$FThierarchy <- FishingTripInfo$hierarchy[i]}
  FishingTripAll <- rbind(FishingTripAll, temp) 
}

FishingTripAll %>% group_by(FThierarchy, FTsamplingType, FTsampler) %>% summarise(Ntrips = n_distinct(FTid))
FishingTripAll %>% group_by(FThierarchy, FTsamplingType, FTsampler) %>% summarise(Ntrips = n_distinct(FTid)) %>% filter(FTsamplingType == "AtSea")

# H1, H2, H3, H4

# Explore Time Date
# FTarrivalDate and FTdepartureDate are mandatory for FTsamplingType == "AtSea"
# FTarrivalTime and FTdepartureTime are mandatory for FTsamplingType == "AtSea"and FTsampler=="Observer"
# There are registers that don't comply with this rule
# The numbr of days at sea is not a good metric to measure sampling
    unique(FishingTripAll$FTsampler)
    FishingTripAll$FTarrivalDate[FishingTripAll$FTarrivalDate==""     & FishingTripAll$FTsamplingType=="AtSea"]
    FishingTripAll$FTarrivalTime[FishingTripAll$FTarrivalTime==""     & FishingTripAll$FTsamplingType=="AtSea" & FishingTripAll$FTsampler=="Observer"]
    FishingTripAll$FTdepartureDate[FishingTripAll$FTdepartureDate=="" & FishingTripAll$FTsamplingType=="AtSea"]
    FishingTripAll$FTdepartureTime[FishingTripAll$FTdepartureTime=="" & FishingTripAll$FTsamplingType=="AtSea" & FishingTripAll$FTsampler=="Observer"]
    
    FishingTripAll %>% group_by(FThierarchy, FTsamplingType, FTsampler) %>% 
                        filter(FTsamplingType == "AtSea") %>%
                        summarise(nTrips          = n_distinct(FTid),
                                  emptyFTarrivalDate   = length(FTarrivalDate[FTarrivalDate == ""]),
                                  emptyFTarrivalTime   = length(FTarrivalTime[FTarrivalTime == ""]),
                                  emptyFTdepartureDate = length(FTdepartureDate[FTdepartureDate == ""]),
                                  emptyFTdepartureTime = length(FTdepartureTime[FTdepartureTime == ""]))


# Create an empty list to store data frames   ---------------------------------- ####

    
    
# H1: DE>SD>VS>FT>FO>SS>SA   --------------------------------------------------- #
file_names <- list.files(path = "Data/CS Sample RDBES RCG NANASEA Baltic year 2023 2024_05_23/H1", pattern = ".csv")
file_names <- list.files(path = "Data/CS Sample RDBES RCG NANASEA Baltic year 2021-2023 2024_05_13/H1", pattern = ".csv")
file_names
dataH1 <- list()
for (i in 1:length(file_names)) {
  # Read the file and assign it a unique name
  dataH1[[gsub(".csv", "", file_names[i])]] <- read.table(file.path("Data/CS Sample RDBES RCG NANASEA Baltic year 2021-2023 2024_05_13/H1",  file_names[i]), sep=",", dec=".", header=T, stringsAsFactors = F)
}

# H2: DE>SD>FT>FO>SS>SA    -------------------------------------------------- #
file_names <- list.files(path = "Data/CS Sample RDBES RCG NANASEA Baltic year 2023 2024_05_23/H2", pattern = ".csv")
file_names <- list.files(path = "Data/CS Sample RDBES RCG NANASEA Baltic year 2021-2023 2024_05_13/H2", pattern = ".csv")
file_names
dataH2 <- list()
for (i in 1:length(file_names)) {
  # Read the file and assign it a unique name
  dataH2[[gsub(".csv", "", file_names[i])]] <- read.table(file.path("Data/CS Sample RDBES RCG NANASEA Baltic year 2021-2023 2024_05_13/H2",  file_names[i]), sep=",", dec=".", header=T, stringsAsFactors = F)
}

# H3: DE>SD>TE>VS>FT>FO>SS>SA    ----------------------------------------------- #
file_names <- list.files(path = "Data/CS Sample RDBES RCG NANASEA Baltic year 2023 2024_05_23/H3", pattern = ".csv")
file_names <- list.files(path = "Data/CS Sample RDBES RCG NANASEA Baltic year 2021-2023 2024_05_13/H3", pattern = ".csv")
file_names
dataH3 <- list()
for (i in 1:length(file_names)) {
  # Read the file and assign it a unique name
  dataH3[[gsub(".csv", "", file_names[i])]] <- read.table(file.path("Data/CS Sample RDBES RCG NANASEA Baltic year 2021-2023 2024_05_13/H3",  file_names[i]), sep=",", dec=".", header=T, stringsAsFactors = F)
  }

# H4: DE>SD>OS>FT>LE>SS>SA    -------------------------------------------------- #
file_names <- list.files(path = "Data/CS Sample RDBES RCG NANASEA Baltic year 2023 2024_05_23/H4", pattern = ".csv")
file_names <- list.files(path = "Data/CS Sample RDBES RCG NANASEA Baltic year 2021-2023 2024_05_13/H4", pattern = ".csv")
file_names
dataH4 <- list()
for (i in 1:length(file_names)) {
  # Read the file and assign it a unique name
  dataH4[[gsub(".csv", "", file_names[i])]] <- read.table(file.path("Data/CS Sample RDBES RCG NANASEA Baltic year 2021-2023 2024_05_13/H4",  file_names[i]), sep=",", dec=".", header=T, stringsAsFactors = F)
}


table(dataH1$FishingTrip$FTsamplingType)  # at sea: 9700
table(dataH2$FishingTrip$FTsamplingType)  # at sea: 258
table(dataH3$FishingTrip$FTsamplingType)  # at sea: 502
table(dataH4$FishingTrip$FTsamplingType)  # at sea: 49





# Save and Load data  ---------------------------------------------------------- ####

save(dataH1, dataH2, dataH3, dataH4, file="RDBESdata.Rdata")

load( file="RDBESdata.Rdata")

# Explore Samplig data  -------------------------------------------------------- ####
str(dataH3)
head(dataH3$SamplingDetails)
head(dataH3$Design)
head(dataH3$TemporalEvent)
head(dataH3$VesselSelection)
head(dataH3$FishingTrip)
head(dataH3$FishingOperation)
head(dataH3$SpeciesSelection)
head(dataH3$Sample)
head(dataH3$FrequencyMeasure)
head(dataH3$BiologicalVariable)

# ..H1   ------------------------------------------------------------------------- ####
# H1: DE>SD>VS>FT>FO>SS>SA

# link variables
dataH1$FishingOperation$VSid               <- dataH1$FishingTrip$VSid           [match(dataH1$FishingOperation$FTid, dataH1$FishingTrip$FTid)]
dataH1$FishingOperation$FTsamplingType     <- dataH1$FishingTrip$FTsamplingType [match(dataH1$FishingOperation$FTid, dataH1$FishingTrip$FTid)]
dataH1$FishingOperation$FTsampler          <- dataH1$FishingTrip$FTsampler      [match(dataH1$FishingOperation$FTid, dataH1$FishingTrip$FTid)]
dataH1$FishingOperation$VSid               <- dataH1$FishingTrip$VSid           [match(dataH1$FishingOperation$FTid, dataH1$FishingTrip$FTid)]
dataH1$FishingOperation$SDid               <- dataH1$VesselSelection$SDid       [match(dataH1$FishingOperation$VSid, dataH1$VesselSelection$VSid)]
dataH1$FishingOperation$DEid               <- dataH1$SamplingDetails$DEid       [match(dataH1$FishingOperation$SDid, dataH1$SamplingDetails$SDid)]
dataH1$FishingOperation$SDcountry          <- dataH1$SamplingDetails$SDcountry  [match(dataH1$FishingOperation$SDid, dataH1$SamplingDetails$SDid)]
dataH1$FishingOperation$DEsamplingScheme   <- dataH1$Design$DEsamplingScheme    [match(dataH1$FishingOperation$DEid, dataH1$Design$DEid)]
dataH1$FishingOperation$DEyear             <- dataH1$Design$DEyear              [match(dataH1$FishingOperation$DEid, dataH1$Design$DEid)]
dataH1$FishingOperation$DEhierarchy        <- dataH1$Design$DEhierarchy         [match(dataH1$FishingOperation$DEid, dataH1$Design$DEid)]

dataH1$FishingOperation$FOmetier        <- ecoregion$EcoRegion               [match(dataH1$FishingOperation$FOarea, ecoregion$ICESsubarea)]
dataH1$FishingOperation$FOecoregion        <- ecoregion$EcoRegion               [match(dataH1$FishingOperation$FOarea, ecoregion$ICESsubarea)]
dataH1$FishingOperation %>% filter(is.na(FOecoregion)) %>% distinct(FOarea)
dataH1$FishingOperation$FOecoregion[dataH1$FishingOperation$FOarea == "27" ] <- 27


# filter
dataH1$FishingOperation <- dataH1$FishingOperation %>% filter(FTsamplingType=="AtSea" & DEyear %in% c(2022,2023))

# select variables
sumH1 <- dataH1$FishingOperation %>% select(FTid, FOid, DEhierarchy, DEyear, SDcountry, DEsamplingScheme, FTsamplingType, FTsampler, FOgear, FOmetier6, FOarea, FOecoregion) 



# ..H2   ------------------------------------------------------------------------- ####
# H2: DE>SD>FT>FO>SS>SA
# link variables
dataH2$FishingOperation$SDid               <- dataH2$FishingTrip$SDid           [match(dataH2$FishingOperation$FTid, dataH2$FishingTrip$FTid)]
dataH2$FishingOperation$FTsamplingType     <- dataH2$FishingTrip$FTsamplingType [match(dataH2$FishingOperation$FTid, dataH2$FishingTrip$FTid)]
dataH2$FishingOperation$FTsampler          <- dataH2$FishingTrip$FTsampler      [match(dataH2$FishingOperation$FTid, dataH2$FishingTrip$FTid)]
dataH2$FishingOperation$DEid               <- dataH2$SamplingDetails$DEid       [match(dataH2$FishingOperation$SDid, dataH2$SamplingDetails$SDid)]
dataH2$FishingOperation$SDcountry          <- dataH2$SamplingDetails$SDcountry  [match(dataH2$FishingOperation$SDid, dataH2$SamplingDetails$SDid)]
dataH2$FishingOperation$DEsamplingScheme   <- dataH2$Design$DEsamplingScheme    [match(dataH2$FishingOperation$DEid, dataH2$Design$DEid)]
dataH2$FishingOperation$DEyear             <- dataH2$Design$DEyear              [match(dataH2$FishingOperation$DEid, dataH2$Design$DEid)]
dataH2$FishingOperation$DEhierarchy        <- dataH2$Design$DEhierarchy         [match(dataH2$FishingOperation$DEid, dataH2$Design$DEid)]

dataH2$FishingOperation$FOecoregion        <- ecoregion$EcoRegion               [match(dataH2$FishingOperation$FOarea, ecoregion$ICESsubarea)]
dataH2$FishingOperation %>% filter(is.na(FOecoregion)) %>% distinct(FOarea)


# filter
dataH2$FishingOperation <- dataH2$FishingOperation %>% filter(FTsamplingType=="AtSea" & DEyear %in% c(2022,2023))

# select variables
sumH2 <- dataH2$FishingOperation %>% select(FTid, FOid, DEhierarchy, DEyear, SDcountry, DEsamplingScheme, FTsamplingType, FTsampler, FOgear, FOmetier6, FOarea, FOecoregion) 



# ..H3   ------------------------------------------------------------------------- ####
# H3: DE>SD>TE>VS>FT>FO>SS>SA
# link variables
dataH3$FishingOperation$VSid               <- dataH3$FishingTrip$VSid           [match(dataH3$FishingOperation$FTid, dataH3$FishingTrip$FTid)]
dataH3$FishingOperation$FTsamplingType     <- dataH3$FishingTrip$FTsamplingType [match(dataH3$FishingOperation$FTid, dataH3$FishingTrip$FTid)]
dataH3$FishingOperation$FTsampler          <- dataH3$FishingTrip$FTsampler      [match(dataH3$FishingOperation$FTid, dataH3$FishingTrip$FTid)]
dataH3$FishingOperation$VSid               <- dataH3$FishingTrip$VSid           [match(dataH3$FishingOperation$FTid, dataH3$FishingTrip$FTid)]
dataH3$FishingOperation$TEid               <- dataH3$VesselSelection$TEid       [match(dataH3$FishingOperation$VSid, dataH3$VesselSelection$VSid)]
dataH3$FishingOperation$SDid               <- dataH3$TemporalEvent$SDid       [match(dataH3$FishingOperation$TEid, dataH3$TemporalEvent$TEid)]
dataH3$FishingOperation$DEid               <- dataH3$SamplingDetails$DEid       [match(dataH3$FishingOperation$SDid, dataH3$SamplingDetails$SDid)]
dataH3$FishingOperation$SDcountry          <- dataH3$SamplingDetails$SDcountry  [match(dataH3$FishingOperation$SDid, dataH3$SamplingDetails$SDid)]
dataH3$FishingOperation$DEsamplingScheme   <- dataH3$Design$DEsamplingScheme    [match(dataH3$FishingOperation$DEid, dataH3$Design$DEid)]
dataH3$FishingOperation$DEyear             <- dataH3$Design$DEyear              [match(dataH3$FishingOperation$DEid, dataH3$Design$DEid)]
dataH3$FishingOperation$DEhierarchy        <- dataH3$Design$DEhierarchy         [match(dataH3$FishingOperation$DEid, dataH3$Design$DEid)]

dataH3$FishingOperation$FOecoregion        <- ecoregion$EcoRegion               [match(dataH3$FishingOperation$FOarea, ecoregion$ICESsubarea)]
dataH3$FishingOperation %>% filter(is.na(FOecoregion)) %>% distinct(FOarea)
dataH3$FishingOperation %>% group_by(FTid) %>% distinct(FOecoregion)


# filter
dataH3$FishingOperation <- dataH3$FishingOperation %>% filter(FTsamplingType=="AtSea" & DEyear %in% c(2022,2023))

# select variables
sumH3 <- dataH3$FishingOperation %>% select(FTid, FOid, DEhierarchy, DEyear, SDcountry, DEsamplingScheme, FTsamplingType, FTsampler, FOgear, FOmetier6, FOarea, FOecoregion) 


# ..H4   ------------------------------------------------------------------------- ####
# H4: DE>SD>OS>FT>LE>SS>SA
# This hierarchy does not have the FO table and therefore we don't have information about the metiers and the area
# link variables
dataH4$FishingTrip$SDid               <- dataH4$OnshoreEvent$SDid          [match(dataH4$FishingTrip$OSid, dataH4$OnshoreEvent$OSid)]
dataH4$FishingTrip$DEid               <- dataH4$SamplingDetails$DEid       [match(dataH4$FishingTrip$SDid, dataH4$SamplingDetails$SDid)]
dataH4$FishingTrip$SDcountry          <- dataH4$SamplingDetails$SDcountry  [match(dataH4$FishingTrip$SDid, dataH4$SamplingDetails$SDid)]
dataH4$FishingTrip$DEsamplingScheme   <- dataH4$Design$DEsamplingScheme    [match(dataH4$FishingTrip$DEid, dataH4$Design$DEid)]
dataH4$FishingTrip$DEyear             <- dataH4$Design$DEyear              [match(dataH4$FishingTrip$DEid, dataH4$Design$DEid)]
dataH4$FishingTrip$DEhierarchy        <- dataH4$Design$DEhierarchy         [match(dataH4$FishingTrip$DEid, dataH4$Design$DEid)]


# filter
dataH4$FishingTrip <- dataH4$FishingTrip %>% filter(FTsamplingType=="AtSea" & DEyear==2023)
dataH4$FishingTrip$FOmetier6    <- NA
dataH4$FishingTrip$FOgear       <- NA
dataH4$FishingTrip$FOarea       <- NA
dataH4$FishingTrip$FOecoregion  <- NA
dataH4$FishingTrip$FOid         <- NA

# select variables
sumH4 <- dataH4$FishingTrip %>% select(FTid, FOid, DEhierarchy, DEyear, SDcountry, DEsamplingScheme, FTsamplingType, FTsampler, FOgear, FOmetier6, FOarea, FOecoregion) 


# Merge all Hierarchies     ---------------------------------------------------- ####

TableSum <- rbind(sumH1,sumH2,sumH3,sumH4)
TableSum <- TableSum %>% group_by(FTid) %>% mutate(FTidFrac = 1/length(FTid))
TableSum <- TableSum %>% filter(DEsamplingScheme != "ESP-AZTI_DCF_Onboard_Sampling")  # quito el muestreo a borod de azti, para no duplicar

# check sampling scheme
TableSum %>% 
  group_by(DEhierarchy, DEyear, SDcountry, DEsamplingScheme, FTsamplingType, FTsampler) %>% 
  summarise(Ntrips = sum(FTidFrac, na.rm=T)) %>% 
  arrange(DEyear, SDcountry, DEsamplingScheme, FTsamplingType, FTsampler) %>%
  data.frame()

# check mareas con difernte metier/area/ecoregion en la misma marea
sum(TableSum$FTidFrac)
n_distinct(TableSum$FTid)

TableSum %>% group_by(FTid, DEhierarchy, DEyear, SDcountry, DEsamplingScheme, FTsamplingType, FTsampler) %>% 
  summarise(Nmetier6     = n_distinct(FOmetier6),
            Narea        = n_distinct(FOarea),
            Necoregion   = n_distinct(FOecoregion)) %>%
 write.table(file = "CheckTripsDiffAreaMetierEcoregion.csv", sep=",", dec=".", row.names = F)


dim(TableSum)
dim(TableSum %>% filter())
  
# check mareas concretas
dataH4$FishingTrip %>% filter(FTid==42213) # H4 sin info de area



# Write table     -------------------------------------------------------------- ####
write.table(TableSum, file = "PETsRDBES_2022_2023_Sampling.csv", sep=",", dec=".", row.names = F)


# Effort data      -------------------------------------------------------------- ####

head(ce)
ce$CEecoregion  <- ecoregion$EcoRegion [match(ce$CEarea, ecoregion$ICESsubarea)]
ce$CEgear       <- substr(ce$CEmetier6,1,3)
ce$CEgear[ce$CEgear=="PS_"] <- "PS"

sumCE <- ce %>% filter(CEyear %in% c(2022,2023)) %>%
  group_by( CEyear , CEvesselFlagCountry, CEgear, CEmetier6, CEarea , CEecoregion) %>%
  summarize(Ntrips         = sum(CEnumberOfFractionalTrips),
            OffFishingDays = sum(CEofficialFishingDays))

# Write table     -------------------------------------------------------------- ####
write.table(sumCE, file = "PETsRDBES_2022_2023_Effort.csv", sep=",", dec=".", row.names = F)



# Merge data      -------------------------------------------------------------- ####
TableSum_2 <- TableSum %>% group_by(DEyear,	SDcountry,	FOecoregion,	FOgear, FTsampler) %>%
  summarise(Ntrips = sum(FTidFrac, na.rm=T)) %>%
  pivot_wider(names_from = FTsampler, values_from = Ntrips, values_fill = 0, names_prefix = "SampledTrips.") 


sumCE_2 <- sumCE %>% group_by(CEyear, CEvesselFlagCountry, CEgear, CEecoregion) %>%
  summarize(CE.Ntrips         = sum(Ntrips),
            CE.OffFishingDays = sum(OffFishingDays))

PETsTable <- TableSum_2 %>% full_join(sumCE_2, 
                                      by = c("DEyear"      = "CEyear",
                                             "SDcountry"   = "CEvesselFlagCountry",
                                             "FOgear"      = "CEgear",
                                             "FOecoregion" = "CEecoregion"))

write.table(PETsTable, file = "PETsRDBES_2022_2023_all.csv", sep=",", dec=".", row.names = F)


# observations
# FTarrivalDate, FTdepartureDate, FTarrivalTime and FTdepartureTime are sometimes empty. Therefore, the number of days at sea is not a good metric to measure sampling
# Hierarchy H4 does not have the FO table and therefore we don't have information about the metiers and the area
# AZTI has ESP-AZTI_DCF_Onboard_Sampling and ESP-AZTI_DCF_OnboardPETs_Sampling. the sampling effort may be doubled if you sum all lines

#♥ en palangre no se distingu ealtura de bajura. nuestrso muestreos son de bajura


# preparar tabla para plots   -------------------------------------------------- ####
PETsTable0 <- PETsTable %>%   mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .))

# Plot General ntrips   -------------------------------------------------------- ####

p1 <- ggplot(PETsTable0 %>% filter(DEyear == 2023 ),
             aes(x = factor(SDcountry), y = CE.Ntrips, fill = factor(FOgear))) +
  geom_bar(stat = "identity") +
  facet_wrap(~ FOecoregion, scales = "free_y") +
  labs( title = "CE Ntrips (fractional) 2023" ) +
  theme_minimal(base_size = 14)
windows()
print(p1)



# Plot generic    ----------------------------------------------------------- ####
year <- 2023

nameEcoregion <- "Bay of Biscay"
nameCountry   <- c("ES", "FR", "PT")

nameEcoregion <- "Baltic Sea"
nameCountry   <- c("DE", "DK", "EE", "FI", "LT", "LV", "PL", "SE")

nameEcoregion <- "Greater North Sea"
nameCountry   <- c("BE", "DE", "DK", "FR", "NL", "PT")


# Crear el primer gráfico (barplot)
p1 <- ggplot(PETsTable0 %>% filter(DEyear == year & FOecoregion == nameEcoregion & 
                                    SDcountry %in% nameCountry),
             aes(x = factor(FOgear), y = CE.Ntrips)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ SDcountry, nrow = 1, scales = "free_x") +
  labs(title = paste(nameEcoregion, year), y = "Ntrips.CE") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) ) +
  labs(x=NULL)


p2 <- ggplot(PETsTable0 %>% filter(DEyear == year & FOecoregion == nameEcoregion & 
                                    SDcountry %in% nameCountry),
             aes(x = factor(FOgear), y = SampledTrips.Observer)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ SDcountry, nrow = 1, scales = "free_x") +
  labs(y = "Ntrips.Observer") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) ) +
  labs(x=NULL)


p3 <- ggplot(PETsTable0 %>% filter(DEyear == year & FOecoregion == nameEcoregion & 
                                    SDcountry %in% nameCountry),
             aes(x = factor(FOgear), y = SampledTrips.SelfSampling)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ SDcountry, nrow = 1, scales = "free_x") +
  labs(y = "Ntrips.SelfSampling") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) ) +
  labs(x=NULL)


p4 <- ggplot(PETsTable0 %>% filter(DEyear == year & FOecoregion == nameEcoregion & 
                                    SDcountry %in% nameCountry),
             aes(x = factor(FOgear), y = SampledTrips.Imagery)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ SDcountry, nrow = 1, scales = "free_x") +
  labs(x = "Gear", y = "Ntrips.Imagery") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) )

if(nameEcoregion %in% c("Baltic Sea", "Greater North Sea")) {combined_plot <- p1 / p2 / p3 / p4}
if(nameEcoregion %in% c("Bay of Biscay")) {combined_plot <- p1 / p2 }

windows()
print(combined_plot)
