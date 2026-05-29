

# Read RDBES data -------------------------------------------------------------- ####
# CS: sampling data
load(data_dir_CS)

# CE: effort data
load(data_dir_CE)

# species list
IS <- read.csv(paste0(data_dir_SL, "IndividualSpeciesInSpeciesList.csv"), header=T)
SL <- read.csv(paste0(data_dir_SL, "SpeciesList.csv"), header=T)


# Read aux files  -------------------------------------------------------------- ####
# ecoregion
ecoregion <- read.csv("RegionalOverviews/data/ecoregion_subareaICES.csv", header=T)

# conversion sp
AsfisWorms <- read.csv("RegionalOverviews/data/ASFIS_WoRMS_updt2025.csv", header = T)

# ETP species list (ICES 2026)
etpList    <- read_excel("RegionalOverviews/overviews_reports_RDBES/etp_sampling_overview_RDBES/ICES_ETP_bycatch_species_2026.xlsx", sheet ="Bycatch_ETP_species_region")
