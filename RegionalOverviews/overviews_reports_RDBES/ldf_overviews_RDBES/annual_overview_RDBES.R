###################################################################
# Annual_overview_RDBES 
###################################################################
#
# By specifying the user´s selection and sourcing relevant scripts,
# this script produces the annual RDBES overview.  
# 
# The main structure of the overview is contained in 
# 
# "annual_overview_RDBES_template.Rmd"
# 
# But the following script should be considered the main source for 
# the RDBES annual overview and should be used to create the
# "annual_overview_RDBES.R" report. 
# 
###################################################################
# Authors: 
# - Marta Suska [first draft]
# - Eros Quesada
# - Kasia Krakówka
# 
# Dev. notes: 
#
# - 20240130: Created  
# - 20240207: Formatted, disassembled in scripts to be sourced. 
# - 20240220: Added fleet register
#
###################################################################

## Custom the overview

# Make your selection 
yearsSelected = c(2021, 2022, 2023, 2024)
year = 2022
regionSelected = 'LDF'  
downloadDataFromSP = 0  # One of: 1 (download from Share Point prepared data) or 0 (do not download and use data prepared locally - using "001_read_and_prepare_data_RDBES_CL_CE.R")
dataprepDate = 20250724 # Date on which data where prepared. If prepared data are downloaded from ICES SP, then this is the date used for the data folder name on the ICES SP. 
data_dir = paste0('RegionalOverviews/data_RDBES/002_prepared/', dataprepDate, '/RCG_', regionSelected)
CLfileName = paste0('RDBES_RCG_', regionSelected, '_CL_',yearsSelected[1],'_',rev(yearsSelected)[1],'_prepared_', dataprepDate)
CEfileName = paste0('RDBES_RCG_', regionSelected, '_CE_',yearsSelected[1],'_',rev(yearsSelected)[1],'_prepared_', dataprepDate)
spatialDataPath = 'RegionalOverviews/data'
auxDataPath = 'RegionalOverviews/data'
table_dir = "../results/tables/"
#RDBES_download_date = '01/01/2000'

## Set wd 
# setwd("//storage-lk.slu.se/home$/erqu0001/Desktop/HLab_GH/Public_Eros/RCGs") # eros machine
# setwd("D:/RegionalOverviewsLDF/") # Kasia machine
#setwd("Path to RCGs local repo")



################################################################################################################



## Load libraries
source("RegionalOverviews/overviews_reports_RDBES/ldf_overviews_RDBES/scripts/loadLibraries.R")

## Load functions 
source("RegionalOverviews/overviews_reports_RDBES/ldf_overviews_RDBES/scripts/loadFunctions.R")

# Parameters are defined based on user selection
source("RegionalOverviews/overviews_reports_RDBES/ldf_overviews_RDBES/scripts/parametersDefinition.R")

## Download prepared data from SP accordingly, if required. 
# If the download is selected (downloadDataFromSP == 1), then prepared data are downloaded from Share Point. 
source("RegionalOverviews/overviews_reports_RDBES/ldf_overviews_RDBES/scripts/downloadPreparedData.R")

## Load prepared data
source("RegionalOverviews/overviews_reports_RDBES/ldf_overviews_RDBES/scripts/loadData.R")

## Load spatial data
source("RegionalOverviews/overviews_reports_RDBES/ldf_overviews_RDBES/scripts/loadSpatialData.R")

### Load auxiliary data
source("RegionalOverviews/overviews_reports_RDBES/ldf_overviews_RDBES/scripts/loadAuxiliaryData.R")


################################################################################################################


## Source the .rmd file producing the overview
rmdReport <- file.path("RegionalOverviews/overviews_reports_RDBES/ldf_overviews_RDBES/annual_overview_RDBES_template_LDF.Rmd")
rmarkdown::render(
  rmdReport,
  params = params,
  output_file = paste0('results/AnnualOverview_', params$year ,'_', params$region, '.html'), # reports saved into results folder
  envir = new.env(parent = globalenv()),
  encoding = 'UTF-8'
)
