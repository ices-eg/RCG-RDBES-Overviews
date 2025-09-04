###################################################################
# annual_sampling_overview_RDBES 
###################################################################
#
# By specifying the user´s selection and sourcing relevant scripts,
# this script produces the annual RDBES overview.  
# 
# The main structure of the overview is contained in 
# 
# "annual_sampling_overview_RDBES_template.Rmd"
# 
# But the following script should be considered the main source for 
# the RDBES annual overview and should be used to create the
# "annual_sampling_overview_RDBES.R" report. 
# 
###################################################################
# Authors: 
# - Kasia Krakówka [first draft]
# -  Ana Cláudia Fernandes
# 
# Dev. notes: 
#
# - 12.02.2025 first draft based on annual_overview_RDBES
# - 05.05.2025 CL added
#
###################################################################

## Custom the overview
# Make your selection 
yearSelected = 2024
regionSelected = 'BA'# One of: 'BA', 'NA', 'NSEA'
dataprepDate = 20250521 # Date on which data where prepared.
dataprepDateCL = 20250527
dataprepDateCE = 20250527

## Set wd 
#setwd("Path to RCGs local repo")
#setwd("F:/Documentos/PNAB/2025/000_RCG_IntersessionalWork/Fisheries_Overviews/RCG-RDBES-Overviews-master/")
#setwd("C:/Use/0_GitHub/RCG-RDBES-Overviews") # Lucia's machine
setwd("D:/RCG-RDBES-Overviews/") # Kasia machine

## Load libraries
source("RegionalOverviews/overviews_reports_RDBES/sampling_overviews_RDBES/scripts/loadLibraries.R")

## Load functions 
source("RegionalOverviews/overviews_reports_RDBES/sampling_overviews_RDBES/scripts/loadFunctions.R")

# Parameters are defined based on user selection
source("RegionalOverviews/overviews_reports_RDBES/sampling_overviews_RDBES/scripts/parametersDefinition.R")

## Load prepared data
source("RegionalOverviews/overviews_reports_RDBES/sampling_overviews_RDBES/scripts/loadData.R")

load("RegionalOverviews/data/UNLOCODE.rData")

## Source the .rmd file producing the overview
rmdReport <- file.path("RegionalOverviews/overviews_reports_RDBES/sampling_overviews_RDBES/annual_sampling_overview_RDBES_template.Rmd")
rmarkdown::render(
  rmdReport,
  params = params,
  output_file = paste0('results/AnnualSamplingOverview_', params$year ,'_', params$region, '.html'), # reports saved into results folder
  envir = new.env(parent = globalenv()),
  encoding = 'UTF-8'
)

