###################################################################
# multiannual_case_study_overview_RDBES 
###################################################################
#
# By specifying the user´s selection and sourcing relevant scripts,
# this script produces the multiannual case study RDBES overview.  
# 
# The main structure of the overview is contained in 
# 
# "multiannual_case_study_overview_RDBES_template.Rmd"
# 
# But the following script should be considered the main source for 
# the RDBES multiannual overview and should be used to create the
# "multiannual_case_study_overview_RDBES.R" report. 
# 
###################################################################
# Authors: 
# - Kasia Krakówka [first draft]
# 
# Dev. notes: 
#
# - 10.06.2025 first draft
#
###################################################################

## Custom the overview
# Make your selection 
yearSelected = 2024
regionSelected = 'BA'# One of: 'BA', 'NA', 'NSEA'
dataprepDate = 20250521 # Date on which data where prepared.
dataprepDateCL = 20250527

## Set wd 
setwd("D:/RCG-RDBES-Overviews/") # Kasia machine
#setwd("Path to RCGs local repo")

## Load libraries
source("RegionalOverviews/overviews_reports_RDBES/case_studies_overviews_RDBES/scripts/loadLibraries.R")

## Load functions 
source("RegionalOverviews/overviews_reports_RDBES/case_studies_overviews_RDBES/scripts/loadFunctions.R")

# Parameters are defined based on user selection
source("RegionalOverviews/overviews_reports_RDBES/case_studies_overviews_RDBES/scripts/parametersDefinitionMultiannual.R")

## Load prepared data
source("RegionalOverviews/overviews_reports_RDBES/case_studies_overviews_RDBES/scripts/loadDataMultiannual.R")

## Load UNLOCODE data/ shp files
source("RegionalOverviews/overviews_reports_RDBES/case_studies_overviews_RDBES/scripts/loadMaps.R")

## Source the .rmd file producing the overview
rmdReport <- file.path("RegionalOverviews/overviews_reports_RDBES/case_studies_overviews_RDBES/multiannual_case_study_overview_RDBES_template.Rmd")
rmarkdown::render(
  rmdReport,
  params = params,
  output_file = paste0('results/MultiannualCaseStudyOverview_', params$year ,'_', params$region, '.html'), # reports saved into results folder
  envir = new.env(parent = globalenv()),
  encoding = 'UTF-8'
)

