###################################################################
# multiannual_sampling_overview_RDBES 
###################################################################
#
# By specifying the user´s selection and sourcing relevant scripts,
# this script produces the multiannual RDBES overview.  
# 
# The main structure of the overview is contained in 
# 
# "multiannual_sampling_overview_RDBES_template.Rmd"
# 
# But the following script should be considered the main source for 
# the RDBES multiannual overview and should be used to create the
# "multiannual_sampling_overview_RDBES.R" report. 
# 
###################################################################
# Authors: 
# - Kasia Krakówka [first draft]
# 
# Dev. notes: 
#
# - 10.06.2025 first draft based on annual_overview_RDBES
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
source("RegionalOverviews/overviews_reports_RDBES/sampling_overviews_RDBES/scripts/loadLibraries.R")

## Load functions 
source("RegionalOverviews/overviews_reports_RDBES/sampling_overviews_RDBES/scripts/loadFunctions.R")

# Parameters are defined based on user selection
source("RegionalOverviews/overviews_reports_RDBES/sampling_overviews_RDBES/scripts/parametersDefinitionMultiannual.R")

## Load prepared data
source("RegionalOverviews/overviews_reports_RDBES/sampling_overviews_RDBES/scripts/loadDataMultiannual.R")
load("RegionalOverviews/data/UNLOCODE.rData")

## Source the .rmd file producing the overview
rmdReport <- file.path("RegionalOverviews/overviews_reports_RDBES/sampling_overviews_RDBES/multiannual_case_studies_overview_RDBES_template.Rmd")
rmarkdown::render(
  rmdReport,
  params = params,
  output_file = paste0('results/MultiannualCaseStudiesOverview_', params$year ,'_', params$region, '.html'), # reports saved into results folder
  envir = new.env(parent = globalenv()),
  encoding = 'UTF-8'
)

