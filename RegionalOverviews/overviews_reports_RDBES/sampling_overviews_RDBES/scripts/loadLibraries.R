###################################################################
# loadLibraries
###################################################################
#
# This script loads the libraries needed for the generation of the 
# annual sampling RDBES overview. 
# 
###################################################################
# Authors: 
# - Kasia Krakówka [first draft]
# 
# 
# Dev. notes: 
#
# - 12.02.2025 first draft based on loadLibraries from fisheries_overviews_RDBES
###################################################################

# Print start message
cat("[1]    Loading libraries")

## Load libraries
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(crayon))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(tidytext))

# Print end message
if(is_empty(warnings())){
  cat("\n")
  cat(green('       \u2713'), paste0(" - Completed")) 
  cat("\n")
  cat("\n")
} 
