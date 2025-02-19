###################################################################
# Load data 
###################################################################
#
# This script is used to load CS data in the prepared form.
#  
# These data originate from raw RDBES data treated with the script 
# "001_read_and_prepare_data_RDBES_CS". The preparation is either
# performed locally or data already prepared are downloaded prior 
# to the overview generation. 
#
###################################################################
# Authors: 
# - Kasia Krakówka [first draft]
# 
# 
# Dev. notes: 
#
# - 12.02.2025 first draft based on loadData from fisheries_overviews_RDBES
#
###################################################################

# Print start message 
  cat("[1]    Loading data")
  cat("\n")


# Empty warnings from previous code
#assign("last.warning", NULL, envir = baseenv()) # Credits: https://stackoverflow.com/questions/5725106/r-how-to-clear-all-warnings

## Load data 
# Load CS data
load(
  paste(params$data_dir, '/', params$CSfileName,'.Rdata', sep = "")
); cs = cs_rcg # shorter name 

# put some necessary data prep part below
######################
# FILTER the data out
######################
cs <- cs[DEyear %in% params$year]

# Print end message
if(is_empty(warnings())){
  cat("\n")
  cat(green('       \u2713'), paste0(" - Completed")) 
  cat("\n")
  cat("\n")
} 