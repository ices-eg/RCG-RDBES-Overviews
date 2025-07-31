###################################################################
# Load auxiliary data 
###################################################################
#
# This script is used to load auxiliary data
#  
#
###################################################################
# Authors: 
# - Kasia Krakówka [first draft]
# 
# Dev. notes: 
#
#
###################################################################

# Print start message 
cat("[1]    Loading auxiliary data")
cat("\n")


# Empty warnings from previous code
assign("last.warning", NULL, envir = baseenv()) # Credits: https://stackoverflow.com/questions/5725106/r-how-to-clear-all-warnings

## Load auxiliary data 
colours <- read.table("RegionalOverviews/data/colourCountryTab.txt", header = T) #RDBESvisualise

# Print end message
if(is_empty(warnings())){
  cat("\n")
  cat(green('       \u2713'), paste0(" - Completed")) 
  cat("\n")
  cat("\n")
} 
