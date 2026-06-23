###################################################################
# parametersDefinition 
###################################################################
#
# This script simply gather user selections (i.e. values 
# specified by the user in annual_overview_RDBES script) and uses 
# them to generate the list of parameters used ultimately to 
# generate the overviews. 
#
###################################################################
# Authors: 
# - Eros Quesada [first draft] 
# - Kasia Krakówka 
# 
# Dev. notes: 
# 
# - 20240207: Created based on annual_overview_RDBES.R
# - 20240220: Added fleet register directory
# - 20240404: The logo depends on the selected region
#
###################################################################

# Print start message
cat("[3]    Parameters definition")

# Empty warnings from previous code
assign("last.warning", NULL, envir = baseenv()) # Credits: https://stackoverflow.com/questions/5725106/r-how-to-clear-all-warnings

## Download data 
params <- list(
  allYears = yearsSelected,
  year = year,
  region = regionSelected, 
  logo_path = file.path("../../data/logo/logo RCG LDF.PNG"),
  data_dir = data_dir,
  table_dir = table_dir,
  CLfileName = CLfileName,
  CEfileName = CEfileName
)

paramsDisplay = params
paramsDisplay$allYears = paste(params$allYears, collapse = ', ') 
# Print parameters. 
cat("\n")
cat("       The overview will be generated using the following parameters:")
cat("\n")
cat("\n")
writeLines(
    paste0(
      "             ", 
      capture.output(print(data.frame(
        do.call(rbind, paramsDisplay) %>% 
          data.frame() %>% 
          tibble::rownames_to_column(var = "Parameter") %>% 
          dplyr::rename("Value" = 2)
        )
      )
    )
  )
)
cat("\n")

# Print end message
if(is_empty(warnings())){
  cat("\n")
  cat(green('       \u2713'), paste0(" - Completed")) 
  cat("\n")
  cat("\n")
} 
