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
# - Kasia Krakówka [first draft] 
# - Ana Cláudia Fernandes
# 
# Dev. notes: 
#- 12.02.2025 first draft based on parametersDefinition from fisheries_overviews_RDBES
#
###################################################################

# Print start message
cat("[3]    Parameters definition")

# Empty warnings from previous code
#assign("last.warning", NULL, envir = baseenv()) # Credits: https://stackoverflow.com/questions/5725106/r-how-to-clear-all-warnings

## Download data 
params <- list(
  year = yearSelected,
  region = regionSelected, 
  logo_path = ifelse(regionSelected=='BA',file.path("../../data/logo/logo RCG BALTIC.PNG"),file.path("../../data/logo/logo RCG NA NS_EA.PNG")),
  data_dir_CS = paste0(getwd(), '/RegionalOverviews/data_RDBES/002_prepared/', dataprepDate, '/RCG_', regionSelected),
  CSfileName = paste0('RDBES_RCG_', regionSelected, '_CS_',yearSelected,'_',yearSelected,'_prepared_', dataprepDate),
  
  data_dir_CL = paste0(getwd(), '/RegionalOverviews/data_RDBES/002_prepared/', dataprepDateCL, '/RCG_', regionSelected),
  CLfileName = paste0('RDBES_RCG_', regionSelected, '_CL_',yearSelected,'_',yearSelected,'_prepared_', dataprepDateCL),
  
  data_dir_CE = paste0(getwd(), '/RegionalOverviews/data_RDBES/002_prepared/', dataprepDateCE, '/RCG_', regionSelected),
  CEfileName = paste0('RDBES_RCG_', regionSelected, '_CE_',yearSelected,'_',yearSelected,'_prepared_', dataprepDateCE),
  
  RDBES_download_date = '01/01/2000'
)

# Print parameters. 
cat("\n")
cat("       The overview will be generated using the following parameters:")
cat("\n")
cat("\n")
writeLines(
  paste0(
    "             ", 
    capture.output(print(data.frame(
      do.call(rbind, params) %>% 
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
