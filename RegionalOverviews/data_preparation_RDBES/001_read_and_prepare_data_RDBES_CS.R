################################################################################################################################################################
################################################################################################################################################################
# PREPARATION OF THE 'CS' DATA TO USE IN THE OVERVIEWS
################################################################################################################################################################
################################################################################################################################################################
#
# Authors:
# - Kasia Krakówka (first draft)
# - 
#
#
# dev notes:
# 12.02.2025 first draft based on 001_read_and_prepare_data_RDBES_CL_CE.R
#
###############################################################################################################################################################

rm(list=ls())

setwd("D:/RCG-RDBES-Overviews")#("Path to RCGs local repo")

library(data.table)
library(RDBEScore)
library(dplyr)
gc()
getwd()


################################################################################################################################################################
################################################################################################################################################################
#
#                                 SET PREP OPTIONS
#
################################################################################################################################################################
################################################################################################################################################################

## ========================
## Set params
## ======================== 

year_start <- 2023
year_end <- 2023
time_tag<-format(Sys.time(), "%Y%m%d")
target_region <- 'RCG_BA'
## =========================== 
## Create directory structure
## =========================== 

dir_output_rcg<-paste("D:/RCG-RDBES-Overviews/RegionalOverviews/data_RDBES/002_prepared/", time_tag ,"/",target_region, sep="")

if (!dir.exists(dir_output_rcg)){
  dir.create(dir_output_rcg,recursive=TRUE, showWarnings=FALSE)
}
## ========================
## Downloads data from sharepoint
## ======================== 
## Here we obtain raw RDBES data. 
#  The preferable choice is to use a function downloading the data from the SharePoint. Alternatively, data are to be manually downloaded. 
#source("RegionalOverviews/funs_RDBES/func_download_data_from_sharepoint.r")
# download_data_from_sharepoint(
# sharepoint_address = "https://community.ices.dk/ExpertGroups/DataExpports/RCG/_layouts/15/start.aspx#/RCG%20Data/Forms/AllItems.aspx?View=%7BFC9DF179%2DB628%2D47C5%2DB2A4%2D1D945AB1BBE4%7D",#"Path to directory on SharePoint",
# filename_vector = paste0(target_region, ".zip"), 
# dir_download_browser = "C:/Users/acfernandes/Downloads", # Directory where browser downloads, e.g. on eros machine
# dir_download_target = "RegionalOverviews/data_RDBES/001_raw",#"Path to directory where data should be stored",  
# unzip=TRUE
# )

# ========================
# reads in data
# ========================

# reads aux_countries dataset
aux_countries <- read.table("RegionalOverviews/data/aux_countries.txt", sep=",", header=T, colClasses="character", na.strings = "")
aux_species <- read.csv("RegionalOverviews/data/ASFIS_WoRMS.csv", sep=",", header=T, colClasses="character", na.strings = "")

# reads RDBES data
RDBESdataPath = 'RegionalOverviews/data_RDBES/001_raw'
setwd('RegionalOverviews/data_RDBES/001_raw')

# read data ### modify createDRBESDataObject from zip file!!! issue in RDBEScore
# to do add VD table
# H7,'H9' error BŁĄD: nie można przydzielić wektora o rozmiarze 231.6 Mb
#cs_object<-data.table()

for (H in c('H2','H3','H4','H6','H8','H10','H11','H12','H13')){ #))#'H1','H5',#change when zip bug will be fixed in RDBEScore
temp <- createRDBESDataObject(input=paste0('D:/RCG-RDBES-Overviews/RegionalOverviews/data_RDBES/001_raw/RDBES CS/',H))
#validateRDBESDataObject(cs_object, verbose = FALSE)
if(nrow(temp$DE) != 0){
temp <- createRDBESEstObject(temp)
if (H=='H2'){ # change
  cs <- temp
}else{
cs <- bind_rows(temp,cs)
#  cs_object<-combineRDBESDataObjects(temp,cs_object)
}
}
}

info<-paste0("Hierarchies: ", unique(cs$DEhierarchy))

#print(cs)
# QCA: duplicates (eliminates if existing)
dim(cs); cs<-unique(cs); dim(cs)

# filter out the proper years
cs<- cs[DEyear >= year_start & DEyear <= year_end]

################################################################################################################################################################
################################################################################################################################################################
#
#                                 COUNTRY SPECIFIC CORRECTIONS <-------------------------- TO BE DONE
#
################################################################################################################################################################
################################################################################################################################################################
# this should be fulfilled after generating the first versions of the overviews
# when some issues to be fixed are noticed 

# should this part be in this script or a separate one not to mess here every year? KK: in my opinion separate. Ana we need decision on this script and for CL/CE prep script

################################################################################################################################################################
################################################################################################################################################################
#
#    BASIC CHECKS
#
################################################################################################################################################################
################################################################################################################################################################
#
#add basic checks
#
################################################################################################################################################################
################################################################################################################################################################

# ======================
# Tweak on areas/region/ harbour/...
# ====================== 
# is it ISSG responsible for correction of the data?

# <----------------------------------------------------------------------------- to be done


################################################################################################################################################################
################################################################################################################################################################
#
#                                 FORMATS VARIABLES
#
################################################################################################################################################################
################################################################################################################################################################

# formats CS 
#<--------- add correct format of data
################################################################################################################################################################
################################################################################################################################################################
#
#                                 CREATE NEW VARIABLES
#
################################################################################################################################################################
################################################################################################################################################################

# ======================
# CS
# ======================

if("FOarea" %in% colnames(cs)){
  cs$Area[cs$DEhierarchy %in% c(1, 2, 3, 6, 10, 13)] <- cs[cs$DEhierarchy %in% c(1, 2, 3, 6, 10, 13), FOarea]
  cs$metier6[cs$DEhierarchy %in% c(1, 2, 3, 6, 10, 13)] <- cs[cs$DEhierarchy %in% c(1, 2, 3, 6, 10, 13), FOmetier6]
}
if("LEarea" %in% colnames(cs)){
  cs$Area[!cs$DEhierarchy %in% c(1, 2, 3, 6, 10, 13)] <- cs[!cs$DEhierarchy %in% c(1, 2, 3, 6, 10, 13), LEarea]
  cs$metier6[!cs$DEhierarchy %in% c(1, 2, 3, 6, 10, 13)] <- cs[!cs$DEhierarchy %in% c(1, 2, 3, 6, 10, 13), LEmetier6]
}

################################################################################################################################################################
################################################################################################################################################################
#
#          			             ASSIGN RCG
#
################################################################################################################################################################
################################################################################################################################################################

# ========================
# subsets data and RCG specific preparations
# ========================	

# RCG Baltic: Baltic Sea (ICES areas III b-d)
if(target_region=="RCG_BA")
{
  print(paste(".subsetting", target_region))

  cs_rcg <- cs[(grepl('27.3.b', Area) |
                  grepl('27.3.c', Area) | grepl('27.3.d', Area))]
}

# RCG NS&EA: the  North  Sea  (ICES  areas  IIIa,  IV  and  VIId),  the  Eastern  Arctic  (ICES  areas  I  and  II),  the  ICES  divisions Va, XII & XIV and the NAFO areas.
if(target_region=="RCG_NSEA") 
{
  print(paste(".subsetting",target_region))
  
  cs_rcg <- cs[ ( Area %in% c("27.1.a","27.1.b") |
                    grepl('27.2',Area) | 
                    grepl('27.3.a',Area) | 
                    grepl('27.4',Area) | 
                    grepl('27.5.a',Area) |
                    grepl('27.7.d',Area) | 
                    grepl('27.12',Area) | 
                    grepl('27.14',Area) | 
                    grepl('21.',Area) 
  )]
}


# RCG NA: the North Atlantic (ICES areas V-X, excluding Va and VIId)
if(target_region=="RCG_NA") 
{
  print(paste(".subsetting",target_region))
  
  cs_rcg <- cs[ ( grepl('27.5',Area) | 
                    grepl('27.6',Area) | 
                    grepl('27.7',Area) | 
                    grepl('27.8',Area) | 
                    grepl('27.9',Area) |
                    grepl('27.10',Area) 
  ) & 
    !grepl('27.5.a', Area) & 
    !grepl('27.7.d', Area)]
  
}

################################################################################################################################################################
################################################################################################################################################################
#
#  AREA MAP - variable to be used when producing the maps
#
################################################################################################################################################################
################################################################################################################################################################

cs_rcg[,AreaMap:=Area,]

if(target_region=="RCG_BA") 
{		
  cs_rcg[AreaMap %in% c("27.3.d.28.1", "27.3.d.28.2"), AreaMap := "27.3.d.28"]
}
if(target_region=="RCG_NSEA") 
{		
  cs_rcg[AreaMap %in% c("21.1"), AreaMap := "NA"] # div required (minority of records)					
  
  cs_rcg[AreaMap %in% c("21.3"), AreaMap := "NA"] # div required (minority of records)				
  
  cs_rcg[AreaMap %in% c("27.2"), AreaMap := "NA"]	 # div required	(minority of records)				
  
  cs_rcg[AreaMap %in% c("27.3.a"), AreaMap := "NA"] # subdiv required	(minority of records)		
  
  cs_rcg[AreaMap %in% c("27.4"), AreaMap := "NA"]	 # div required	(minority of records)	
  
  cs_rcg[AreaMap %in% c("27.14"), AreaMap := "NA"] # div required	(some records)		
  
  cs_rcg[AreaMap %in% c("27.2.a.1", "27.2.a.2"), AreaMap := "27.2.a"]
  
  cs_rcg[AreaMap %in% c("27.2.b.2"), AreaMap := "27.2.b"]			
  
  cs_rcg[AreaMap %in% c("27.14.b.1", "27.14.b.2"), AreaMap := "27.14.b"]
}

if(target_region=="RCG_NA") 
{		
  cs_rcg[AreaMap %in% c("27.5.b.1","27.5.b.1.a","27.5.b.1.b","27.5.b.2"), AreaMap := "27.5.b"]
  
  cs_rcg[AreaMap %in% c("27.9.b.1", "27.9.b.2"), AreaMap := "27.9.b"]
  
  cs_rcg[AreaMap %in% c("27.10.a.1","27.10.a.2"), AreaMap := "27.10.a"]		
  
  cs_rcg[AreaMap %in% c("27.8.e.1"), AreaMap := "27.8.e"]
  
  cs_rcg[AreaMap %in% c("27.8.d.2"), AreaMap := "27.8.d"]
  
  cs_rcg[AreaMap %in% c("27.7.c.1","27.7.c.2"), AreaMap := "27.7.c"]
  
  cs_rcg[AreaMap %in% c("27.6.b.1","27.6.b.2"), AreaMap := "27.6.b"]
  
  cs_rcg[AreaMap %in% c("27.7.j.2"), AreaMap := "27.7.j"]
  
  cs_rcg[AreaMap %in% c("27.7.k.1", "27.7.k.2"), AreaMap := "27.7.k"]
  
  cs_rcg[AreaMap %in% c("27.10"), AreaMap := "NA"]	# div required	(minority of records)	
  
  cs_rcg[AreaMap %in% c("27.6"), AreaMap := "NA"]		# div required	(minority of records)				
  
  cs_rcg[AreaMap %in% c("27.7"), AreaMap := "NA"]		# div required	(minority of records)
  
}			

################################################################################################################################################################
################################################################################################################################################################
#
#             SPECIES SCIENTIFIC NAME
#
################################################################################################################################################################
################################################################################################################################################################

cs_rcg[,SpeciesLaName:=aux_species$ScientificName[match(cs_rcg$SAspeCode, aux_species$AphiaID_accepted)]]

# QCA: should yield TRUE otherwise debug
nrow(cs_rcg[is.na(SpeciesLaName),]) == 0
dim(cs_rcg[is.na(SpeciesLaName),])

## Evaluate the need to put all "Trachurus" as "Trachurus spp", except "Trachurus trachurus"


################################################################################################################################################################
################################################################################################################################################################
#
#             CATCH GROUP
#
################################################################################################################################################################
################################################################################################################################################################

cs_rcg[,CatchGroup:=aux_species$CatchGroup[match(cs_rcg$SAspeCode, aux_species$AphiaID_accepted)]]

# QCA: should yield TRUE otherwise debug
nrow(cs_rcg[is.na(CatchGroup),]) == 0
unique(cs_rcg[is.na(CatchGroup),]$SpeciesLaName)


# check demersal
# check flatfish
# check small pelagic
# check large pelagic
# check molluscs
# check crustaceans
# check	elasmobranchs
# check	diadromous
# check	incidental by-catch
# check	other

################################################################################################################################################################
################################################################################################################################################################
#
#                                 FACTORIZATION <------------------ [establishes the order in unsorted bar graphs]
#
################################################################################################################################################################
################################################################################################################################################################

cs_rcg[,SDctry:=factor(SDctry, levels=sort(unique(SDctry))),]
cs_rcg[,metier6:=factor(metier6, levels=sort(unique(metier6))),]
cs_rcg[,SpeciesLaName:=factor(SpeciesLaName, levels=sort(unique(SpeciesLaName))),]
cs_rcg[,CatchGroup:=factor(CatchGroup, levels=sort(unique(CatchGroup))),]	

#cs_rcg[,Harbour:=factor(Harbour, levels=sort(unique(Harbour))),]

#cs_rcg[,VDlenCat:=factor(VDlenCat, levels=c())]


################################################################################################################################################################
################################################################################################################################################################
#
#                                 STOCK <---------------------------------------------- to be done
#
################################################################################################################################################################
################################################################################################################################################################


################################################################################################################################################################
################################################################################################################################################################
#
#                                 SAVE DATA
#
################################################################################################################################################################
################################################################################################################################################################

cs_rcg<-as.data.table(cs_rcg)
save(cs_rcg, file = paste(dir_output_rcg, paste("/RDBES",target_region,"CS", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))

