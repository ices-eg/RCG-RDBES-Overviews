################################################################################################################################################################
################################################################################################################################################################
# PREPARATION OF THE 'CL' and 'CE' DATA TO USE IN THE OVERVIEWS
################################################################################################################################################################
################################################################################################################################################################
#
# Authors:
# - Marta Suska 
# - Kasia Krakówka
# - Ana Cláudia Fernandes
#
# The code was based on '001_read_and_prepare_data_rdb_2009_2021_CL_CE.r' 
#
# dev notes:
# 11.04.2024 catch group 
# 11.04.2024 species scientific name
# 22.04.2024 included the AreaMap in the data (ACFernandes)
# 06.05.2024 included some basic data checks
# 15.05.2024 included part for subseting SSF data
# 20.06.2024 LDF part KK
#
###############################################################################################################################################################

rm(list=ls())

#setwd("Path to RCGs local repo") 
#setwd("D:/RegionalOverviewsLDF") # KK 
library(data.table)
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

target_region <- "RCG_LDF"
year_start <- 2021
year_end <- 2025
time_tag<-format(Sys.time(), "%Y%m%d")

## =========================== 
## Create directory structure
## =========================== 

dir_output_rcg<-paste("RegionalOverviews/data_RDBES/002_prepared/", time_tag ,"/",target_region, sep="")

if (!dir.exists(dir_output_rcg)){
  dir.create(dir_output_rcg,recursive=TRUE, showWarnings=FALSE)
  message("Dir created")
}

## ========================
## Downloads data from sharepoint
## ======================== 
## Here we obtain raw RDBES data. 
#  The preferable choice is to use a function downloading the data from the SharePoint. Alternatively, data are to be manually downloaded. 
# source("RegionalOverviews/funs_RDBES/func_download_data_from_sharepoint.r") contact with Henrik - structure of file name
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
aux_countries<-read.table("RegionalOverviews/data/aux_countries.txt", sep=",", header=T, colClasses="character", na.strings = "")
aux_species <- read.csv("RegionalOverviews/data/ASFIS_WoRMS.csv", sep=",", header=T, colClasses="character", na.strings = "")

# reads RDBES data
RDBESdataPath = 'RegionalOverviews/data_RDBES/001_raw/RCG_LDF'

file_cl <- paste(RDBESdataPath, "/RDBES CL/CommercialLanding.csv" , sep = '') 
file_ce <- paste(RDBESdataPath, "/RDBES CE/CommercialEffort.csv" , sep = '') 

# read data
cl <- data.table::fread(file_cl, stringsAsFactors=FALSE, verbose=FALSE, sep=",", na.strings="NULL",quote = "")
ce <- data.table::fread(file_ce, stringsAsFactors=FALSE, verbose=FALSE, sep=",", na.strings="NULL",quote = "")

# QCA: duplicates (eliminates if existing)
dim(cl); cl<-unique(cl); dim(cl)
dim(ce); ce<-unique(ce); dim(ce)

# Summary of the data extracted from RDBES
cl_total <- dcast(cl, CLvesselFlagCountry~CLyear, fun.aggregate = sum, value.var = "CLscientificWeight")
cl_total

ce_total <-dcast(ce, CEvesselFlagCountry~CEyear, fun.aggregate = sum, value.var = "CEscientificDaysAtSea")
ce_total

# filter out the proper years
cl<- cl[CLyear >= year_start & CLyear <= year_end]
ce<- ce[CEyear >= year_start & CEyear <= year_end]

# when jurisdictionArea is not specified it is an empty string instead of NA. Need to convert.
cl[CLjurisdictionArea=="", CLjurisdictionArea:=NA];.Last.updated;
ce[CEjurisdictionArea=="", CEjurisdictionArea:=NA];.Last.updated;
table(cl$CLjurisdictionArea, useNA = "always")
table(ce$CEjurisdictionArea, useNA = "always")

################################################################################################################################################################
################################################################################################################################################################
#
#                                 COUNTRY SPECIFIC CORRECTIONS
#
################################################################################################################################################################
################################################################################################################################################################

# this should be fulfilled after generating the first versions of the overviews
# when some issues to be fixed are noticed 

# corrections rcg ldf 2025
cl[CLvesselFlagCountry=="ES" & CLarea=="34.1.2",CLjurisdictionArea:="Canaries"];.Last.updated
ce[CEvesselFlagCountry=="ES" & CEarea=="34.1.2",CEjurisdictionArea:="Canaries"];.Last.updated

cl[CLvesselFlagCountry=="LT" & substr(CLarea,1,2) =="34" ,CLjurisdictionArea:="Mauritania"];.Last.updated
ce[CEvesselFlagCountry=="LT" & substr(CEarea,1,2) =="34" ,CEjurisdictionArea:="Mauritania"];.Last.updated

# cl[CLvesselFlagCountry=="LV" & substr(CLarea,1,2) =="34" ,CLjurisdictionArea:="Mauritania"];.Last.updated
# ce[CEvesselFlagCountry=="LV" & substr(CEarea,1,2) =="34" ,CEjurisdictionArea:="Mauritania"];.Last.updated

# should this part be in this script or a separate one not to mess here every year? KK: in my opinion separate

################################################################################################################################################################
################################################################################################################################################################
#
#    BASIC CHECKS <---------------------------------- 

## Check totals for landings and effort data (if more than one year in the data) - may pick some strange differences between data reported by year; missing data for countries
#
################################################################################################################################################################
################################################################################################################################################################

cl_total <- dcast(cl, CLvesselFlagCountry~CLyear, fun.aggregate = sum, value.var = "CLscientificWeight")
cl_total

ce_total <-dcast(ce, CEvesselFlagCountry~CEyear, fun.aggregate = sum, value.var = "CEscientificDaysAtSea")
ce_total
  
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

# formats CL 
cl[,CLlandingLocation:=iconv(CLlandingLocation, from="UTF-8", to="")]
cl[,CLlandingLocation:=toupper(CLlandingLocation)]
cl[,CLscientificWeight:=as.numeric(CLscientificWeight)]

# formats CE 
ce[,CElandingLocation:=iconv(CElandingLocation, from="UTF-8", to="")]
ce[,CElandingLocation:=toupper(CElandingLocation)]


################################################################################################################################################################
################################################################################################################################################################
#
#                                 CREATE NEW VARIABLES
#
################################################################################################################################################################
################################################################################################################################################################

# ======================
# CL
# ======================

# OfficialLandingCatchWeight_1000ton
cl[,CLscientificWeight_ton := CLscientificWeight/1000]
cl[,CLscientificWeight_1000ton := CLscientificWeight/1000000]

# fleet segment (FlagCountry_Loa)
cl[,FlagCountry_Loa:=paste(CLvesselFlagCountry, CLvesselLengthCategory, sep="_")]

# HarbourCountry (ISO3) and HarbourCountry2 (ISO2) - there are harbour codes ('CLlandingLocation') that don't match the 'CLandingCountry'
## NOTE: Use harbour code for the landings abroad analysis
cl[,HarbourCountry2:=substring(CLlandingLocation,1,2)]
cl[,HarbourCountry:=aux_countries$ISO3Code[match(HarbourCountry2, aux_countries$ISO2Code)]]
cl[HarbourCountry2 == "NZ", HarbourCountry := "NZL"]
cl[HarbourCountry2 == "RE", HarbourCountry := "REU"]
cl[HarbourCountry2 == "YT", HarbourCountry := "MYT"]
cl[HarbourCountry2 == "MQ", HarbourCountry := "MTQ"]
cl[is.na(HarbourCountry), unique(CLlandingLocation)]

# ======================
# CE 
# ======================

# KWDays_thousands
ce[,CEscientifickWDaysAtSea_1000x := CEscientifickWDaysAtSea/1000]				
# GTDays_thousands
ce[,CEgTDaysAtSea_1000x := CEgTDaysAtSea/1000]

# fleet segment (FlagCountry_Loa)	
ce[,FlagCountry_Loa:=paste(CEvesselFlagCountry, CEvesselLengthCategory, sep="_")]

# HarbourCountry (ISO3) and HarbourCountry2 (ISO2)			
ce[,HarbourCountry2:=substring(CElandingLocation,1,2)]
ce[,HarbourCountry:=aux_countries$ISO3Code[match(HarbourCountry2, aux_countries$ISO2Code)]]
ce[HarbourCountry2 == "SC", HarbourCountry := "SYC"]
ce[HarbourCountry2 == "RE", HarbourCountry := "REU"]
ce[HarbourCountry2 == "YT", HarbourCountry := "MYT"]
ce[HarbourCountry2 == "MQ", HarbourCountry := "MTQ"]
ce[is.na(HarbourCountry), unique(CElandingLocation)]

# QCA: should yield TRUE otherwise debug on ce - checks for missing codes in the "aux_countries.txt"
nrow(ce[is.na(HarbourCountry) & !is.na(HarbourCountry2),]) == 0


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

if(target_region=="RCG_LDF") {
  print(paste(".subsetting",target_region))
  
  cl_rcg <- cl[ !grepl('27.',CLarea)]
  
  ce_rcg <- ce[ !grepl('27.',CEarea)]
}

################################################################################################################################################################
################################################################################################################################################################
#
#  AREA MAP - variable to be used when producing the maps
#
################################################################################################################################################################
################################################################################################################################################################

	cl_rcg[,AreaMap:=CLarea,]
	ce_rcg[,AreaMap:=CEarea,]
		
	if(target_region=="RCG_LDF"){ #Division is the same as AreaMap ask Maksims??? In previes format is new variable DIVISION in my opinion is the same as AreaMap
	 # <- check it MA, MK, is everything approprietly assigned?
	  cl_rcg[AreaMap %in% c("34.1.1.1", "34.1.1.2", "34.1.1.3",  "34.1.1"), AreaMap := "34.1.1"]
	  ce_rcg[AreaMap %in% c("34.1.1.1", "34.1.1.2", "34.1.1.3",  "34.1.1"), AreaMap := "34.1.1"]
	  
	  cl_rcg[AreaMap %in% c("34.1.3.1", "34.1.3.2", "34.1.3"), AreaMap := "34.1.3"]
	  ce_rcg[AreaMap %in% c("34.1.3.1", "34.1.3.2", "34.1.3"), AreaMap := "34.1.3"]
	  
	  cl_rcg[AreaMap %in% c("34.2") & is.na(CLjurisdictionArea), AreaMap := "34.2.0"]
	  ce_rcg[AreaMap %in% c("34.2") & is.na(CEjurisdictionArea), AreaMap := "34.2.0"] 
	  
	  cl_rcg[AreaMap %in% c("34.3.1.1", "34.3.1.2", "34.3.1.3", "34.3.1"), AreaMap := "34.3.1"]
	  ce_rcg[AreaMap %in% c("34.3.1.1", "34.3.1.2", "34.3.1.3", "34.3.1"), AreaMap := "34.3.1"]
	  
	  cl_rcg[AreaMap %in% c("34") & CLjurisdictionArea == 'Morocco', AreaMap := "34.1.1"] # inconsistency, either 34.1.1 or 34.1.3. As 34.1.1 apeears much more often I assigned this one
	  ce_rcg[AreaMap %in% c("34") & CEjurisdictionArea == 'Morocco', AreaMap := "34.1.1"] # inconsistency, either 34.1.1 or 34.1.3. As 34.1.1 apeears much more often I assigned this one
	  
	  
	  cl_rcg[AreaMap %in% c("34") & CLjurisdictionArea == 'Guinea', AreaMap := "34.3.1"]
	  ce_rcg[AreaMap %in% c("34") & CEjurisdictionArea == 'Guinea', AreaMap := "34.3.1"]
	  
	  cl_rcg[AreaMap %in% c("34") & CLjurisdictionArea == 'Canaries', AreaMap := "34.1.2"]
	  ce_rcg[AreaMap %in% c("34") & CEjurisdictionArea == 'Canaries', AreaMap := "34.1.2"]
	  
	  cl_rcg[AreaMap %in% c("41", "87","31","51"), AreaMap := NA] # too low
	  ce_rcg[AreaMap %in% c("41", "87","31","51"), AreaMap := NA] # too low
	  
	  cl_rcg[AreaMap %in% c("34") & is.na(CLjurisdictionArea), AreaMap := NA] # ask Sieto
	  ce_rcg[AreaMap %in% c("34") & is.na(CEjurisdictionArea), AreaMap := NA] # ask Sieto
	  
	  cl_rcg[AreaMap %in% c("87.1.4"), AreaMap := "87.1"]
	  ce_rcg[AreaMap %in% c("87.1.4"), AreaMap := "87.1"]
	  
	  cl_rcg[AreaMap %in% c("87.2.6"), AreaMap := "87.2"]
	  ce_rcg[AreaMap %in% c("87.2.6"), AreaMap := "87.2"]
	  
	  cl_rcg[AreaMap %in% c("87.3.3"), AreaMap := "87.3"]
	  ce_rcg[AreaMap %in% c("87.3.3"), AreaMap := "87.3"]
	  
	  ce_rcg[AreaMap %in% c("41.1.4"), AreaMap := "41.1"]
	  ce_rcg[AreaMap %in% c("41.2.1","41.2.2","41.2.3","41.2.4"), AreaMap := "41.2"]
	  ce_rcg[AreaMap %in% c("41.3.1","41.3.3"), AreaMap := "41.3"]
	  ce_rcg[AreaMap %in% c("47.A.0","47.A.1"), AreaMap := "47.A"]
	  ce_rcg[AreaMap %in% c("47.B.0","47.B.1"), AreaMap := "47.B"]
	  ce_rcg[AreaMap %in% c("47.C.0","47.C.1"), AreaMap := "47.C"]
	  
	  print(cl_rcg |> dplyr::distinct(CLarea, AreaMap))
	  
	  print(ce_rcg |> dplyr::distinct(CEarea, AreaMap))
	  
	  # Area level
	  cl_rcg[,AreaOrg := CLarea]
	  ce_rcg[,AreaOrg := CEarea]
	  
	  cl_rcg[,Area:=stringr::str_sub(AreaOrg, end = 2),]
	  ce_rcg[,Area:=stringr::str_sub(AreaOrg, end = 2),]
	}
	# QCA: visual
		cl_rcg[, list(N=.N,ton1000 = round(sum(CLscientificWeight_1000ton),1)),list(AreaMap,CLarea)][order(AreaMap)]
		cl_rcg[, list(N=.N,ton1000 = round(sum(CLscientificWeight_1000ton),1)),list(AreaMap,CLarea, CLvesselFlagCountry, CLyear)][order(AreaMap)][AreaMap=="NA" | is.na(AreaMap),]
		# <- check it MA, MK, FR reported area as 31, should it be fixed?
		ce_rcg[, list(N=.N,TripsNumber = sum(CEnumberOfFractionalTrips)),list(AreaMap,CEarea)][order(AreaMap)]
		ce_rcg[, list(N=.N,TripsNumber = sum(CEnumberOfFractionalTrips)),list(AreaMap,CEarea, CEvesselFlagCountry, CEyear)][order(AreaMap)][AreaMap=="NA" | is.na(AreaMap),]
	
		## Area
		FracTrips_area <- ce_rcg[, list(N=.N,FracTripsNumber = sum(CEnumberOfFractionalTrips)),list(AreaMap,CEarea)][order(AreaMap)]
		DomTrips_area <- ce_rcg[, list(N=.N,DomTripsNumber = sum(CEnumberOfDominantTrips)),list(AreaMap,CEarea)][order(AreaMap)]
		
		## Checks over/under estimation of effort using fractional trips
		DiffFracDomTrips_area <- cbind(FracTrips_area, DomTripsNumber = DomTrips_area$DomTripsNumber)
		DiffFracDomTrips_area$Diff <- DiffFracDomTrips_area$FracTripsNumber/DiffFracDomTrips_area$DomTripsNumber

		## Flag country
		FracTrips_ctry <- ce[, list(N=.N,FracTripsNumber = sum(CEnumberOfFractionalTrips)),list(CEvesselFlagCountry)][order(CEvesselFlagCountry)]
		DomTrips_ctry <- ce[, list(N=.N,DomTripsNumber = sum(CEnumberOfDominantTrips)),list(CEvesselFlagCountry)][order(CEvesselFlagCountry)]
		
		DiffFracDomTrips_ctry <- cbind(FracTrips_ctry, DomTripsNumber = DomTrips_ctry$DomTripsNumber)
		DiffFracDomTrips_ctry$Diff <- DiffFracDomTrips_ctry$FracTripsNumber/DiffFracDomTrips_ctry$DomTripsNumber
		DiffFracDomTrips_ctry #	<- check it MA, MK, fracTrips is much bigger then domTrips
		# CEvesselFlagCountry     N FracTripsNumber DomTripsNumber      Diff
		# <char> <int>           <num>          <int>     <num>
		#   1:                  DE    57           27.82             56 0.4967857
		# 
################################################################################################################################################################
################################################################################################################################################################
#
#             SPECIES SCIENTIFIC NAME
#
################################################################################################################################################################
################################################################################################################################################################

cl_rcg[,SpeciesLaName:=aux_species$ScientificName[match(cl_rcg$CLspeciesCode, aux_species$AphiaID_accepted)]]

# QCA: should yield TRUE otherwise debug
cl_rcg[CLspeciesCode=="293578", SpeciesLaName:="Diplodus argenteus"]
cl_rcg[CLspeciesCode=="987079", SpeciesLaName:="Maguimithrax spinosissimus"]
cl_rcg[CLspeciesCode=="401693", SpeciesLaName:="Paracaesio xanthurus"]
# IZO, 259252? How to fix? <- check it MA, MK, looks like (IZO Istiophorus platypterus 217712) doesn't match to (259252 Bathynomus giganteus BIG)
cl_rcg[CLspeciesCode=="259252", SpeciesLaName:="Bathynomus giganteus"];.Last.updated
nrow(cl_rcg[is.na(SpeciesLaName),]) == 0
dim(cl_rcg[is.na(SpeciesLaName),])
cl_rcg[is.na(SpeciesLaName),unique(CLspeciesCode)]
## Evaluate the need to put all "Trachurus" as "Trachurus spp", except "Trachurus trachurus"

################################
################################
#            FAO3ALPHA
################################
################################

cl_rcg[,Species3ALPHA:=aux_species$X3A_CODE[match(cl_rcg$CLspeciesCode, aux_species$AphiaID_accepted)]]
cl_rcg[CLspeciesCode=="293578", Species3ALPHA:="DIG"];.Last.updated
cl_rcg[CLspeciesCode=="889885", Species3ALPHA:="AUU"];.Last.updated
cl_rcg[CLspeciesCode=="987079", Species3ALPHA:="MXI"];.Last.updated
cl_rcg[CLspeciesCode=="401693", Species3ALPHA:="LRX"];.Last.updated
cl_rcg[CLspeciesCode=="368408", Species3ALPHA:="SKH"];.Last.updated #  is it ok?
cl_rcg[CLspeciesCode=="259252", Species3ALPHA:="IZO"];.Last.updated
nrow(cl_rcg[is.na(Species3ALPHA),]) == 0
dim(cl_rcg[is.na(SpeciesLaName),])
cl_rcg[is.na(Species3ALPHA),unique(CLspeciesCode)]
# IZO, 259252? How to fix?  <- check it MA, MK


################################################################################################################################################################
################################################################################################################################################################
#
#             CATCH GROUP
#
################################################################################################################################################################
################################################################################################################################################################

cl_rcg[,CatchGroup:=aux_species$CatchGroup[match(cl_rcg$CLspeciesCode, aux_species$AphiaID_accepted)]]

# QCA: should yield TRUE otherwise debug
unique(cl_rcg[is.na(CatchGroup),]$SpeciesLaName)
cl_rcg[SpeciesLaName=="Diplodus argenteus",CatchGroup:="demersal"]
cl_rcg[SpeciesLaName=="Maguimithrax spinosissimus",CatchGroup:="crustaceans"]
cl_rcg[SpeciesLaName=="Paracaesio xanthurus",CatchGroup:="other"]
cl_rcg[SpeciesLaName=="Bathynomus giganteus",CatchGroup:="other"]
nrow(cl_rcg[is.na(CatchGroup),]) == 0
unique(cl_rcg[is.na(CatchGroup),]$CLspeciesCode)

# give it a check (see if it makes sense)
			 # check demersal
				head(cl_rcg[CatchGroup == "demersal",list(Kg=sum(CLscientificWeight), KgLastYear=sum(CLscientificWeight[CLyear==max(CLyear)])),list(SpeciesLaName)] [order(-Kg),],20)
			# check flatfish
				head(cl_rcg[CatchGroup == "flatfish",list(Kg=sum(CLscientificWeight), KgLastYear=sum(CLscientificWeight[CLyear==max(CLyear)])),list(SpeciesLaName)] [order(-Kg),],20)
			# check small pelagic
				head(cl_rcg[CatchGroup == "small pelagic",list(Kg=sum(CLscientificWeight), KgLastYear=sum(CLscientificWeight[CLyear==max(CLyear)])),list(SpeciesLaName)] [order(-Kg),],20)
			# check large pelagic
				head(cl_rcg[CatchGroup == "large pelagic",list(Kg=sum(CLscientificWeight), KgLastYear=sum(CLscientificWeight[CLyear==max(CLyear)])),list(SpeciesLaName)] [order(-Kg),],20)
			# check molluscs
				head(cl_rcg[CatchGroup == "molluscs",list(Kg=sum(CLscientificWeight), KgLastYear=sum(CLscientificWeight[CLyear==max(CLyear)])),list(SpeciesLaName)] [order(-Kg),],20)
			# check crustaceans
				head(cl_rcg[CatchGroup == "crustaceans",list(Kg=sum(CLscientificWeight), KgLastYear=sum(CLscientificWeight[CLyear==max(CLyear)])),list(SpeciesLaName)] [order(-Kg),],20)
			# check	elasmobranchs
				head(cl_rcg[CatchGroup == "elasmobranchs",list(Kg=sum(CLscientificWeight), KgLastYear=sum(CLscientificWeight[CLyear==max(CLyear)])),list(SpeciesLaName)] [order(-Kg),],20)
			# check	diadromous
				head(cl_rcg[CatchGroup == "diadromous",list(Kg=sum(CLscientificWeight), KgLastYear=sum(CLscientificWeight[CLyear==max(CLyear)])),list(SpeciesLaName)] [order(-Kg),],20)
			# check	incidental by-catch
				head(cl_rcg[CatchGroup == "incidental by-catch",list(Kg=sum(CLscientificWeight), KgLastYear=sum(CLscientificWeight[CLyear==max(CLyear)])),list(SpeciesLaName)] [order(-Kg),],20)			
			# check	other
				head(cl_rcg[CatchGroup == "other",list(Kg=sum(CLscientificWeight), KgLastYear=sum(CLscientificWeight[CLyear==max(CLyear)])),list(SpeciesLaName)] [order(-Kg),],20)

################################################################################################################################################################
################################################################################################################################################################
#
#                                 FACTORIZATION <------------------ [establishes the order in unsorted bar graphs]
#
################################################################################################################################################################
################################################################################################################################################################

cl_rcg[,CLvesselFlagCountry:=factor(CLvesselFlagCountry, levels=sort(unique(CLvesselFlagCountry))),]
cl_rcg[,CLlandingCountry:=factor(CLlandingCountry, levels=sort(unique(CLlandingCountry))),]
cl_rcg[,CLfishingTechnique:=factor(CLfishingTechnique, levels=sort(unique(CLfishingTechnique))),]
cl_rcg[,CLmetier6:=factor(CLmetier6, levels=sort(unique(CLmetier6))),]
cl_rcg[,CLlandingLocation:=factor(CLlandingLocation, levels=sort(unique(CLlandingLocation))),]
cl_rcg[,SpeciesLaName:=factor(SpeciesLaName, levels=sort(unique(SpeciesLaName))),]
cl_rcg[,CLvesselLengthCategory:=factor(CLvesselLengthCategory, levels=c("NK", "VL0006", "VL0608", "VL0810", "VL1012","VL1215", "VL1518", "VL1824", "VL2440", "VL40XX"))]
cl_rcg[,CatchGroup:=factor(CatchGroup, levels=sort(unique(CatchGroup))),]	

ce_rcg[,CEvesselFlagCountry:=factor(CEvesselFlagCountry, levels=sort(unique(CEvesselFlagCountry))),]
ce_rcg[,CEfishingTechnique:=factor(CEfishingTechnique, levels=sort(unique(CEfishingTechnique))),]
ce_rcg[,CEmetier6:=factor(CEmetier6, levels=sort(unique(CEmetier6))),]
ce_rcg[,CElandingLocation:=factor(CElandingLocation, levels=sort(unique(CElandingLocation))),]
ce_rcg[,CEvesselLengthCategory:=factor(CEvesselLengthCategory, levels=c("NK", "VL0006", "VL0608", "VL0810", "VL1012","VL1215", "VL1518", "VL1824", "VL2440", "VL40XX"))]
ce_rcg[,CEscientificDaysAtSea:=as.numeric(CEscientificDaysAtSea)]
ce_rcg[,CEscientificFishingDays:=as.numeric(CEscientificFishingDays)]
ce_rcg[,CEscientificVesselFishingHour:=as.numeric(CEscientificVesselFishingHour)]
ce_rcg[,CEscientifickWFishingDays:=as.numeric(CEscientifickWFishingDays)]
ce_rcg[,CEgTDaysAtSea:=as.numeric(CEgTDaysAtSea)]
ce_rcg[,CEscientifickWDaysAtSea_1000x:=as.numeric(CEscientifickWDaysAtSea_1000x)]
ce_rcg[,CEgTDaysAtSea_1000x:=as.numeric(CEgTDaysAtSea_1000x)]

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

file_info_cl<-file.info(file_cl)
file_info_ce<-file.info(file_ce)


save(cl_rcg, file_info_cl, file = paste(dir_output_rcg, paste("/RDBES",target_region,"CL", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
save(ce_rcg, file_info_ce, file = paste(dir_output_rcg, paste("/RDBES",target_region,"CE", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
