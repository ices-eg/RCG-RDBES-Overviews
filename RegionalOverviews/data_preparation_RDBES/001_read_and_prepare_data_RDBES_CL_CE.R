# based on the 001_read_and_prepare_data_rdb_2009_2021_CL_CE.r
# script prepares datasets for further analysis

#dev notes
# 11.04.2024 catch group 
# 11.04.2024 species scientific name
# 22.04.2024 included the AreaMap in the data (ACFernandes)
# 06.05.2024 included some basic data checks
# 15.05.2024 included part for subseting SSF data


rm(list=ls())

setwd("C:/Users/acfernandes/Documents/2024/000_RCG_InterssessionalWork/ISSG_Overviews/RCG-RDBES-Overviews")#("Path to RCGs local repo")

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

restrict_to_SSF_data <- FALSE
target_region <- "RCG_BA" # RCG_BA, RCG_NA, RCG_NSEA
year_start <- 2023
year_end <- 2023
time_tag<-format(Sys.time(), "%Y%m%d")

## ====================== 
## Create directory structure
## ====================== 

dir_output_all<-paste("RegionalOverviews/data_RDBES/002_prepared/", time_tag, sep="")
dir_output_rcg<-paste("RegionalOverviews/data_RDBES/002_prepared/", time_tag ,"/",target_region, sep="")


if (!dir.exists(dir_output_all)){
  dir.create(dir_output_all,recursive=TRUE, showWarnings=FALSE)
}

if (!dir.exists(dir_output_rcg)){
  dir.create(dir_output_rcg,recursive=TRUE, showWarnings=FALSE)
}

## ========================
## Downloads data from sharepoint
## ======================== 
## Here we obtain raw RDBES data. 
#  The preferable choice is to use a function downloading the data from the SharePoint. Alternatively, data are to be manually downloaded. 
source("RegionalOverviews/funs_RDBES/func_download_data_from_sharepoint.r")
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
RDBESdataPath = 'RegionalOverviews/data_RDBES/001_raw'
setwd('RegionalOverviews/data_RDBES/001_raw')


file_cl <- paste(RDBESdataPath, "/RDBES CL/CommercialLanding.csv", sep = '')
file_ce <- paste(RDBESdataPath, "/RDBES CE/CommercialEffort.csv" , sep = '') 

# read data
#cl <- data.table::fread(file_cl, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=",", na.strings="NULL")
cl <- data.table::fread(file_cl)
#ce <- data.table::fread(file_ce, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=",", na.strings="NULL") 
ce <- data.table::fread(file_ce)

# QCA: duplicates (eliminates if existing)
dim(cl); cl<-unique(cl); dim(cl)
dim(ce); ce<-unique(ce); dim(ce)

# filter out the proper years
cl<- cl[CLyear >= year_start & CLyear <= year_end]
ce<- ce[CEyear >= year_start & CEyear <= year_end]

################################################################################################################################################################
################################################################################################################################################################
#
#                                 COUNTRY SPECIFIC CORRECTIONS <-------------------------- TO BE DONE
#
################################################################################################################################################################
################################################################################################################################################################
# this should be fulfilled after generating the first versions of the overviews
# when some issues to be fixed are noticed 

# should this part be in this script or a separate one not to mess here every year? KK: in my opinion separate


################################################################################################################################################################
################################################################################################################################################################
#
#    BASIC CHECKS <---------------------------------- 

## Check totals for landings and effort data (if more than one year in the data) - may pick some strange differences between data reported by year; missing data for countries

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


# QCA: should yield TRUE otherwise debug on ce - checks for missing codes in the "aux_countries.txt"
nrow(ce[is.na(HarbourCountry) & !is.na(HarbourCountry2),]) == 0


################################################################################################################################################################
################################################################################################################################################################
#
#                                 ASSIGN RCG
#
################################################################################################################################################################
################################################################################################################################################################

# ========================
# subsets data and RCG specific preparations
# ========================	

# RCM Baltic: Baltic Sea (ICES areas III b-d)
if(target_region=="RCG_BA") 
{
  print(paste(".subsetting",target_region))
  
  cl_rcg <- cl[ ( grepl('27.3.b',CLarea) | grepl('27.3.c',CLarea) | grepl('27.3.d',CLarea) )]

  ce_rcg <- ce[ ( grepl('27.3.b',CEarea) | grepl('27.3.c',CEarea) | grepl('27.3.d',CEarea) )]
}

# RCM NS&EA: the  North  Sea  (ICES  areas  IIIa,  IV  and  VIId),  the  Eastern  Arctic  (ICES  areas  I  and  II),  the  ICES  divisions Va, XII & XIV and the NAFO areas.
if(target_region=="RCG_NSEA") 
{
  print(paste(".subsetting",target_region))
  
  cl_rcg <- cl[ ( CLarea %in% c("27.1.a","27.1.b") | 		## Added '.' after 1 because it was picking area 27.10 which is North Atlantic
                    grepl('27.2',CLarea) | 
                    grepl('27.3.a',CLarea) | 
                    grepl('27.4',CLarea) | 
                    grepl('27.5.a',CLarea) |
                    grepl('27.7.d',CLarea) | 
                    grepl('27.12',CLarea) | 
                    grepl('27.14',CLarea) | 
                    grepl('21.',CLarea) 
  )]
  
  ce_rcg <- ce[ ( CEarea %in% c("27.1.a","27.1.b")  | 
                    grepl('27.2',CEarea) | 
                    grepl('27.3.a',CEarea) | 
                    grepl('27.4',CEarea) | 
                    grepl('27.5.a',CEarea) |
                    grepl('27.7.d',CEarea) | 
                    grepl('27.12',CEarea) | 
                    grepl('27.14',CEarea) | 
                    grepl('21.',CEarea)   
  )]
}


# RCM NA: the North Atlantic (ICES areas V-X, excluding Va and VIId)
if(target_region=="RCG_NA") 
{
  print(paste(".subsetting",target_region))
  
  cl_rcg <- cl[ ( grepl('27.5',CLarea) | 
                    grepl('27.6',CLarea) | 
                    grepl('27.7',CLarea) | 
                    grepl('27.8',CLarea) | 
                    grepl('27.9',CLarea) |
                    grepl('27.10',CLarea) 
  ) & 
    !grepl('27.5.a', CLarea) & 
    !grepl('27.7.d', CLarea)]
  
  ce_rcg <- ce[ ( grepl('27.5',CEarea) | 
                    grepl('27.6',CEarea) | 
                    grepl('27.7',CEarea) | 
                    grepl('27.8',CEarea) | 
                    grepl('27.9',CEarea) |
                    grepl('27.10',CEarea) 
  ) & 
    !grepl('27.5.a', CEarea) & 
    !grepl('27.7.d', CEarea)]
}

################################################################################
################################################################################
#
#  AREA MAP - variable to be used when producing the maps
#
################################################################################
################################################################################

	cl_rcg[,AreaMap:=CLarea,]
	ce_rcg[,AreaMap:=CEarea,]
		
		if(target_region=="RCG_BA") 
			{		
			cl_rcg[AreaMap %in% c("27.3.d.28.1", "27.3.d.28.2"), AreaMap := "27.3.d.28"]
			ce_rcg[AreaMap %in% c("27.3.d.28.1", "27.3.d.28.2"), AreaMap := "27.3.d.28"]
			}
		if(target_region=="RCG_NSEA") 
			{		
			cl_rcg[AreaMap %in% c("21.1"), AreaMap := "NA"] # div required (minority of records)					
			ce_rcg[AreaMap %in% c("21.1"), AreaMap := "NA"] # div required (minority of records)		
			
			cl_rcg[AreaMap %in% c("21.3"), AreaMap := "NA"] # div required (minority of records)				
			ce_rcg[AreaMap %in% c("21.3"), AreaMap := "NA"] # div required (minority of records)		
			
			cl_rcg[AreaMap %in% c("27.2"), AreaMap := "NA"]	 # div required	(minority of records)				
			ce_rcg[AreaMap %in% c("27.2"), AreaMap := "NA"]	 # div required	(minority of records)	
			
			cl_rcg[AreaMap %in% c("27.3.a"), AreaMap := "NA"] # subdiv required	(minority of records)		
			ce_rcg[AreaMap %in% c("27.3.a"), AreaMap := "NA"] # subdiv required	(minority of records)

			cl_rcg[AreaMap %in% c("27.4"), AreaMap := "NA"]	 # div required	(minority of records)	
			ce_rcg[AreaMap %in% c("27.4"), AreaMap := "NA"]	 # div required	(minority of records)	
			
			cl_rcg[AreaMap %in% c("27.14"), AreaMap := "NA"] # div required	(some records)				
			ce_rcg[AreaMap %in% c("27.14"), AreaMap := "NA"] # div required	(some records)		
			
			cl_rcg[AreaMap %in% c("27.2.a.1", "27.2.a.2"), AreaMap := "27.2.a"]
			ce_rcg[AreaMap %in% c("27.2.a.1", "27.2.a.2"), AreaMap := "27.2.a"]
			
			cl_rcg[AreaMap %in% c("27.2.b.2"), AreaMap := "27.2.b"]			
			ce_rcg[AreaMap %in% c("27.2.b.2"), AreaMap := "27.2.b"]			
			
			cl_rcg[AreaMap %in% c("27.14.b.1", "27.14.b.2"), AreaMap := "27.14.b"]
			ce_rcg[AreaMap %in% c("27.14.b.1", "27.14.b.2"), AreaMap := "27.14.b"]
			}
		
		if(target_region=="RCG_NA") 
			{		
			cl_rcg[AreaMap %in% c("27.5.b.1","27.5.b.1.a","27.5.b.1.b","27.5.b.2"), AreaMap := "27.5.b"]
			ce_rcg[AreaMap %in% c("27.5.b.1","27.5.b.1.a","27.5.b.1.b","27.5.b.2"), AreaMap := "27.5.b"]
			
			cl_rcg[AreaMap %in% c("27.9.b.1", "27.9.b.2"), AreaMap := "27.9.b"]
			ce_rcg[AreaMap %in% c("27.9.b.1", "27.9.b.2"), AreaMap := "27.9.b"]	

			cl_rcg[AreaMap %in% c("27.10.a.1","27.10.a.2"), AreaMap := "27.10.a"]
			ce_rcg[AreaMap %in% c("27.10.a.1","27.10.a.2"), AreaMap := "27.10.a"]				

			cl_rcg[AreaMap %in% c("27.8.e.1"), AreaMap := "27.8.e"]
			ce_rcg[AreaMap %in% c("27.8.e.1"), AreaMap := "27.8.e"]
			
			cl_rcg[AreaMap %in% c("27.8.d.2"), AreaMap := "27.8.d"]
			ce_rcg[AreaMap %in% c("27.8.d.2"), AreaMap := "27.8.d"]
			
			cl_rcg[AreaMap %in% c("27.7.c.1","27.7.c.2"), AreaMap := "27.7.c"]
			ce_rcg[AreaMap %in% c("27.7.c.1","27.7.c.2"), AreaMap := "27.7.c"]
			
			cl_rcg[AreaMap %in% c("27.6.b.1","27.6.b.2"), AreaMap := "27.6.b"]
			ce_rcg[AreaMap %in% c("27.6.b.1","27.6.b.2"), AreaMap := "27.6.b"]
			
			cl_rcg[AreaMap %in% c("27.7.j.2"), AreaMap := "27.7.j"]
			ce_rcg[AreaMap %in% c("27.7.j.2"), AreaMap := "27.7.j"]
			
			cl_rcg[AreaMap %in% c("27.7.k.1", "27.7.k.2"), AreaMap := "27.7.k"]
			ce_rcg[AreaMap %in% c("27.7.k.1", "27.7.k.2"), AreaMap := "27.7.k"]
		
			cl_rcg[AreaMap %in% c("27.10"), AreaMap := "NA"]	# div required	(minority of records)			
			ce_rcg[AreaMap %in% c("27.10"), AreaMap := "NA"]	# div required	(minority of records)	

			cl_rcg[AreaMap %in% c("27.6"), AreaMap := "NA"]		# div required	(minority of records)			
			ce_rcg[AreaMap %in% c("27.6"), AreaMap := "NA"]		# div required	(minority of records)				
			
			cl_rcg[AreaMap %in% c("27.7"), AreaMap := "NA"]		# div required	(minority of records)		
			ce_rcg[AreaMap %in% c("27.7"), AreaMap := "NA"]		# div required	(minority of records)	
			
			}			
	# QCA: visual
		cl_rcg[, list(N=.N,ton1000 = round(sum(CLscientificWeight_1000ton),1)),list(AreaMap,CLarea)][order(AreaMap)]
		cl_rcg[, list(N=.N,ton1000 = round(sum(CLscientificWeight_1000ton),1)),list(AreaMap,CLarea, CLvesselFlagCountry, CLyear)][order(AreaMap)][AreaMap=="NA",]
		
		ce_rcg[, list(N=.N,TripsNumber = sum(CEnumberOfFractionalTrips)),list(AreaMap,CEarea)][order(AreaMap)]
		ce_rcg[, list(N=.N,TripsNumber = sum(CEnumberOfFractionalTrips)),list(AreaMap,CEarea, CEvesselFlagCountry, CEyear)][order(AreaMap)][AreaMap=="NA",]
	
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
		DiffFracDomTrips_ctry

################################################################################
################################################################################
#
#		SMALL SCALE FISHERIES
#
################################################################################
################################################################################

if (restrict_to_SSF_data==TRUE)
	{
	cl_rcg<-cl_rcg[CLvesselLengthCategory %in% c("VL0006", "VL0608", "VL0810", "VL1012"),]
	cl_rcg[,CLvesselLengthCategory:=factor(CLvesselLengthCategory, levels=c("VL0006", "VL0608", "VL0810", "VL1012"))]
	ce_rcg<-ce_rcg[CEvesselLengthCategory %in% c("VL0006", "VL0608", "VL0810", "VL1012"),]
	ce_rcg[,CEvesselLengthCategory:=factor(CEvesselLengthCategory, levels=c("VL0006", "VL0608", "VL0810", "VL1012"))]
	}


################################################################################
################################################################################
#
#             SPECIES SCIENTIFIC NAME
#
################################################################################
################################################################################

cl_rcg[,SpeciesLaName:=aux_species$ScientificName[match(cl_rcg$CLspeciesCode, aux_species$AphiaID_accepted)]]

# QCA: should yield TRUE otherwise debug
nrow(cl_rcg[is.na(SpeciesLaName),]) == 0
dim(cl_rcg[is.na(SpeciesLaName),])

## Evaluate the need to put all "Trachurus" as "Trachurus spp", except "Trachurus trachurus"


################################################################################
################################################################################
#
#             CATCH GROUP
#
################################################################################
################################################################################

cl_rcg[,CatchGroup:=aux_species$CatchGroup[match(cl_rcg$CLspeciesCode, aux_species$AphiaID_accepted)]]

# QCA: should yield TRUE otherwise debug
nrow(cl_rcg[is.na(CatchGroup),]) == 0
unique(cl_rcg[is.na(CatchGroup),]$SpeciesLaName)

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

###############################################################################
################################################################################
#
#                                 FACTORIZATION <------------------ [establishes the order in unsorted bar graphs]
#
################################################################################
################################################################################

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

################################################################################
################################################################################
#
#                                 STOCK <---------------------------------------------- to be done
#
################################################################################
################################################################################


################################################################################
################################################################################
#
#                                 SAVE DATA
#
################################################################################
################################################################################

file_info_cl<-file.info(file_cl)
file_info_ce<-file.info(file_ce)


if (restrict_to_SSF_data==FALSE)
	{
	save(cl_rcg, file_info_cl, file = paste(dir_output_rcg, paste("/RDBES",target_region,"CL", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	save(ce_rcg, file_info_ce, file = paste(dir_output_rcg, paste("/RDBES",target_region,"CE", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	
	save(cl, file_info_cl, file = paste(dir_output_all, paste("/RDBES","All_Regions","CL", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	save(ce, file_info_ce, file = paste(dir_output_all, paste("/RDBES","All_Regions","CE", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	
	} else  {
	
	print(1)
	save(cl_rcg, file_info_cl, file = paste(dir_output_rcg, paste("/RDBES",target_region,"CL_SSF", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	save(ce_rcg, file_info_ce, file = paste(dir_output_rcg, paste("/RDBES",target_region,"CE_SSF", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	save(cl, file_info_cl, file = paste(dir_output_all, paste("/RDBES","All_Regions","CL_SSF", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	save(ce, file_info_ce, file = paste(dir_output_all, paste("/RDBES","All_Regions","CE_SSF", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	}

# NOTE: 'cl' and 'ce' data files were not prepared as the 'cl_rcg' and the 'ce_rcg' - do we want to save them anyway??
