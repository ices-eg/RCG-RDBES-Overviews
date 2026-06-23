# from func_barplot

# Prepare colors - countries dataset
 
coloursDataset <- read.csv2(paste(auxDataPath, "/colours2.csv", sep = ''))  
countryDataset <- read.table(paste(auxDataPath, "/aux_countries.txt", sep = ''), sep = ",", header = T)

koloryRDBES <- read.table("https://raw.githubusercontent.com/ices-tools-dev/RDBESvisualise/refs/heads/dev/data/colourCountryTab.txt", header = TRUE)
