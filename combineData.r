# Combines eDNA project data from multiple sources into a single dataframe and outputs as a CSV
# Certain sections must be configured to fit your data, those can be found between the rows of
# Once configured, program should run in one go (Ctrl + Shift + Enter)
# Or line by line (Ctrl + Shift)

library(tidyverse)
library(glue)
library(mapview)
library(sf)

#1) Sampler --------------------------------------------------------------------------------------
#'* Combines information from sampler CSVs into one place, also allows for some location visualization*

########################################################################
#'* Enter path to root directory of eDNA data here*
wd <- "C:/Users/HynesD/Documents/eDNA/data/WD46410AB"
#Ex: wd <- "C:/Users/macaskilln/OneDrive - EC-EC/eDnaDataCombination"

#'* Folder name of Smith-Root CSVs here*
setwd(paste(wd, "", sep="/"))
#Ex: setwd(paste(wd, "WD46410AB", sep="/"))
########################################################################

filenames <- list.files(getwd(), pattern = "*.csv", recursive = TRUE, full.names = FALSE)

########################################################################
#'* If there are any Smith-Root samples you wish to discard, enter them here*
testSamples <- c("01", "02", "03", "04", "06", "07", "09", "10", "11", "16", "17", "19", "36", "47", "56", "57", "58", "66", "68")
#Ex: testSamples <- c("01", "02", "03", "04", "06", "07", "09", "10", "11", "16", "17", "19", "36", "47", "56", "57", "58") #not actual samples; extra CSV files created when resetting pump
########################################################################

#Collates Smith-Root CSV information into one dataframe
#Skip=12: information from these files start on line 13
sampler <- filenames %>%
  set_names() %>%
  map_df(read_csv, .id = "file_name", skip = 12) %>%
  mutate(sampleId = substr(file_name, 4,5)) %>%
  filter(!sampleId %in% testSamples)


#Summarize important Smith-Root data for each sample
sumSampler <- sampler %>% 
  group_by(sampleId) %>% summarise(
    waterVolumeL = max(`Volume (l)`),
    pressurePsi = median(`Pressure (psi)`),
    flowLPerMin = mean(`Flow (l/min)`),
    pumpTimeS = max(`Elapsed time (s)`),
    rateMlPerM = (mean(`Rate (mL/m)`)),
    speedMPerS = mean(`Speed (m/s)`),
    dateTime = min(`Date (UTC)`))

# Summarize location data for each sample
latLon <- sampler %>% 
  group_by(sampleId) %>% slice(which.min(`Accuracy (+/- m)`)) %>% select(
    sampleId,
    latitude = "Latitude",
    longitude = "Longitude", 
    accuracyM = "Accuracy (+/- m)",
    elevationM = "Elevation (m)")

# Map locations
latLonMap <- latLon %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
mapview(latLonMap)

# 2) DNA Yield --------------------------------------------------------------------------------------
#'* Extract DNA Yield data for each sample*
setwd("C:/Users/HynesD/Documents/eDNA/data")
dna <- read_csv(paste(getwd(), "dnaYields.csv", sep="/"))
dna <- dna %>% select(
  extractionId = "Extraction ID",
  sampleId = "Sample ID",
  filterSizeMicro = "Filter Size",
  qubitYieldNgPerMl = "Qubit Yield (ng/mL)") %>%
  mutate(sampleId = as.character(sampleId))

# 3) Field site --------------------------------------------------------------------------------------
#'* Collect field site data here*

fieldData <- read_csv(paste(getwd(), "eDNAProtectedAreas_0.csv", sep="/"), locale = locale(encoding = "latin1"))
names(fieldData) <- gsub(" \\s*\\([^\\)]+\\)", "", names(fieldData))

fieldData <- fieldData %>%
  select(
    sampleId = "Sample ID:",
    locality = "Locality:",
    ecosystem = "Ecosystem type:",
    ecosystemNotes = "Other ecosystem type:",
    comments,
    substrate = "Substrate type:",
    pH = "pH:",
    tempCelsius = "Temperature:",
    tdsPpm = "TDS:",
    eC = "Electrical Conductivity:") %>%
  mutate(sampleId = as.character(sampleId))

########################################################################
#'*If there are any samples you wish to add or remove manually, or data you wish to add, do so here*

fieldData <- rbind(
  tribble(~sampleId, ~locality, ~ecosystem, ~ecosystemNotes, ~comments, ~substrate, ~pH, ~tempCelsius, ~tdsPpm, ~eC, #Add samples below!
              "15", "North Mud", "Pond", "Barachois pond", "", "Brackish water", 7.51, 22.8, 651, 1419,
              "29", "Seal Island", "Wetland", "Emergence of sub-surface stream", "two water samples taken at this location", "Freshwater", 4.24, 19.5, 226, 500,
              "35", "Seal Island", "Pond", "", "", "Freshwater", 3.98,	20.3,	352, 659,
              "39", "Seal Island", "Pond", "behind barrier dune", "", "Brackish water", 6.25, 20.1, 688, 1550
  ),
  fieldData
) %>%
  filter(!sampleId %in% c(0) ) %>% #Remove samples by ID here!
  add_column() #Add additional columns here!

#Ex:
#fieldData <- rbind(
#  tribble(~sampleId, ~locality, ~ecosystem, ~ecosystemNotes, ~comments, ~substrate, ~pH, ~tempCelsius, ~tdsPpm, ~eC, #Add samples below!
#          "15", "North Mud", "Pond", "Barachois pond", "", "Brackish water", 7.51, 22.8, 651, 1419,
#          "29", "Seal Island", "Wetland", "Emergence of sub-surface stream", "two water samples taken at this location", "Freshwater", 4.24, 19.5, 226, 500,
#          "35", "Seal Island", "Pond", "", "", "Freshwater", 3.98,	20.3,	352, 659,
#          "39", "Seal Island", "Pond", "behind barrier dune", "", "Brackish water", 6.25, 20.1, 688, 1550,
#  ),
#  fieldData
#) %>%
#  filter(!sampleId %in% c(0)) %>% #Remove samples by ID here!
#  add_column(sampleCollector = "Doug Hynes, Karel Allard")
########################################################################

fieldData <-
  fieldData %>%
  mutate(ecosystemNotes = na_if(ecosystemNotes, "A")) %>% #replace error "A" with NAs
  mutate(ecosystemNotes = na_if(ecosystemNotes, "")) %>% #replace blanks with NAs
  mutate(comments = na_if(comments, "")) #replace blanks with NAs

# Join Smith-Root, DNA yield, and field data into one data frame
allData <- inner_join(sumSampler, latLon) %>% inner_join(fieldData) %>% left_join(dna)

# 4) eDNA Results --------------------------------------------------------------------------------------
#'* Collect and format eDNA data*
#'* eDNA data is store horizontally, we flip it vertically here*

eDna <- read_csv(paste(getwd(), "eDNA-Analysis-Results-MiMammal_12Sr.csv", sep="/")) #manually removed superfluous row and column from original XLSX file

# Calculate the number of samples (each sample takes up 4 columns)
numSamples = seq_len(ncol(eDna)/4)

# Individual collect percentReads, taxa, commonName and reads from the csv for each sample using a loop structure
tables = list()
varNames = c("percentReads", "taxa", "commonName", "reads")

for (i in 0:3) {
  tables[[i+1]] <- eDna[seq_len(ncol(eDna)) %% 4 == i]
  
  colnames(tables[[i+1]]) <- as.character(numSamples)
  tables[[i+1]] <- pivot_longer(tables[[i+1]], 1:37, names_to = "id", values_to = varNames[i+1]) %>%
    mutate(extractionId = unclass(glue("CW{id}"))) %>% select(extractionId, varNames[i+1])
}

# Collate eDNA data together
eDnaData <- cbind(tables[[2]], tables[[3]], tables[[4]], tables[[1]])[,c(1,2,4,6,8)]

# Filter out NA rows
eDnaData <- filter(eDnaData, rowSums(is.na(eDnaData)) != 4)

# Join all data together
finalDf <- left_join(allData, eDnaData)

# Collect number of detections for each species
detections <- finalDf %>% group_by(sampleId, latitude, longitude, taxa, percentReads) %>% summarise() %>% pivot_wider(names_from = taxa, values_from = percentReads)

########################################################################
#'* Select an output location here*
write_csv(finalDf, "", append = FALSE)
#write_csv(finalDf, "C:/Users/macaskilln/OneDrive - EC-EC/eDnaDataCombination/out.csv", append = FALSE)