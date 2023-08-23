# Combines eDNA project data from multiple sources into a single dataframe and outputs as a CSV
# Certain sections must be configured to fit your data, those can be found between the rows of
# Once configured, program should run in one go (Ctrl + Shift + Enter)
# Or line by line (Ctrl + Shift)

library(tidyverse)
library(glue)
library(mapview)
library(leafem)
library(sf)
library(lubridate)

#1) Sampler --------------------------------------------------------------------------------------
#'* Combines information from sampler CSVs into one place, also allows for some location visualization*

########################################################################
#'* Enter path to root directory of eDNA data here*
wd <- "C:/Users/HynesD/Documents/eDNA/data"
#Ex: wd <- "C:/Users/macaskilln/OneDrive - EC-EC/eDnaDataCombination"

#'* Folder name of Smith-Root CSVs here*
#setwd(paste(wd, "", sep="/"))
setwd(paste(wd, "WD46410AB", sep="/"))
########################################################################

filenames <- list.files(getwd(), pattern = "*.csv", recursive = TRUE, full.names = FALSE)

########################################################################
#'* If there are any Smith-Root samples you wish to discard, enter them here*
testSamples <- c("1", "2", "3", "4", "6", "7", "9", 
                 "10", "11", "16", "17", "19", "36", "47", 
                 "56", "57", "58", "66", "68", "76", "77", 
                 "87", "88", "90", "96", "115")
#Ex: testSamples <- c("01", "02", "03", "04", "06", "07", "09", "10", "11", "16", "17", "19", "36", "47", "56", "57", "58") #not actual samples; extra CSV files created when resetting pump
########################################################################



#Collates Smith-Root CSV information into one dataframe
#Skip=12: information from these files start on line 13
sampler <- filenames %>%
  set_names() %>%
  map_df(read_csv, .id = "file_name", skip = 12) %>%
  mutate(sampleId = str_remove(file_name, "^0+")) %>%
  mutate(sampleId = tools::file_path_sans_ext(basename(sampleId))) %>%
  #mutate(sampleId = substr(file_name, 4,5)) %>%
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
    dateTime = min(`Date (UTC)`)) %>%
    mutate(dateTime = with_tz(dateTime, "America/Halifax")) %>%
    bind_rows(
  tribble(~sampleId, ~waterVolumeL, ~dateTime, #Add samples below!
          "69", 2.00, ymd_hms("2022-04-18 13:25:00")
        ))



# Summarize location data for each sample----
latLon <- sampler %>%
  group_by(sampleId) %>% slice(which.min(`Accuracy (+/- m)`)) %>% select(
    sampleId,
    latitude = "Latitude",
    longitude = "Longitude",
    accuracyM = "Accuracy (+/- m)",
    elevationM = "Elevation (m)") %>%
  mutate(latitude = case_when(accuracyM == 2500.0 ~ 44.645357,
                              TRUE ~ latitude)) %>%
  mutate(longitude = case_when(accuracyM == 2500.0 ~ -62.806054,
                              TRUE ~ longitude)) 
 
# Map locations
latLonMap <- latLon %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) #%>% st_jitter(0.005)
# latLonMap2 <- latLon %>% filter(as.numeric(sampleId) > 96 & !sampleId == 114) %>%
 #  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) 


 
map1 <- mapview(latLonMap, layer.name = "Sample Sites", color = "black", col.region = "orange", alpha.region = 0.4, cex = 12)
map1
# map2 <- map1 %>% addStaticLabels(label = latLonMap$sampleId, textsize = "25px") 
# map2


# 2) DNA Yield --------------------------------------------------------------------------------------
#'* Extract DNA Yield data for each sample*
setwd("C:/Users/HynesD/Documents/eDNA/data")
dna <- read_csv(paste(getwd(), "dnaYields.csv", sep="/"))
dna <- dna %>% select(
  extractionKitId = "Extraction ID",
  sampleId = "Sample ID",
  filterSizeMm = "Filter Size",
  YieldNgPerMl = "Qubit Yield (ng/mL)") %>%
  mutate(sampleId = as.character(sampleId))

dna2 <- read_csv(paste(getwd(), "NM-MammalsYield.csv", sep="/"), locale=locale(encoding="latin1"))
dna2 <- dna2 %>%
  add_column(filterSizeMm = 1.2) %>%
  select(
    extractionKitId = "Kit ID",
    sampleId = "Sample ID",
    filterSizeMm,
    YieldNgPerMl = "DNA (ng/µl)"
  ) %>%
  mutate(YieldNgPerMl = YieldNgPerMl * 1000)

allDna <- rbind(dna, dna2)

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
    control = "Field control:",
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
  tribble(~sampleId, ~locality, ~ecosystem, ~ecosystemNotes, ~control, ~comments, ~substrate, ~pH, ~tempCelsius, ~tdsPpm, ~eC, #Add samples below!
              "15", "North Mud", "Pond", "Barachois pond", "No", "", "Brackish water", 7.51, 22.8, 651, 1419,
              "29", "Seal Island", "Wetland", "Emergence of sub-surface stream", "No", "two water samples taken at this location", "Freshwater", 4.24, 19.5, 226, 500,
              "35", "Seal Island", "Pond", "", "No", "", "Freshwater", 3.98,	20.3,	352, 659,
              "39", "Seal Island", "Pond", "Behind barrier dune","No", "", "Brackish water", 6.25, 20.1, 688, 1550
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
  mutate(comments = na_if(comments, "")) %>% #replace blanks with NAs
  mutate(control = ifelse(sampleId %in% c("32"), "No", control))

# Join Smith-Root, DNA yield, and field data into one data frame
allData <- fieldData %>% left_join(sumSampler) %>% left_join(allDna) %>% left_join(latLon)

# 4) eDNA Results --------------------------------------------------------------------------------------
#'* Collect and format eDNA data*
#'* eDNA data is store horizontally, we flip it vertically here*

eDna <- read_csv(paste(getwd(), "eDNA-Analysis-Results-MiMammal_12Sr.csv", sep="/")) #manually removed superfluous row and column from original XLSX file

# Calculate the number of samples (each sample takes up 4 columns)
numSamples = seq_len(ncol(eDna)/4)

# Individual collect percentReads, taxa, commonName and reads from the csv for each sample using a loop structure
tables = list()
varNames = c("percentReads", "taxon", "commonName", "reads")

for (i in 0:3) {
  tables[[i+1]] <- eDna[seq_len(ncol(eDna)) %% 4 == i]
  
  colnames(tables[[i+1]]) <- as.character(numSamples)
  tables[[i+1]] <- pivot_longer(tables[[i+1]], 1:37, names_to = "id", values_to = varNames[i+1]) %>%
    mutate(extractionKitId = unclass(glue("CW{id}"))) %>% select(extractionKitId, varNames[i+1])
}

# Collate eDNA data together
eDnaData <- cbind(tables[[2]], tables[[3]], tables[[4]], tables[[1]])[,c(1,2,4,6,8)]

# Filter out NA rows
eDnaData <- filter(eDnaData, rowSums(is.na(eDnaData)) != 4)

#Join extractionKitIds and sampleIds

allId <- dna %>% select(extractionKitId, sampleId) 

eDnaData <- eDnaData %>% left_join(allId) %>% select(-extractionKitId)


# NatureMetrics data----

eDna2 <- read_csv(paste(getwd(), "NM-MammalsReads.csv", sep="/"), na = c("-", "")) #manually removed superfluous row from original XLSX file

eDna3 <- eDna2 %>% mutate(Species = case_when(
  Genus == "Myodes" ~ "Myodes",
  Family == "Cricetidae" & is.na(Species) & is.na(Genus) ~ "Cricetidae",
  Genus == "Anaxyrus" ~ "Anaxyrus",
  Genus == "Anas" ~ "Anas",
  Genus == "Branta" ~ "Branta",
  Genus == "Larus" ~ "Larus",
  Genus == "Somateria" ~ "Somateria",
  TRUE ~ Species)) %>%
  select(commonName = "Common Name", taxon = Species, 15:36) %>%
  pivot_longer(!c(commonName, taxon), names_to = "sampleId", values_to = "reads") %>%
  drop_na(reads) %>%
  group_by(sampleId) %>%
  mutate(percentReads = round((reads/sum(reads))*100,2)) %>%
  ungroup()

#Brown Rat qPCR results----

rats <- fieldData %>% select(sampleId) %>%
  add_column(BrownRat = "Negative (0/12)") %>%
  mutate(BrownRat = case_when(sampleId == "25" ~ "Positive (1/12)",
                              sampleId == "51" ~ "Positive (1/12)",
                              sampleId == "52" ~ "Positive (1/12)",
                              sampleId == "53" ~ "Positive (9/12)",
                              sampleId == "119" ~ "Positive (1/12)",
                              sampleId == "125" ~ "Positive (1/12)",
                              TRUE ~ BrownRat))

#combine lab results

eDnaResults <- rbind(eDnaData, eDna3)

# Join all data together
finalDf <- left_join(allData, eDnaResults) %>% left_join(rats) 



finalDf <- finalDf %>% mutate(locality = case_when(locality == "North Mud" ~ "Mud Island",
                                                     locality == "Boot Island NWA" ~ "Boot Island",
                                                     locality == "CountryIsland" ~ "Country Island",
                                                     TRUE ~ locality))

# Collect number of detections for each species
#detections <- finalDf %>% group_by(sampleId, latitude, longitude, taxa, percentReads) %>% summarise() %>% pivot_wider(names_from = taxa, values_from = percentReads)

########################################################################
#'* Select an output location here*
#write_csv(finalDf, "C:/Users/HynesD/Documents/eDNA/eDnaCombined.csv", append = FALSE)
#write_csv(finalDf, "C:/Users/macaskilln/OneDrive - EC-EC/eDnaDataCombination/out.csv", append = FALSE)


