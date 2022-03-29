#Clean and transform variables

library(tidyverse)

data <- read_csv("C:/Users/HynesD/Documents/eDNA/eDnaCombined.csv")

finalDf <- data %>% 
  mutate(taxa = case_when(str_detect(taxa, "Cyprinodont") ~ "Cyprinodontiformes",
                          TRUE ~ taxa)) %>% #fix spelling of taxon 
  mutate(taxa = case_when(str_detect(taxa, "Esociforme") ~ "Esociformes",
                          TRUE ~ taxa)) %>% #fix spelling of taxon  
  mutate(taxa = str_replace(taxa, pattern = "sp.", replacement = "")) %>%
  mutate(taxa = case_when(str_detect(taxa, "Ondatra zibethicus sp.") ~ "Ondatra zibethicus",
                          TRUE ~ taxa)) %>% #fix spelling
  mutate(commonName = case_when(str_detect(commonName, "Common muskrat") ~ "Muskrat",
                                TRUE ~ taxa)) %>% #fix spelling
  mutate(locality = case_when(str_detect(locality, "Boot Island NWA") ~ "Boot Island",
                              TRUE ~ locality)) %>% #fix name
  mutate(locality = case_when(str_detect(locality, "North Mud") ~ "Mud Island",
                              TRUE ~ locality)) %>% #fix name
  mutate(taxa = str_replace(taxa, pattern = "sp.", replacement = "")) %>%
  filter(!sampleId %in% c(21, 65)) %>% #remove controls
  mutate(qubitYieldNgPerMl = case_when(
    str_detect(qubitYieldNgPerMl, ">") ~ "12000", #
    TRUE ~ qubitYieldNgPerMl)) %>%
  mutate_at(vars(qubitYieldNgPerMl), as.numeric) %>%
  mutate(taxa = str_trim(taxa, side = "right")) %>%
  mutate(rain = case_when(
    sampleId == "48" ~ "After rain",
    sampleId == "49" ~ "After rain, not paired",
    sampleId == "50" ~ "After rain",
    sampleId == "51" ~ "After rain",
    sampleId == "52" ~ "After rain",
    sampleId == "53" ~ "After rain",
    sampleId == "30" ~ "Before rain",
    sampleId == "31" ~ "Before rain",
    sampleId == "32" ~ "Before rain",
    sampleId == "33" ~ "Before rain",
    sampleId == "35" ~ "Before rain",
    TRUE ~ "Not paired")) %>% #54 & 55 after rain but on Flat Island 
  mutate(rainPaired = case_when(
    sampleId == "48" ~ "A",
    sampleId == "49" ~ "After rain, not paired",
    sampleId == "50" ~ "B",
    sampleId == "51" ~ "C",
    sampleId == "52" ~ "D",
    sampleId == "53" ~ "E",
    sampleId == "30" ~ "A",
    sampleId == "31" ~ "B",
    sampleId == "32" ~ "C",
    sampleId == "33" ~ "E",
    sampleId == "35" ~ "D",
    TRUE ~ "Not paired")) %>%
  mutate(waterbody = case_when(
    ecosystemNotes = is.na(ecosystemNotes) ~ "Seep",
    str_detect(ecosystemNotes, "Barachois") ~ "Barachois",
    str_detect(ecosystemNotes, "barachois") ~ "Barachois",
    str_detect(ecosystemNotes, "Emergence") ~ "Seep",
    str_detect(ecosystemNotes, "Well") ~ "Well",
    str_detect(ecosystemNotes, "well") ~ "Well",
    str_detect(ecosystemNotes, "After rain, was dry before") ~ "Barachois",
    str_detect(ecosystemNotes, "Following rain") ~ "Barachois",
    str_detect(ecosystemNotes, "Following rain, previously sampled") ~ "Seep",
    str_detect(ecosystemNotes, "Isthmus pond, behind dune") ~ "Barachois",
    str_detect(ecosystemNotes, "Pond behind barrier dune") ~ "Barachois",
    str_detect(ecosystemNotes, "behind barrier dune") ~ "Barachois pond",
    str_detect(ecosystemNotes, "Feeds largest barachois from point E of N Home") ~ "Seep",
    str_detect(ecosystemNotes, "Very small stream") ~ "Seep",
    str_detect(ecosystemNotes, "Isthmus pond, behind dune") ~ "Barachois",
    str_detect(ecosystemNotes, "Standing water") ~ "Seep",
    str_detect(ecosystemNotes, "pan") ~ "Barachois",
    str_detect(ecosystemNotes, "Pond") ~ "Seep",
    str_detect(ecosystemNotes, "bog") ~ "Seep"))

#Summaries----

sumData <- finalDf %>%
  group_by(sampleId) %>%
  mutate(detections = length(reads)) %>%
  mutate(readsPerSampleId = sum(reads)) %>%
  mutate(perReads = round(reads/sum(reads), 3)) %>%
  ungroup() %>%
  mutate(totalReads = sum(reads)) %>%
  mutate(avePerRead = round(readsPerSampleId/totalReads*100, 1)) %>%
  select("Sample Id" = sampleId, "Water (L)" = waterVolumeL, pH,
                     "Temp (°C)" = tempCelsius, "TDS (ppm)" = tdsPpm, Conductivity = eC, 
                     "DNA Yield" = qubitYieldNgPerMl, "No. of Taxon" = detections, "Reads (%)" = avePerRead)

#Correlogram----


corData <- sumData %>% group_by("Sample Id") %>% distinct(.keep_all = TRUE) 

corFig <-
  ggpairs(corData,
          columns = 2:9,
          lower = list(continuous = wrap(
            "smooth",
            size = 2.5,
            alpha = 0.3
          ))) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

corFig




    