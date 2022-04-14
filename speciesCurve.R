## ---- species-sample-curve --------

library(tidyverse)
library(rotl)
library(ggtree)
library(phylobase)
library(aplot)
library(scico)
library(gtools)

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
    sampleId == "25" ~ "Seep",
    sampleId == "35" ~ "Seep",
    sampleId == "23" ~ "Brackish",
    str_detect(ecosystemNotes, "Barachois") ~ "Brackish",
    str_detect(ecosystemNotes, "barachois") ~ "Brackish",
    str_detect(ecosystemNotes, "Emergence") ~ "Seep",
    str_detect(ecosystemNotes, "Well") ~ "Well",
    str_detect(ecosystemNotes, "well") ~ "Well",
    str_detect(ecosystemNotes, "After rain, was dry before") ~ "Brackish",
    str_detect(ecosystemNotes, "Following rain") ~ "Brackish",
    str_detect(ecosystemNotes, "Following rain, previously sampled") ~ "Seep",
    str_detect(ecosystemNotes, "Isthmus pond, behind dune") ~ "Brackish",
    str_detect(ecosystemNotes, "Pond behind barrier dune") ~ "Brackish",
    str_detect(ecosystemNotes, "behind barrier dune") ~ "Brackish",
    str_detect(ecosystemNotes, "Feeds largest barachois from point E of N Home") ~ "Seep",
    str_detect(ecosystemNotes, "Very small stream") ~ "Seep",
    str_detect(ecosystemNotes, "Isthmus pond, behind dune") ~ "Brackish",
    str_detect(ecosystemNotes, "Standing water") ~ "Seep",
    str_detect(ecosystemNotes, "pan") ~ "Brackish",
    str_detect(ecosystemNotes, "Pond") ~ "Seep",
    str_detect(ecosystemNotes, "Small bog upstream") ~ "Brackish",
    str_detect(ecosystemNotes, "bog") ~ "Seep"
  )) %>%
  mutate(site = case_when(
    sampleId == "12" ~ "M1",
    sampleId == "15" ~ "M1",
    sampleId == "40" ~ "S1",
    sampleId == "41" ~ "S1",
    sampleId == "42" ~ "S2",
    sampleId == "43" ~ "S3",
    sampleId == "38" ~ "S4",
    sampleId == "39" ~ "S4",
    sampleId == "37" ~ "S5",
    sampleId == "53" ~ "S6",
    sampleId == "33" ~ "S6",
    sampleId == "44" ~ "S7",
    sampleId == "35" ~ "S8",
    sampleId == "52" ~ "S8",
    sampleId == "34" ~ "S8",
    sampleId == "32" ~ "S9",
    sampleId == "51" ~ "S9",
    sampleId == "49" ~ "S10",
    sampleId == "50" ~ "S11",
    sampleId == "31" ~ "S11",
    sampleId == "48" ~ "S12",
    sampleId == "30" ~ "S12",
    sampleId == "45" ~ "S13",
    sampleId == "23" ~ "S14",
    sampleId == "25" ~ "S15",
    sampleId == "27" ~ "S16",
    sampleId == "28" ~ "S17",
    sampleId == "29" ~ "S17",
    sampleId == "55" ~ "F1",
    sampleId == "54" ~ "F2",
    sampleId == "62" ~ "B1",
    sampleId == "61" ~ "B2",
    sampleId == "60" ~ "B3",
    sampleId == "64" ~ "B4",
    sampleId == "63" ~ "B5"))

ssc <- finalDf %>%
  mutate(taxa = case_when(str_detect(taxa, "Lavin") ~ "Leuciscidae",
                          TRUE ~ taxa)) %>%
  mutate(taxa = case_when(str_detect(taxa, "Phoca") ~ "Phoca vitulina",
                          TRUE ~ taxa)) %>%
  mutate(taxa = case_when(str_detect(taxa, "Canis lupus") ~ "Canis latrans",
                          TRUE ~ taxa)) %>%
  mutate(taxa = case_when(str_detect(taxa, "Sus") ~ "Sus scrofa domesticus",
                          TRUE ~ taxa)) %>%
  mutate(taxa = case_when(str_detect(taxa, "Equus") ~ "Equus caballus",
                          TRUE ~ taxa)) %>%
  group_by(site, taxa) %>%
  mutate(reads2 = sum(reads)) %>% 
  group_by(site) %>%
  #mutate(detections = length(reads2)) %>%
  distinct(taxa, .keep_all = TRUE) %>%
  #mutate(readsT = round(reads2^(1/5), 1)) %>%
  select(site, taxa, reads2) %>%
  pivot_wider(names_from = site, values_from = reads2) %>%
  column_to_rownames(var = "taxa") %>%
  replace(is.na(.), 0)

ssc2 <- finalDf %>%
  mutate(taxa = case_when(str_detect(taxa, "Lavin") ~ "Leuciscidae",
                          TRUE ~ taxa)) %>%
  mutate(taxa = case_when(str_detect(taxa, "Phoca") ~ "Phoca vitulina",
                          TRUE ~ taxa)) %>%
  mutate(taxa = case_when(str_detect(taxa, "Canis lupus") ~ "Canis latrans",
                          TRUE ~ taxa)) %>%
  mutate(taxa = case_when(str_detect(taxa, "Sus") ~ "Sus scrofa domesticus",
                          TRUE ~ taxa)) %>%
  mutate(taxa = case_when(str_detect(taxa, "Equus") ~ "Equus caballus",
                          TRUE ~ taxa)) %>%
  group_by(site, taxa) %>%
  mutate(reads2 = sum(reads)) %>% 
  group_by(site) %>%
  #mutate(detections = length(reads2)) %>%
  distinct(taxa, .keep_all = TRUE) %>%
  #mutate(readsT = round(reads2^(1/5), 1)) %>%
  select(site, taxa, reads2) %>%
  pivot_wider(names_from = taxa, values_from = reads2) %>%
  column_to_rownames(var = "site") %>%
  replace(is.na(.), 0)
