## ---- map --------

library(sf)
library(tidyverse)
library(ggspatial)
library(cowplot)


isl <- st_read("C://Users/HynesD/eDnaTools/images/fourIslands.gpkg", quiet = TRUE)

data <- read_csv("C:/Users/HynesD/Documents/eDNA/eDnaCombined.csv")

locs <- data %>% 
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
  select(sampleId, lon = longitude, lat = latitude) %>% distinct(sampleId, .keep_all = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


isl$centroid <- 
  sf::st_transform(isl) %>% 
  sf::st_centroid() %>% 
  #sf::st_transform(., '+proj=longlat +ellps=GRS80 +no_defs')  %>% 
  sf::st_geometry() 

padding <- 0.025

graph <- function(x){
  ggplot2::ggplot(isl[x,]) +
    geom_sf(colour = "black", fill = NA) +
    geom_sf(data = locs, color = "black", fill = "yellow", alpha = 0.7, size = 5, pch = 21) +
    coord_sf(xlim = c(isl$centroid[[x]][1]-padding , 
                      isl$centroid[[x]][1]+padding), 
             ylim = c(isl$centroid[[x]][2]-padding , 
                      isl$centroid[[x]][2]+padding), 
             expand = FALSE) +
    theme_bw()
    #theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) 
}

plot_list <- lapply(X = 1:nrow(isl), FUN = graph)

g <- cowplot::plot_grid(plotlist = plot_list,align = "v", nrow = 1) + 
  draw_label("Mud", 0.47, 0.5, size = 18) +
  draw_label("Flat", 0.45, 0.74, size = 18) +
  draw_label("Seal", 0.12, 0.53, size = 18) +
  draw_label("Boot", 0.81, 0.61, size = 18) +
  #draw_label("Longitude", 0.5, 0.1, size = 18, vjust = -3.75) +
  #draw_label("Latitude", 0.0, 0.5, size = 18, angle = 90, vjust = -2) +
  annotation_north_arrow(pad_x = unit(1.5, "cm"), pad_y = unit(16.5, "cm"), style = north_arrow_minimal())

print(g)

