## ---- map --------

library(sf)
library(tidyverse)
library(ggspatial)
library(patchwork)
library(ggrepel)



ns <- st_read("C://Users/HynesD/eDnaTools/data/novaScotia.gpkg", quiet = TRUE)
boot <- st_read("C://Users/HynesD/eDnaTools/data/bootIsland.gpkg" , quiet = TRUE)
flat <- st_read("C://Users/HynesD/eDnaTools/data/flatIsland.gpkg", quiet = TRUE)
mud <- st_read("C://Users/HynesD/eDnaTools/data/mudIsland.gpkg", quiet = TRUE)
seal <- st_read("C://Users/HynesD/eDnaTools/data/sealIsland.gpkg", quiet = TRUE)

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
    sampleId == "63" ~ "B5")) %>%
  select(sampleId, site, lon = longitude, lat = latitude) %>% distinct(sampleId, .keep_all = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 

#inset <- mf_worldmap(water_col = "grey89", border_col = "black", land_col = "White", lon = -64.75, lat = 43.9,  pch = 0, cex = 2, lwd=4)

sl <- locs %>% filter(str_detect(site, "S")) %>% distinct(site, .keep_all = TRUE) %>%  mutate(long = sf::st_coordinates(.)[,1], lat = sf::st_coordinates(.)[,2])
fl <- locs %>% filter(str_detect(site, "F")) %>% distinct(site, .keep_all = TRUE) %>%  mutate(long = sf::st_coordinates(.)[,1], lat = sf::st_coordinates(.)[,2])
bl <- locs %>% filter(str_detect(site, "B")) %>% distinct(site, .keep_all = TRUE) %>%  mutate(long = sf::st_coordinates(.)[,1], lat = sf::st_coordinates(.)[,2])
ml <- locs %>% filter(str_detect(site, "M")) %>% distinct(site, .keep_all = TRUE) %>%  mutate(long = sf::st_coordinates(.)[,1], lat = sf::st_coordinates(.)[,2])

labs <- tribble(~site, ~long, ~lat, "S, M, F", -66.02551, 43.40595, "B",-64.26642, 45.14374) %>%
  st_as_sf(coords = c(Longitude = "long", Latitude = "lat"), crs = 4326, remove = FALSE)
 


n <- ggplot() + geom_sf(data = ns, fill = NA) + theme_bw() +
  geom_sf(data=labs , pch = 19,  cex = 3) +
  geom_label_repel(data= labs,aes(x = long, y = lat, label = site), size = 4, min.segment.length = Inf, fontface = "bold") +
  annotate("text", label = "Atlantic Ocean", x= -63.015, y = 44, size = 4, angle = 37) + xlab("Latitude") +ylab("Longitude") +
  annotate("text", label = "Nova Scotia", x= -63.36, y = 45.15, size = 3, angle = 28) 
  #scale_x_continuous(expand = c(0, 0)) +
  #scale_y_continuous(expand = c(0, 0))
  
  # annotate("text", label = "S", x = -66.01451 , y = 43.41095, size = 12) +
  # annotate("text", label = "B", x = -64.26842 , y = 45.14074, size = 12)



s <- ggplot() + geom_sf(data = seal, fill = NA) + 
  #geom_sf(data=sl,color = "black", fill = "yellow", pch =21,  cex = 4.5) +
  geom_sf(data=sl, pch =19,  cex = 4) +
  geom_text_repel(data= sl,aes(x = long, y = lat, label = site), size = 5, min.segment.length = Inf, box.padding = 0.37, fontface = "bold") +
  theme_void() + 
  annotation_scale(width_hint = 0.3, line_width = 1, bar_cols = c("black"), height = unit(0.1, "cm"), location = "br") +
  annotate("text", label = "Seal", x= -66.015, y = 43.422, size = 5, fontface = "italic")


b <- ggplot() + geom_sf(data = boot, fill = NA) + 
  #geom_sf(data=bl, color = "black", fill = "yellow", pch =21,  cex = 4.5) +
  geom_sf(data=bl, pch =19,  cex = 4) +
  geom_text_repel(data= bl,aes(x = long, y = lat, label = site), size = 5, min.segment.length = Inf, box.padding = 0.4, fontface = "bold") + 
  theme_void() +
  annotation_scale(width_hint = 0.25, line_width = 1, bar_cols = c("black"), height = unit(0.1, "cm"), location = "br") +
  annotate("text", label = "Boot", x= -64.275, y = 45.143, size = 5, fontface = "italic")


f <- ggplot() + geom_sf(data = flat, fill = NA) +  
  #geom_sf(data=fl, color = "black", fill = "yellow", pch =21,  cex = 4.5) +
  geom_sf(data=fl, pch =19,  cex = 4) +
  geom_text_repel(data= fl, aes(x = long, y = lat, label = site), size = 5, min.segment.length = Inf, box.padding = 0.4, fontface = "bold") + 
  theme_void() +
  annotation_scale(width_hint = 0.25, line_width = 1, bar_cols = c("black"), height = unit(0.1, "cm"), location = "br") +
  annotate("text", label = "Flat", x= -66.0051, y = 43.5102, size = 5, fontface = "italic")
 
m <- ggplot() + geom_sf(data = mud, fill = NA) + 
  #geom_sf(data=ml, color = "black", fill = "yellow", pch =21,  cex = 4.5) +
  geom_sf(data=ml, pch =19,  cex = 4) +
  geom_text_repel(data= ml, aes(x = long, y = lat, label = site), size = 5, 
                  min.segment.length = Inf, box.padding = 0.9, fontface = "bold") +  
  theme_void() + 
  annotation_scale(width_hint = 0.4, line_width = 1, bar_cols = c("black"), height = unit(0.1, "cm"), location = "br") + 
  annotate("text", label = "Mud", x= -65.9908, y = 43.4949, size = 5, fontface = "italic")

design <- "
  124
  325
  "
n + s + f + m + b + plot_layout(design = design, widths = c(0.5, 0.5))

#n + s + f + m + b + plot_layout(design = design, widths = c(0.25))









