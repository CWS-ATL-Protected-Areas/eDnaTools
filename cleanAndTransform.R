#Clean and transform variables

library(tidyverse)
library(data.tree)
library(DiagrammeR)
library(taxize)
library(rotl)
library(ggtree)

#Import CSV----
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
  )) 

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

#Bar plot----

barData <- finalDf %>% group_by("Sample Id") %>% distinct(.keep_all = TRUE) %>%
  filter(!rainPaired == "Not paired") %>% 
  select(sampleId, taxa, rain) %>%
  count(rain, taxa) %>%
  mutate(taxa = fct_reorder(taxa, desc(n))) 

ggplot(barData, aes(x = fct_infreq(taxa), fill=rain)) +
  geom_bar(stat = "count", position = "dodge") +
  facet_wrap(vars(rain), scales = "free_y", ncol = 1) +
  theme_classic() +
  xlab("Taxa") +
  theme(axis.text.x = element_text(size=11.5, angle=45, vjust =1., hjust = 1),
        axis.text.y = element_text(size=11.5),
        axis.title = element_text(size=16)) +
  theme(legend.position = "none") +
  ylab("\n\nNo. of Point Occurrences \n") + xlab("Taxa") +
  geom_text(stat='count', aes(label=..count..), size = 3.0, vjust=-0.5) 


#cleanData <- read_csv("C:/Users/HynesD/eDnaTools/cleanedData.csv")




#Analysis of taxa----

#Muroidea - Superfamily rats, mice, voles, 
#Cricetidae - Muskrat Family - new world rats and mice, voles, Myodes, Microtus, Lemmus, etc. deer mouse
#Arvicolinae - subfamily - muskrat
#Murinae - subfamily old world rats and mice
#Lagamorpha
#Leporidae 
#Lepus
#Sus
#Soricidae
#Soricinae = shrews,could be Long-tailed (Blarina)
#Sorex sp. =  could be smoky, water, etc.
#Soricinae = could be other species
#Soricidae = Probably represents masked shrew; less probable smokey shrew or others
#Leuciscidae = true minnows
#Fundulus = Greater than one species of Fundulus possible across study extent

#sumData2 <- finalDf %>% group_by(taxa) %>% tally(reads)

sumData2 <- finalDf %>%
  mutate(taxa = case_when(str_detect(taxa, "Lavin") ~ "Leuciscidae",
                    TRUE ~ taxa)) %>%
  group_by(taxa) %>%
  mutate(readsPerTaxon = sum(reads)) %>%
  mutate(readsT = round(readsPerTaxon^(1/3), 0)) %>%
  mutate(occur = n()) %>%
  mutate(occurPer = round(occur/35*100, 0)) %>%
  distinct(taxa, .keep_all = TRUE) %>%
  #mutate(detections = length(reads)) %>%
  #mutate(readsPerSampleId = sum(reads)) %>%
  #mutate(perReads = round(reads/sum(reads), 3)) %>%
  #mutate(totalReads = sum(reads)) %>%
  #mutate(avePerRead = round(readsPerSampleId/totalReads*100, 1)) %>%
  select(taxa, readsT, occurPer) 

sumData2 <- finalDf %>%
  mutate(taxa = case_when(str_detect(taxa, "Lavin") ~ "Leuciscidae",
                          TRUE ~ taxa)) %>%
  group_by(sampleId, taxa) %>%
  mutate(readsT = round(reads^(1/4), 1)) %>%
  select(sampleId, taxa, readsT) %>%
  pivot_wider(names_from = sampleId, values_from = readsT)



#speciesNames <- as.character(getNames[, 11])
#cNames <- classification(speciesNames, db = "ncbi")

#tree_data <- tol_induced_subtree(ott_ids = ott_id(taxa))

taxonSearch <- tnrs_match_names(names = sumData2$taxa, context_name = "All life")
sumData2$ottName <- unique_name(taxonSearch)
sumData2$ottId <- taxonSearch$ott_id
ottInTree <- ott_id(taxonSearch)[is_in_tree(ott_id(taxonSearch))]
tre <- tol_induced_subtree(ott_ids = ottInTree)
plot(tre)
ggtree(tre) + geom_tiplab() + xlim(NA, 12)


tre$tip.label <- strip_ott_ids(tre$tip.label, remove_underscores = TRUE)
tre$tip.label %in% sumData2$ottName

sum_numeric <- sumData2[, c("readsT", "occurPer")]
rownames(sum_numeric) <- sumData2$ottName
treeData <- phylo4d(tre, sum_numeric)
plot(treeData)

e1 <- data.frame(x = seq(8.5, 9, length.out = 2),
                 lab = names(sum_numeric))


ggtree(treeData) + 
  geom_tippoint(aes(size = readsT), x = e1$x[1], shape = 1) +  
  geom_tippoint(aes(size = occurPer), x = e1$x[2], shape = 1) + 
# geom_tippoint(aes(size = culmenL), x = d1$x[3], shape = 1) + 
# geom_tippoint(aes(size = beakD),   x = d1$x[4], shape = 1) + 
# geom_tippoint(aes(size = gonysW),  x = d1$x[5], shape = 1) + 
scale_size_continuous(range = c(1,12), name="") + 
geom_text(aes(x = x, y = 0, label = lab), data = e1, angle = 45) +
geom_tiplab(offset = 1.3, fontface = 3) + xlim(0, 11) +
theme(legend.position = c(.1, .85))  


sum_numeric <- sumData2[, c(2:36)]
rownames(sum_numeric) <- sumData2$ottName
treeData <- phylo4d(tre, sum_numeric)
plot(treeData)


e2 <- data.frame(x = seq(10, 105, length.out = 35),
                 lab = names(sum_numeric))
ggtree(treeData) + 
  geom_tippoint(aes(size = X12), x = e2$x[1], shape = 1) +  
  geom_tippoint(aes(size = X15), x = e2$x[2], shape = 1) + 
  geom_tippoint(aes(size = X23), x = e2$x[3], shape = 1) + 
  geom_tippoint(aes(size = X25), x = e2$x[4], shape = 1) + 
  geom_tippoint(aes(size = X27), x = e2$x[5], shape = 1) + 
  geom_tippoint(aes(size = X28), x = e2$x[6], shape = 1) + 
  geom_tippoint(aes(size = X29), x = e2$x[7], shape = 1) + 
  geom_tippoint(aes(size = X30), x = e2$x[8], shape = 1) + 
  geom_tippoint(aes(size = X31), x = e2$x[9], shape = 1) + 
  geom_tippoint(aes(size = X32), x = e2$x[10], shape = 1) + 
  geom_tippoint(aes(size = X33), x = e2$x[11], shape = 1) + 
  geom_tippoint(aes(size = X34), x = e2$x[12], shape = 1) + 
  geom_tippoint(aes(size = X35), x = e2$x[13], shape = 1) + 
  geom_tippoint(aes(size = X37), x = e2$x[14], shape = 1) + 
  geom_tippoint(aes(size = X38), x = e2$x[15], shape = 1) + 
  scale_size_continuous(range = c(1,10), name="") + 
  geom_text(aes(x = x, y = 0, label = lab), data = e2, angle = 0) +
  geom_tiplab(offset = 60, fontface = 3) + xlim(0, 80) +
  theme(legend.position = c(.01, .85))  

gheatmap(ggtree(treeData, right = TRUE), data=sum_numeric, colnames_angle=0, width = 40) + 
  geom_tiplab(offset=330, fontface = 3) + hexpand(.2) + theme(legend.position = c(.01, .85))

#Trees----
# Fill in the taxonomic gaps for each identified taxa from the eDNA data, using the ncbi database
# https://yulab-smu.top/treedata-book/related-tools.html




#getNames <- tax_name(sumData2$taxa, get = c("phylum", "class", "order", "suborder", "superfamily", "family", "subfamily", "genus", "species"), db = "ncbi")

#newDf <- getNames %>% rename(taxa = "query") %>% inner_join(finalDf) %>% mutate(readsT = reads^(1/4)) %>% replace_na(list(class = "Reptilia"))
    
ggplot(data = newDf, aes(x = taxa, y = readsT, fill = waterbody)) +
  geom_col() +
  facet_grid(~order, scales = "free_x", # Let the x axis vary across facets.
             space = "free_x",  # Let the width of facets vary and force all bars to have the same width.
             switch = "x") +
  theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
        strip.background = element_rect(fill = "white"),  # Make facet label background white.
        axis.title = element_blank()) +
  theme(axis.text.x = element_text(size=11.5, angle=45, vjust =1.01, hjust = 1),
        axis.text.y = element_text(size=16),
        axis.title = element_text(size=16))


#Visualize evolutionary data hierarchically---- 

# DiagrammeR
# Tally the reads of each species
z2 <- z %>% group_by(taxa) %>% tally(reads)

# Fill in the taxonomic gaps for each identified taxa from the eDNA data, using the ncbi database
z3 <- tax_name(z2$taxa, get = c("phylum", "class", "order", "suborder", "superfamily", "family", "subfamily", "genus", "species"), db = "ncbi") %>%
  rename(taxa = "query") %>%
  inner_join(z2)

2# String together the taxa into a pathString (used to create the tree below)
z3$pathString <- paste(z3$phylum, z3$class, z3$order, z3$suborder, z3$superfamily, z3$family, z3$subfamily, z3$genus, z3$species, sep="/")

z3$pathString <- ""
groups <- c("phylum", "class", "order", "suborder", "superfamily", "family", "subfamily", "genus", "species")

for(i in 1:nrow(z3)) {
  isNA <- TRUE
  
  for(j in 11:3) {
    if(isNA) {
      if(!is.na(z3[i,j])) {
        z3$pathString[i] <- paste(z3[i,j], z3$pathString[i], sep = "/")
        isNA <- FALSE
      }
    } else {
      z3$pathString[i] <- paste(z3[i,j], z3$pathString[i], sep = "/")
    }
  }
}


tree <- as.Node(z3)
print(tree, "n")
plot(tree, "n")
t <- as.data.frame(tree)
