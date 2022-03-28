# eDNA Tools

## Description

This repository contains R functions used to analyze data collected from field samples, that aimed to extract environmental DNA from water bodies, during a biological inventorying initiative. Data from the project is derived from several sources, including:
- output from a [Smith-Root environmental DNA sampler](https://www.smith-root.com/edna/edna-sampler), which records sampling parameters such a volume (L), pressure (PSI), flow rate (L/min), etc. The sampler produces a CSV file each time the device is reset, i.e., for each water sample;
- other parameters from the water sample location, such as, pH, water temperature, substrate type, ecosystem, etc. This is collected via an iPad and [Survey123](https://survey123.arcgis.com/), and is exported as a CSV file; and
- results from the lab's bioinformatic pipeline, i.e., DNA yield and numbers of DNA reads per taxon.

