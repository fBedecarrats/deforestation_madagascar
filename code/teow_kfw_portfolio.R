#  teow_kfw_portfolio
# Authors: Johannes Schielein
# Purpose: This script uses the routines from the TEOW rmd files to process the whole KfW portfolio
# Notes: 
  # The teow rawdata is stored in the mapme datalake and is not part of this repository.
  # More information on where to download the TEOW data and how to process it is given in the project documentation website


# load required libraries
library("sf")
library("dplyr")

# load data
read_sf("../../datalake/")

file.copy("data/Terrestrial_Ecoregions_World.shx",
          "../../datalake/mapma.protectedareas_teow/Terrestrial_Ecoregions_World.shx")

dir.create("../../datalake/mapma.protectedareas_teow/")


