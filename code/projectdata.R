# script for analyzing  project information descriptively

library("tidyverse")
library("sf")

# working directory - setwd()
projectdata_db<-
  read_csv("../../datalake/mapme.protectedareas/input/project-data/xyz.csv")

# ----- data wrangling ----

# ---- analyze xyz ---- 

# ---- merge with KfW data ---- 
   