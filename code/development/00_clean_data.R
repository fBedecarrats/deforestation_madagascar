############################################################################
#####                         KfW Schutzgebiete                        #####
############################################################################
#                                                                          #
# Authors: Melvin Wong                                                     #
############################################################################
# clean workspace, set options
rm(list=ls())

# get packages
lop <- c("dplyr", "plm", "stargazer", "tidyverse", "sf")
newp <- lop[!(lop %in% installed.packages()[,"Package"])]
if(length(newp)) install.packages(newp)
lapply(lop, require, character.only = TRUE)

# set working directory
setwd("~/shared/datalake/mapme.protectedareas")


# Reshape fcl_supported_AND_nonPas: wide -> long
## Load data
fcl_supported_AND_nonPas <-
  read_csv("../../datalake/mapme.protectedareas/output/matching/model_frames/fcl_supported_AND_nonPas.csv")

## reshape
fcl_reshaped <- fcl_supported_AND_nonPas %>%
  gather(name, loss, 2:22) %>%
  mutate(year = as.numeric(substr(name, 6,9))) %>%
  select(-name)





# merge WDPA with project data
## load
keys_database<-
  read_csv("../../datalake/mapme.protectedareas/output/matching/model_frames/keys_wdpaid_bmz.csv") %>% 
  dplyr::rename(., bmz_nummer=value)

projectdata_database<-
  read_csv("../../datalake/mapme.protectedareas/output/matching/model_frames/projectdata_supported.csv")
## merge
bmz_wdpa.df <- merge(keys_database, projectdata_database,  by=c("bmz_nummer"))









# Add time-invariant columns
time_invariant_vars <- 
  read_csv("../../datalake/mapme.protectedareas/output/matching/matching_frames/matching_frame_2015.csv")






# merge
fcl_matching_frames_merged <- fcl_reshaped %>%
  left_join(.,time_invariant_vars,
            by=c("UID"),
            suffix=c("","_delete")) %>%
  select(UID, loss, year, travel_time_to_nearby_cities_min_5k_10k, travel_time_to_nearby_cities_min_50k_100k, clay_content_30_cm,
         terrain_ruggedness_index_mean, elevation_mean, biome_max, country)

















