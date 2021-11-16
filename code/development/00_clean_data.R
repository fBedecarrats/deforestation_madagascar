############################################################################
#####                         KfW Schutzgebiete                        #####
############################################################################
#                                                                          #
# Authors: Melvin Wong                                                     #
############################################################################
# clean workspace, set options
rm(list=ls())

# get packages
lop <- c("dplyr", "tidyverse", "sf")
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


# aggregate project disbursement data by BMZ-year
projectdata_database<-
  read_csv("../../datalake/mapme.protectedareas/output/matching/model_frames/projectdata_supported.csv")
## reshape
projectdata_database_reshaped <- projectdata_database %>%
  gather(name, disbursement, 21:52) %>%
  mutate(year = as.numeric(substr(name, 14,17))) %>%
  select(-name)
## remove if BMZ==NA
projectdata_database_reshaped <- as.data.frame(projectdata_database_reshaped[-which(is.na(projectdata_database_reshaped$bmz_nummer)),])
## aggregate by BMZ-year
project_agg <- projectdata_database_reshaped %>%
    dplyr::group_by(bmz_nummer, year, first_year) %>%
    dplyr::summarize(disbursement_proj = sum(disbursement))
## create treatment variable for disbursement
project_agg$treatment_disb <- ifelse(project_agg$year >= project_agg$first_year, 1, 0)



# merge WDPA with project data and UID
## load
keys_database <-
  read_csv("../../datalake/mapme.protectedareas/output/matching/model_frames/keys_wdpaid_bmz.csv") %>% 
  dplyr::rename(., bmz_nummer=value)

sampling.ids <-
  read_csv("../../datalake/mapme.protectedareas/output/matching/model_frames/sampling.ids.csv")

## merge
bmz_wdpa.df <- merge(keys_database, project_agg,  by=c("bmz_nummer")) %>% 
  dplyr::rename(., wdpa_id=WDPAID)
## merge with UID
bmz_wdpa_uid.df <- merge(bmz_wdpa.df, sampling.ids,  by=c("wdpa_id")) %>% 
  relocate(., UID)
## order data
bmz_wdpa_uid.df <- bmz_wdpa_uid.df[
  order( bmz_wdpa_uid.df[,"UID"], bmz_wdpa_uid.df[,"year"] ),
] %>% 
  relocate(., c(UID, year))



duplicated(bmz_wdpa_uid.df[,1:2])


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

















