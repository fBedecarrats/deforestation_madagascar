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


# Forest loss data: Reshape fcl_supported_AND_nonPas: wide -> long
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

## get area data by WDPA
area.sf <- read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique.gpkg") %>% 
  select(WDPAID, AREA_KM2) %>% 
  dplyr::rename(., wdpa_id=WDPAID)




## merge BMZ with WDPA
bmz_wdpa.df <- merge(keys_database, project_agg,  by=c("bmz_nummer")) %>% 
  dplyr::rename(., wdpa_id=WDPAID)
## aggregate BMZ disb and treatment variables by WDPA
bmz_wdpa.2df <- bmz_wdpa.df %>%
  dplyr::group_by(wdpa_id, year) %>%
  dplyr::summarize(first_year = min(first_year),
                   disbursement_proj = sum(disbursement_proj),
                   treatment_disb = max(treatment_disb))

## add WDPA area
bmz_wdpa.3df <- merge(bmz_wdpa.2df, area.sf,  by=c("wdpa_id")) 
## Calculate disbursement per BMZ-UID-myear conditional on actual disbursements (or calculate disbursement per sq km)
bmz_wdpa.3df <- bmz_wdpa.3df %>% 
  mutate(disb_sqkm = disbursement_proj/AREA_KM2)
  
  
## merge with UID
bmz_wdpa_uid.df <- merge(bmz_wdpa.3df, sampling.ids,  by=c("wdpa_id")) %>% 
  relocate(., UID)
## order data
bmz_wdpa_uid.df <- bmz_wdpa_uid.df[
  order( bmz_wdpa_uid.df[,"UID"], bmz_wdpa_uid.df[,"year"] ),
] %>% 
  relocate(., c(UID, year))



# Aggregate data by UID
## create standardized year (by matching year or first year)
uid.df <- bmz_wdpa_uid.df %>% 
  mutate(year_standard = year-first_year)
## Create new unique for aggregation later: UID_matching year
uid.df <- uid.df %>% 
  unite("uid_myear", c("UID", "first_year"), sep="_", remove = F)



# Add time-invariant columns
time_invariant_vars <- 
  read_csv("../../datalake/mapme.protectedareas/output/matching/matching_frames/matching_frame_2015.csv")
## merge time-invariant columns
fcl_matching_frames_merged <- fcl_reshaped %>%
  left_join(.,time_invariant_vars,
            by=c("UID"),
            suffix=c("","_delete")) %>%
  select(UID, loss, year, travel_time_to_nearby_cities_min_5k_10k, travel_time_to_nearby_cities_min_50k_100k, clay_content_30_cm,
         terrain_ruggedness_index_mean, elevation_mean, biome_max, country)
## merge with project data
out.df <- merge(uid.df, fcl_matching_frames_merged, by=c("UID", "year"), all = T) # include all here so that we do not drop control regions
## create variable indicatin if cell has ever been treated (needed for matching)
out.df$treat_ever <- NA
out.df$treat_ever[which(is.na(out.df$wdpa_id))] <- 0
out.df$treat_ever[which(!is.na(out.df$wdpa_id))] <- 1

## create data table for 2015 block
out2015.df <- uid.df %>% 
  select(-geom.x, -geom.y) %>% 
  subset(first_year==2015) %>% # subset 2015
  merge(.,fcl_matching_frames_merged, by=c("UID", "year"), all = T) %>%  # merge with matching frame
  mutate(., first_year=2015,
         disbursement_proj=case_when(is.na(disbursement_proj) ~ 0,
                                     TRUE ~ disbursement_proj),
         treatment_disb = case_when(is.na(treatment_disb) ~ 0,
                                    TRUE ~ treatment_disb),
         disb_sqkm = case_when(is.na(disb_sqkm) ~ 0,
                               TRUE ~ disb_sqkm),
         year_standard = case_when(is.na(year_standard) ~ year-first_year,
                               TRUE ~ year_standard)) %>%  # fill out NA
  unite("uid_myear", c("UID", "first_year"), sep="_", remove = F)
### create variable indicatin if cell has ever been treated (needed for matching)
out2015.df$treat_ever <- NA
out2015.df$treat_ever[which(is.na(out2015.df$wdpa_id))] <- 0
out2015.df$treat_ever[which(!is.na(out2015.df$wdpa_id))] <- 1

write_csv(out2015.df, "./output/tabular/regression_input/out2015.csv")
  
  







# # Things we do not need at the moment
# ## count how many cells per bmz_project treated
# x_list <- list(unique(uid.df$bmz_nummer)) # get list of unique BMZ numbers
# fun_countcell <- function(x){ # define funtion to count unique cell_myear by BMZ number
#   length(unique(uid.df$uid_myear[which(uid.df$bmz_nummer==x)]))
# }
# cell_list <- lapply(unique(uid.df$bmz_nummer), FUN = fun_countcell) # apply function
# names(cell_list) <- unlist(x_list) # give BMZ list names
# ### convert to dataframe
# cell.df <- as.data.frame(unlist(cell_list)) %>% 
#   mutate(bmz_nummer= rownames(.)) %>% 
#   dplyr::rename(., disb_cells="unlist(cell_list)")
# rownames(cell.df) <- 1:dim(cell.df)[1] # give running row names



















