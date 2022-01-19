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
# setwd("~/shared/datalake/mapme.protectedareas")


# Forest loss data: Reshape fcl_supported_AND_nonPas: wide -> long
## Load data
fcl_supported_AND_nonPas <-
  read_csv("../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08/gfw/gfw_10km_all.csv")

# ## reshape 
# fcl_reshaped <- fcl_supported_AND_nonPas %>%
#   gather(name, loss, 2:22) %>%
#   mutate(year = as.numeric(substr(name, 6,9))) %>%
#   select(-name)

# COMMENT JOHANNES: NOT SURE IF THE ABOVE IS STILL NEEDED. I SUBSTITUTED BY THE FOLLOWING
fcl_reshaped1 <- fcl_supported_AND_nonPas %>%
  select(poly_id,id.x,treatment.x, starts_with("area")) %>% 
  gather(name, fc_area, 4:24) %>% # this should be also made for loss variable
  mutate(year = as.numeric(substr(name, 6,9))) %>%
  select(-name)

fcl_reshaped2 <- fcl_supported_AND_nonPas %>%
  select(poly_id,id.x,treatment.x, starts_with("loss")) %>% 
  gather(name, fc_loss, 4:24) %>% # this should be also made for loss variable
  mutate(year = as.numeric(substr(name, 6,9))) %>%
  select(-name)

fcl_reshaped <- merge(fcl_reshaped1, fcl_reshaped2, by=c("poly_id", "year"))

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
    dplyr::group_by(bmz_nummer, year, first_year) %>% # check whether this makes sense
    dplyr::summarize(disbursement_proj = sum(disbursement))


## create treatment variable for disbursement
project_agg$treatment_disb <- ifelse(project_agg$year >= project_agg$first_year, 1, 0)

## COMMENT JOHANNES: This should be discussed. I would argue against leaving the treatment 1
## if there is no disbursement anymore. The projects have by far too few financial ressources to achieve a 
## longstanding impact. I suggest the following substitution
project_agg$treatment_disb_duringproj <- ifelse(project_agg$disbursement_proj > 0, 1, 0)


# merge WDPA with project data and UID
## load
keys_database <-
  read_csv("../../datalake/mapme.protectedareas/output/matching/model_frames/keys_wdpaid_bmz.csv") %>% 
  dplyr::rename(., bmz_nummer=value)

## COMMENT JOHANNES: I GUESS WE DO NOT NEED THAT INFORMATION ANYMORE
# sampling.ids <-
#   read_csv("../../datalake/mapme.protectedareas/output/matching/model_frames/sampling.ids.csv")

## get area data by WDPA
area.sf <- read_sf("../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique.gpkg") %>% 
  select(WDPAID, AREA_KM2, MARINE) %>% 
  dplyr::rename(., wdpa_id=WDPAID)


## merge BMZ with WDPA
bmz_wdpa.df <- merge(keys_database, project_agg,  by=c("bmz_nummer")) %>% 
  dplyr::rename(., wdpa_id=WDPAID)

## COMMENT JOHANNES: THIS ROUTINE SEEMS TO BE WRONG TO ME (AS DISCUSSED IN OUR MEETING ON FRINDAY)
## MY SUGGESTION IS TO FIRST AGGREGATE THE AMOUNT OF AREAS PER PROJECT AND THEN DIVIDE THE ANNUAL 
## DISBURSEMENTS BY THE AGGREGATED AREA. I THEREFORE WILL COMMENT THIS SECTION OUT AND CREATE A NEW 
## SECTION THAT DOES EXACTLY THAT

# ## aggregate BMZ disb and treatment variables by WDPA
# bmz_wdpa.2df <- bmz_wdpa.df %>%
#   dplyr::group_by(wdpa_id, year) %>%
#   dplyr::summarize(first_year = min(first_year),
#                    disbursement_proj = sum(disbursement_proj),
#                    treatment_disb = max(treatment_disb))
# 
# ## add WDPA area
# bmz_wdpa.3df <- merge(bmz_wdpa.2df, area.sf,  by=c("wdpa_id")) 
# ## Calculate disbursement per BMZ-UID-myear conditional on actual disbursements (or calculate disbursement per sq km)
# bmz_wdpa.3df <- bmz_wdpa.3df %>% 
#   mutate(disb_sqkm = disbursement_proj/AREA_KM2)
#   

## NEW ROUTINE BY JOHANNES
bmz_wdpa.df <- merge(keys_database, project_agg,  by=c("bmz_nummer")) %>% 
  dplyr::rename(., wdpa_id=WDPAID)

bmz_wdpa.df<-bmz_wdpa.df %>% 
  left_join(.,st_drop_geometry(area.sf),by="wdpa_id")

area.agg<- bmz_wdpa.df %>% 
  # st_drop_geometry() %>% 
  filter(treatment_disb_duringproj==1) %>% 
  group_by(year,bmz_nummer) %>% 
  summarize(area_total=sum(AREA_KM2))

# finance per total area  
bmz_wdpa.df<-bmz_wdpa.df %>% 
  left_join(.,area.agg,by=c("bmz_nummer","year"))


bmz_wdpa.df$area_total[is.na(bmz_wdpa.df$area_total)==T]<-0


bmz_wdpa.df$disbursement_sqkm<-bmz_wdpa.df$disbursement_proj/bmz_wdpa.df$area_total


# get quick summary stats of payments
bmz_wdpa.df %>% 
  filter(disbursement_sqkm>0) %>% 
  summary()

summary(bmz_wdpa.df$disbursement_sqkm)
## COMMENT JOHANNES: THE FOLLOWING ROUTINE WAS ALSO CHANGED SINCE WE DO NOT WORK WITH THE SAMPLE ANYMORE
# # ## merge with UID
# bmz_wdpa_uid.df <- merge(bmz_wdpa.3df, sampling.ids,  by=c("wdpa_id")) %>% 
#   relocate(., UID)
# ## order data
# bmz_wdpa_uid.df <- bmz_wdpa_uid.df[
#   order( bmz_wdpa_uid.df[,"UID"], bmz_wdpa_uid.df[,"year"] ),
# ] %>% 
#   relocate(., c(UID, year))


## HERE COMES THE NEW ROUTINE BY JOHANNES TO ASSIGN IDS FROM PAS TO POLYGONS
fishnet_all <-
  read_sf(
    "../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/fishnet_all_update_Dec-07.gpkg"
  )

pas_kfw <-
  read_sf(
    "../../datalake/mapme.protectedareas/input/wdpa_kfw/wdpa_kfw_spatial_latinamerica_2021-02-01_supportedPAs_unique.gpkg"
  )

# get intersections
fishnet_all_centroids<-st_centroid(st_transform(fishnet_all, crs = st_crs(pas_kfw)))

fishnet_all_intersections <-
  st_intersects(fishnet_all_centroids,pas_kfw)


# see how many sampling points fall within more than one treated area
table(unlist(lapply(fishnet_all_intersections,function(x)length(x))))
## note: we have 670 polygons that intersect with two treated PAs. that is a problem. This is probably result of overlapping polygons.

# retrive the WDPA ids for the intersections (for first and eventually second intersecting PA)
tmp_ids_1<-map(fishnet_all_intersections, 1)
tmp_ids_1[tmp_ids_1 == 'NULL'] <- NA
tmp_ids_1<-pas_kfw$WDPAID[unlist(tmp_ids_1)]

tmp_ids_2<-map(fishnet_all_intersections, 2)
tmp_ids_2[tmp_ids_2 == 'NULL'] <- NA
tmp_ids_2<-pas_kfw$WDPAID[unlist(tmp_ids_2)]

# attach to the remaining the wdpa id
fishnet_all_centroids <-
  fishnet_all_centroids %>%
  add_column(wdpa_id = tmp_ids_1) %>%
  add_column(wdpa_id_2 = tmp_ids_2) 

rm(tmp_ids_1,tmp_ids_2)

# View(st_drop_geometry(fishnet_all_centroids))

bmz_wdpa_uid.df <-
  merge(bmz_wdpa.df,
        st_drop_geometry(fishnet_all_centroids),
        by = c("wdpa_id"))


## order data
bmz_wdpa_uid.df <- bmz_wdpa_uid.df[
  order( bmz_wdpa_uid.df[,"poly_id"], bmz_wdpa_uid.df[,"year"] ),
  ] %>% 
  relocate(., c(poly_id, year))



# Aggregate data by UID
### NOTE JOHANNES: CHANGED UIDs for poly_ids in the following code


## create standardized year (by matching year or first year)
uid.df <- bmz_wdpa_uid.df %>% 
  mutate(year_standard = year-first_year)
## Create new unique for aggregation later: UID_matching year
uid.df <- uid.df %>% 
  unite("uid_myear", c("poly_id", "first_year"), sep="_", remove = F)


for (i in 2003:2020) {
  print(i)

# Add time-invariant columns
time_invariant_vars <- 
  read_csv(paste0("../../datalake/mapme.protectedareas/output/matching/matching_frames/matching_frame_", i, ".csv"))

## NOTE JOHANNES: SOMEHOW I MISSED THE YEAR COLUMN IN THE DATA SO I ADDED IT HERE MANUALLY
time_invariant_vars$year <- i

## merge time-invariant columns
fcl_matching_frames_merged <- fcl_reshaped %>%
  left_join(.,time_invariant_vars,
            by=c("poly_id"),
            suffix=c("","_delete")) %>%
  select(poly_id, 
         sum_fcl_matchingyear_t3,
         sum_fcl_matchingyear_tmax,
         fc_area_matchingyear,
         year,
         travel_time_to_nearby_cities_min_5k_100mio, 
         travel_time_to_nearby_cities_min_20l_100mio, 
         clay_content_10_cm,
         terrain_ruggedness_index_mean, 
         elevation_mean, 
         average_popgrowth,
         # biome_max, 
         country,
         fc_area,
         fc_loss)

class(fcl_matching_frames_merged)
class(uid.df)
## merge with project data
out.df <- merge(uid.df, fcl_matching_frames_merged, by=c("poly_id", "year"), all = T) # include all here so that we do not drop control regions
## merge with forest cover loss data
out.df <- merge(out.df, fcl_reshaped, by=c("poly_id", "year"), all = T) 
## create variable indicatin if cell has ever been treated (needed for matching)
out.df$treat_ever <- NA
out.df$treat_ever[which(is.na(out.df$wdpa_id))] <- 0
out.df$treat_ever[which(!is.na(out.df$wdpa_id))] <- 1

summary(out.df$disbursement_sqkm)

## create data table for matching frame block
out.df <- uid.df %>% 
  # select(-geom.x, -geom.y) %>% 
  subset(first_year==i) %>% # subset matching frame year
  merge(.,fcl_matching_frames_merged, by=c("poly_id", "year"), all = T) %>%  # merge with matching frame
  mutate(., first_year=i,
         disbursement_proj=case_when(is.na(disbursement_proj) ~ 0,
                                     TRUE ~ disbursement_proj),
         treatment_disb = case_when(is.na(treatment_disb) ~ 0,
                                    TRUE ~ treatment_disb),
         treatment_disb_duringproj = case_when(is.na(treatment_disb_duringproj) ~ 0,
                                    TRUE ~ treatment_disb_duringproj),
         disbursement_sqkm = case_when(is.na(disbursement_sqkm) ~ 0,
                               TRUE ~ disbursement_sqkm),
         year_standard = case_when(is.na(year_standard) ~ year-first_year,
                               TRUE ~ year_standard)) %>%  # fill out NA
  unite("uid_myear", c("poly_id", "first_year"), sep="_", remove = F)
### create variable indicatin if cell has ever been treated (needed for matching)
out.df$treat_ever <- NA
out.df$treat_ever[which(is.na(out.df$wdpa_id))] <- 0
out.df$treat_ever[which(!is.na(out.df$wdpa_id))] <- 1

summary(out.df$disbursement_sqkm)



## COMMENT: SAVED THE DATA AS NEW FILE
write_csv(out.df, paste0("../../datalake/mapme.protectedareas/output/tabular/regression_input/out", i, ".csv"))

}






# Things we do not need at the moment
## count how many cells per bmz_project treated
x_list <- list(unique(uid.df$bmz_nummer)) # get list of unique BMZ numbers
fun_countcell <- function(x){ # define funtion to count unique cell_myear by BMZ number
  length(unique(uid.df$uid_myear[which(uid.df$bmz_nummer==x)]))
}
cell_list <- lapply(unique(uid.df$bmz_nummer), FUN = fun_countcell) # apply function
names(cell_list) <- unlist(x_list) # give BMZ list names
### convert to dataframe
cell.df <- as.data.frame(unlist(cell_list)) %>%
  mutate(bmz_nummer= rownames(.)) %>%
  dplyr::rename(., disb_cells="unlist(cell_list)")
rownames(cell.df) <- 1:dim(cell.df)[1] # give running row names



















