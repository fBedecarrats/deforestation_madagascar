############################################################################
#####                         KfW Schutzgebiete                        #####
############################################################################
# Authors: Melvin Wong                                                     #
############################################################################
# clean workspace, set options
rm(list=ls())

# get packages
lop <- c("dplyr", "plm", "stargazer", "tidyverse", "cem")
newp <- lop[!(lop %in% installed.packages()[,"Package"])]
if(length(newp)) install.packages(newp)
lapply(lop, require, character.only = TRUE)

# set working directory
setwd("~/shared/datalake/mapme.protectedareas")

# ----- Create cut-off points for CEM -----
# Load data
out2015.df <- read.csv("./output/tabular/regression_input/out2015.csv")
static.df <- out2015.df %>% 
  subset(year==2015) %>% 
  filter(!is.na(fc_area_matchingyear))

# do dummy matching to get cut-off points and analyse them
cem_matched_test <-
  cem("treat_ever",
      as.data.frame(static.df),
      drop = c("MARINE", "sum_fcl_matchingyear_t3", "fc_area","fc_loss", "average_popgrowth", "travel_time_to_nearby_cities_min_20l_100mio", "treatment", "id", "poly_id", "wdpa_id", "bmz_nummer", "name", "left", "top", "right", "bottom",  "travel_time_to_nearby_cities_min_50k_100", "cem_weights", "uid_myear","UID", "year", "wdpa_id", "wdpa_id_2", "first_year", "disbursement_proj", "treatment_disb_duringproj", "treatment_disb", "disb_sqkm", "AREA_KM2", "year_standard", "strata", "area_total", "disbursement_sqkm", "disb_sqkm"),
      eval.imbalance = TRUE)
cem_matched_test$imbalance
cem_matched_test$tab

# create list for manual cutoff points
cutoffs_list<-
  cem_matched_test$breaks

# table default cutoffs CEM
table(cut(static.df$travel_time_to_nearby_cities_min_5k_100mio,
          cem_matched_test$breaks$travel_time_to_nearby_cities_min_5k_100mio),
      static.df$treat_ever)
## 1) ---- Travel time ---- 
# define proposed cuttoffs
proposed_cutoffs_traveltime<-
  c(0,120,300, 400, 500, 600, 800, 1000, 1500, 2000, 3000, max(static.df$travel_time_to_nearby_cities_min_5k_100mio,na.rm = T)) # 0-2hrs, 2-6 hrs, >6 hrs

table(cut(static.df$travel_time_to_nearby_cities_min_5k_100mio,
          proposed_cutoffs_traveltime),
      static.df$treat_ever)
# change list to proposed cutoffs
cutoffs_list$travel_time_to_nearby_cities_min_5k_100mio<- 
  proposed_cutoffs_traveltime

## 2) ---- ruggedness ---- 
# definition of TRI see: https://community.esri.com/t5/water-resources-blog/terrain-ruggedness-index-tri-and-vector-ruggedness/ba-p/884340
# interpretation: 
# We are dealing with 30 meter resolution in original DEM data. This means that a TRI of 10 is an average slope of 30%. A
# According to some websites the maximum slope for a agricultural mechanization should be 15 degrees or 26%
# We suggest the following cutoff points (0-5) for easy mechanization 5-10 for medium mechanization and >10 for no mechanization
# note, that the TRI values are averaged. This means that we could have parts of the AOI with high TRIs and low TRIs which would result in medium TRI. 

# define proposed cuttoffs
proposed_cutoffs_tri<-
  c(0,5,10,max(static.df$terrain_ruggedness_index_mean,na.rm = T))
#  assign new cutoff points
cutoffs_list$terrain_ruggedness_index_mean<-proposed_cutoffs_tri

## 3) ---- Elevation ---- 
# we simplify by creating three different altitude groups. Lowland 0-500, midland 500-1500, highland >1500
# define proposed cuttoffs
proposed_cutoffs_elevation<-
  c(0,500,1500,max(static.df$elevation_mean,na.rm = T))
#  assign new cutoff points
cutoffs_list$elevation_mean<-proposed_cutoffs_elevation

## 4) ---- Forest Cover Loss ----
# proposed simplified cutoffs: forest cover loss yes and no. 
# define proposed cuttoffs
proposed_cutoffs_fcl<-
  c(-1,0, 1, 10, 100, 200, 300, 400, 500, 600, 1000, max(static.df$sum_fcl_matchingyear_tmax))
#  assign new cutoff points
cutoffs_list$average_fcl_matchingyear<-proposed_cutoffs_fcl





# create table displaying number of matched obs in treatment and control group
i = 2015
overview.list <- as.list(cem_matched_test$tab[2,])
overview.list$year <- i

cem_matched_test$tab[2,1] <- "control"
cem_matched_test$tab[2,2] <- "treatment"

i=2014
overview.list$year[2] <- i
unlist(overview.list)
cem_matched_test$tab[2,1]




T_year <- c(2004:2013, 2015, 2016, 2019)
for (i in T_year) {
  print(i)
  
  # Load data
  out.df <- read.csv(paste0("./output/tabular/regression_input/out", i, ".csv"))
  static.df <- out.df %>% 
    subset(year==i) %>% 
    filter(!is.na(fc_area_matchingyear))
  
## ---- CEM ----
# imbalance(
#   static.df$treat_ever,
#   as.data.frame(static.df),
#   drop = c("average_popgrowth", "travel_time_to_nearby_cities_min_20l_100mio", "treatment", "id", "poly_id", "wdpa_id", "bmz_nummer", "name", "left", "top", "right", "bottom",  "travel_time_to_nearby_cities_min_50k_100", "cem_weights", "uid_myear","UID", "year", "wdpa_id", "wdpa_id_2", "first_year", "disbursement_proj", "treatment_disb_duringproj", "treatment_disb", "disb_sqkm", "AREA_KM2", "year_standard", "strata", "area_total", "disbursement_sqkm", "disb_sqkm"))


cem_matched <-
  cem("treat_ever",
      as.data.frame(static.df),
      drop = c("sum_fcl_matchingyear_t3", "fc_area","fc_loss", "average_popgrowth", "travel_time_to_nearby_cities_min_20l_100mio", "treatment", "id", "poly_id", "wdpa_id", "bmz_nummer", "name", "left", "top", "right", "bottom",  "travel_time_to_nearby_cities_min_50k_100", "cem_weights", "uid_myear","UID", "year", "wdpa_id", "wdpa_id_2", "first_year", "disbursement_proj", "treatment_disb_duringproj", "treatment_disb", "disb_sqkm", "AREA_KM2", "year_standard", "strata", "area_total", "disbursement_sqkm", "disb_sqkm"),
      eval.imbalance = TRUE, cutpoints = cutoffs_list)
cem_matched$imbalance
cem_matched$tab


## ---- Create panel with only positive CEM weights ----
## retain the matching weights (which will be used later) and keep only the *exactly* matched samples (i.e., treatment & controll grids), based on the exact matching with the cem package
static.df$cem_weights <- cem_matched$w

static_matched.df <- static.df %>% 
  filter(cem_weights!=0)
table(static_matched.df$country, static_matched.df$treat_ever)
## merge weights with panel
temp.df <- static_matched.df %>% 
  select("uid_myear", "cem_weights")
panel_match.df <- merge(out.df, temp.df, by=c("uid_myear"))
panel_match.df <- pdata.frame(panel_match.df, index=c("uid_myear","year_standard"))
## Export data
write_csv(panel_match.df, paste0("../../datalake/mapme.protectedareas/output/tabular/regression_input/matched_panel_", i, ".csv"))
}



# ----- Plot difference in distribution before and after matching -----
test_before_matching <- static.df %>% 
  mutate(., 
         test_treat=case_when(treat_ever==0 ~ "No treatment",
                              TRUE ~ "Treatment"))

test_after_matching <- static.df %>% 
  filter(cem_weights!=0) %>%  
  mutate(., 
         test_treat=case_when(treat_ever==0 ~ "No treatment",
                              TRUE ~ "Treatment"))
### --- Forest cover ---
test_before_matching %>%
  ggplot( aes(x=fc_area_matchingyear, fill=test_treat)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = "identity") +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

test_after_matching %>%
  ggplot( aes(x=fc_area_matchingyear, fill=test_treat)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = "identity") +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

### --- Accessibility ---
test_before_matching %>%
  filter(travel_time_to_nearby_cities_min_5k_100mio<=10000) %>%
  ggplot( aes(x=fc_area_matchingyear, fill=test_treat)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = "identity") +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

test_after_matching %>%
  filter(travel_time_to_nearby_cities_min_5k_100mio<=10000) %>%
  ggplot( aes(x=fc_area_matchingyear, fill=test_treat)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = "identity") +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
