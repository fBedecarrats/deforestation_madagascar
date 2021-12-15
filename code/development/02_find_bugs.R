# Debug script
lop <- c("dplyr", "plm", "stargazer", "tidyverse", "cem", "ggplot2", "hrbrthemes")
newp <- lop[!(lop %in% installed.packages()[,"Package"])]
if(length(newp)) install.packages(newp)
lapply(lop, require, character.only = TRUE)

# set working directory
setwd("~/shared/datalake/mapme.protectedareas")




time_invariant_vars <- 
  read_csv("../../datalake/mapme.protectedareas/output/matching/matching_frames/matching_frame_2015.csv")

### define panel
out2015.df <- read.csv("./output/tabular/regression_input/out2015_JS.csv")
panel.df <- pdata.frame(out2015.df, index=c("uid_myear","year_standard"))
static.df <- panel.df %>% 
  subset(year==2015)



#### check differences between sample and population
out2015_sample.df <- read.csv("./output/tabular/regression_input/out2015.csv")


boxplot(out2015_sample.df$travel_time_to_nearby_cities_min_5k_10k)
boxplot(out2015.df$travel_time_to_nearby_cities_min_5k_100mio)


summary(out2015_sample.df$travel_time_to_nearby_cities_min_5k_10k)
summary(out2015.df$travel_time_to_nearby_cities_min_5k_100mio)
#### there are many NAs
# check for NAs
fcl_reshaped %>%
  left_join(.,time_invariant_vars,
            by=c("poly_id"),
            suffix=c("","_delete"))

## check why poly_id== "id_1" and poly_id== "id_10" are NA 
unique(time_invariant_vars$poly_id) # they do not exist in time_invariant df. poly_id is also not a complete running number. e.g. id_88 and id_89 do not exist either


## check distribution of travel time
test <- as.data.frame(static.df) %>% 
  mutate(., 
         test_treat=case_when(treat_ever==0 ~ "No treatment",
                                    TRUE ~ "Treatment"))

test %>%
  ggplot( aes(x=elevation_mean, fill=test_treat)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = "identity") +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


test %>%
  filter(travel_time_to_nearby_cities_min_5k_100mio<=10000) %>%
  ggplot( aes(x=travel_time_to_nearby_cities_min_5k_100mio, fill=test_treat)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = "identity") +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

test %>%
  ggplot( aes(x=fc_area_matchingyear, fill=test_treat)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = "identity") +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")


## check CEM
imbalance(
  static.df$treat_ever,
  as.data.frame(static.df),
  drop = c("travel_time_to_nearby_cities_min_20l_100mio", "treatment", "id", "poly_id", "wdpa_id", "bmz_nummer", "name", "left", "top", "right", "bottom",  "travel_time_to_nearby_cities_min_50k_100", "cem_weights", "uid_myear","UID", "year", "wdpa_id", "wdpa_id_2", "first_year", "disbursement_proj", "treatment_disb_duringproj",  "treat_ever", "treatment_disb", "disb_sqkm", "AREA_KM2", "year_standard", "strata", "area_total", "disbursement_sqkm", "disb_sqkm"))

## conduct CEM
cem_matched <-
  cem("treat_ever",
      as.data.frame(static.df),
      drop = c("average_popgrowth", "elevation_mean", "clay_content_10_cm", "travel_time_to_nearby_cities_min_20l_100mio", "treatment", "id", "poly_id", "wdpa_id", "bmz_nummer", "name", "left", "top", "right", "bottom",  "travel_time_to_nearby_cities_min_50k_100", "cem_weights", "uid_myear","UID", "year", "wdpa_id", "wdpa_id_2", "first_year", "disbursement_proj", "treatment_disb_duringproj", "treatment_disb", "disb_sqkm", "AREA_KM2", "year_standard", "strata", "area_total", "disbursement_sqkm", "disb_sqkm"),
      eval.imbalance = TRUE)




# create list for manual cutoff points
cutoffs_list<-
  cem_matched$breaks

# table default cutoffs CEM
table(cut(static.df$travel_time_to_nearby_cities_min_5k_100mio,
          cem_matched$breaks$travel_time_to_nearby_cities_min_5k_100mio),
      static.df$treat_ever)

## Travel time
# define proposed cuttoffs
proposed_cutoffs_traveltime<-
  c(0,120,300,max(static.df$travel_time_to_nearby_cities_min_5k_100mio,na.rm = T)) # 0-2hrs, 2-6 hrs, >6 hrs

table(cut(static.df$travel_time_to_nearby_cities_min_5k_100mio,
          proposed_cutoffs_traveltime),
      static.df$treat_ever)
# change list to proposed cutoffs
cutoffs_list$travel_time_to_nearby_cities_min_5k_100mio<- 
  proposed_cutoffs_traveltime
## ruggedness
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

## 4) ---- Elevation ---- 
# we simplify by creating three different altitude groups. Lowland 0-500, midland 500-1500, highland >1500
# define proposed cuttoffs
proposed_cutoffs_elevation<-
  c(0,500,1500,max(static.df$elevation_mean,na.rm = T))
#  assign new cutoff points
cutoffs_list$elevation_mean<-proposed_cutoffs_elevation

## 6) ---- Forest Cover Loss ----
# proposed simplified cutoffs: forest cover loss yes and no. 
# define proposed cuttoffs
proposed_cutoffs_fcl<-
  c(-1,0,max(static.df$average_fcl_matchingyear))
#  assign new cutoff points
cutoffs_list$average_fcl_matchingyear<-proposed_cutoffs_fcl



















cem_matched_test <-
  cem("treat_ever",
      as.data.frame(static.df),
      drop = c("average_popgrowth", "travel_time_to_nearby_cities_min_20l_100mio", "treatment", "id", "poly_id", "wdpa_id", "bmz_nummer", "name", "left", "top", "right", "bottom",  "travel_time_to_nearby_cities_min_50k_100", "cem_weights", "uid_myear","UID", "year", "wdpa_id", "wdpa_id_2", "first_year", "disbursement_proj", "treatment_disb_duringproj", "treatment_disb", "disb_sqkm", "AREA_KM2", "year_standard", "strata", "area_total", "disbursement_sqkm", "disb_sqkm"),
      eval.imbalance = TRUE, cutpoints = cutoffs_list)
cem_matched_test$imbalance
cem_matched_test$tab

## check matched successes
cem_matched$tab
## check imbalance
cem_matched$imbalance
## retain the matching weights (which will be used later) and keep only the *exactly* matched samples (i.e., treatment & controll grids), based on the exact matching with the cem package
static.df$cem_weights <- cem_matched_test$w

static_cem.df <- static.df %>% 
  filter(cem_weights!=0)
table(static_cem.df$country, static_cem.df$treat_ever)
## merge weights with panel
static_merge.df <- static_cem.df %>% 
  select("uid_myear", "cem_weights")
panel_match.df <- merge(panel.df, static_merge.df, by=c("uid_myear"))
panel_match.df <- pdata.frame(panel_match.df, index=c("uid_myear","year_standard"))




static.df <- as.data.frame(static.df) %>% 
  mutate(., 
         test_treat=case_when(treat_ever==0 ~ "No treatment",
                              TRUE ~ "Treatment"))

static_cem.df <- static.df %>% 
  filter(cem_weights!=0)

static_cem.df %>%
  filter(travel_time_to_nearby_cities_min_5k_100mio<=10000) %>%
  ggplot( aes(x=travel_time_to_nearby_cities_min_5k_100mio, fill=test_treat)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = "identity") +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

  
static_cem.df %>%
  ggplot( aes(x=fc_area_matchingyear, fill=test_treat)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = "identity") +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")



## run models
## merge weights with panel
static_merge.df <- static.df %>% 
  select("uid_myear", "cem_weights")
panel_match.df <- merge(panel.df, static_merge.df, by=c("uid_myear"))
panel_match.df <- pdata.frame(panel_match.df, index=c("uid_myear","year_standard"))


m1 <- plm(fc_area_matchingyear ~ treatment_disb, data=panel_match.df, model=("within"), weights = cem_weights) 
summary(m1)

