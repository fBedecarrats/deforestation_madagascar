library("cem")
library("ggplot2")
library("readr")
library("tidyverse")

setwd("/datadrive/melvin/mapme.protectedareas")
# ----- get data -----
# load individual matching frames
   # note: those contain samples AOIs from 2002-2020 for supported PAs and non-protected areas. 

# test for 2015
matching_db<-
  read.csv("../../datalake/mapme.protectedareas/output/matching/matching_frames/matching_frame_2015.csv") 

## drop obs if any matching variable is NA
is.na(matching_db)

# get all data
matching_db_list<-
  lapply(list.files("../../datalake/mapme.protectedareas/output/matching/matching_frames/"), function(x) {
    read_csv(paste(
      "../../datalake/mapme.protectedareas/output/matching/matching_frames/",
      x,
      sep = ""
    ))
  })


# ---- apply Coarse Exact Matching -----

imbalance(
  matching_db$treatment,
  as.data.frame(matching_db),
  drop = c("treatment","UID"))

results_matching_PAs <-
  cem("treatment",
      as.data.frame(matching_db),
      drop =  c("treatment","UID","biome_max"),
      eval.imbalance = TRUE)

# check matched successes
results_matching_PAs$tab

# check imbalance
results_matching_PAs$imbalance

# check variable breaks
results_matching_PAs$breaks

# ----- adjust breaks -----
# 1. Travel time: travel_time_to_nearby_cities_min_5k_10k
ggplot(data = matching_db)+
  geom_histogram(aes(travel_time_to_nearby_cities_min_5k_10k))

results_matching_PAs$breaks$travel_time_to_nearby_cities_min_5k_10k
# suggeste breaks 0-120 min, 120-300 min, 300 - 1440, > 1440



# ----- use relax function -----
# define groups of countries manually
country.grp <- list(c("Belize"), c("Bolivia"), c("Brazil" ), c("Colombia"), c("Costa Rica"), 
                    c("Dominican Republic"), c("Ecuador"), c("El Salvador"), c("Guatemala"), c("Guyana"), 
                    c("Honduras"), c("Mexico"), c("Nicaragua"), c("Panama"), c("Peru"))

results_matching_PAs <-
  cem("treatment",
      as.data.frame(matching_db),
      drop =  c("treatment","UID","biome_max"),
      eval.imbalance = TRUE,
      keep.all = T,
      grouping = list(country.grp))

# check imbalance
results_matching_PAs$imbalance

relax.cem(results_matching_PAs,
          as.data.frame(matching_db),
          depth = 2,
          fixed=("country"))
