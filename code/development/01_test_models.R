############################################################################
#####                         KfW Schutzgebiete                        #####
############################################################################
#                                                                          #
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





### define panel
out2015.df <- read.csv("./output/tabular/regression_input/out2015_JS.csv")
panel.df <- pdata.frame(out2015.df, index=c("uid_myear","year_standard"))

### run models
m1 <- plm(loss ~ treatment_disb, data=panel.df, model=("within")) 
summary(m1)

m2<- plm(loss ~ treatment_disb + year, data=panel.df, model=("within")) 
summary(m2)

m3 <- plm(loss ~ disb_sqkm, data=panel.df, model=("within")) 
summary(m3)

m4<- plm(loss ~ disb_sqkm + year, data=panel.df, model=("within")) 
summary(m4)


### check table
stargazer(m1, m2, m3, m4 ,title="Pre-matching Results",type="text", align=TRUE)

# get heteroskedastic std. errors
library("sandwich")
m1.rob.se <- sqrt(diag(vcovHC(m1, type = "HC1")))
m2.rob.se <- sqrt(diag(vcovHC(m2, type = "HC1")))
m3.rob.se <- sqrt(diag(vcovHC(m3, type = "HC1")))
m4.rob.se <- sqrt(diag(vcovHC(m4, type = "HC1")))

se.list <- list(NULL, NULL, NULL, NULL,
                m1.rob.se, m2.rob.se, m3.rob.se, m4.rob.se)

# Compare the standard errors
# call function with align=TRUE for pretty output
stargazer(m1, m2, m3, m4, m1, m2, m3, m4,
          type = "text",
          se = se.list,
          dep.var.labels = "Forest cover loss",
          title = "Regression Table before matching",
          column.labels=c(rep("default",4),
                          rep("robust",4)),
          omit.stat = c("ser","f"),
          no.space = TRUE, align = TRUE)

se.list2 <- list(m1.rob.se, m2.rob.se, m3.rob.se, m4.rob.se)
# call function with align=TRUE for pretty output
stargazer(m1, m2, m3, m4,
          type = "text",
          se = se.list2,
          dep.var.labels = "Forest cover loss",
          title = "Regression Table before matching",
          omit.stat = c("ser","f"),
          no.space = TRUE, align = TRUE)



# missing


# ---- apply Coarse Exact Matching ----- Note MW Dec 12 2021: try matching on static dataset without panel structure
## create static dataset for matching
static.df <- panel.df %>% 
  subset(year==2015)

## check imbalance pre-matching
imbalance(
  static.df$treat_ever,
  as.data.frame(static.df),
  drop = c("treatment", "id", "poly_id", "wdpa_id", "bmz_nummer", "name", "left", "top", "right", "bottom",  "travel_time_to_nearby_cities_min_50k_100", "cem_weights", "uid_myear","UID", "year", "wdpa_id", "wdpa_id_2", "first_year", "disbursement_proj", "treatment_disb_duringproj",  "treat_ever", "treatment_disb", "disb_sqkm", "AREA_KM2", "year_standard", "strata", "area_total", "disbursement_sqkm", "disb_sqkm"))

## conduct CEM
cem_matched <-
  cem("treat_ever",
      as.data.frame(static.df),
      drop = c("treatment", "id", "poly_id", "wdpa_id", "bmz_nummer", "name", "left", "top", "right", "bottom",  "travel_time_to_nearby_cities_min_50k_100", "cem_weights", "uid_myear","UID", "year", "wdpa_id", "wdpa_id_2", "first_year", "disbursement_proj", "treatment_disb_duringproj", "treatment_disb", "disb_sqkm", "AREA_KM2", "year_standard", "strata", "area_total", "disbursement_sqkm", "disb_sqkm"),
      eval.imbalance = TRUE)

## check matched successes
cem_matched$tab
## check imbalance
cem_matched$imbalance
## retain the matching weights (which will be used later) and keep only the *exactly* matched samples (i.e., treatment & controll grids), based on the exact matching with the cem package
static.df$cem_weights <- cem_matched$w
## merge weights with panel
static_merge.df <- static.df %>% 
  select("UID", "cem_weights")
panel_match.df <- merge(panel.df, static_merge.df, by=c("UID"))
panel_match.df <- pdata.frame(panel_match.df, index=c("uid_myear","year_standard"))

## conduct CEM
cem_matched_panel <-
  cem("treat_ever",
      as.data.frame(panel_match.df),
      drop = c("uid_myear","UID", "year", "wdpa_id", "first_year", "disbursement_proj", "treat_ever", "treatment_disb", "disb_sqkm", "AREA_KM2", "year_standard", "strata" ),
      eval.imbalance = TRUE)
## check matched successes
cem_matched_panel$tab
## check imbalance
cem_matched_panel$imbalance



# match with panel data as test (do not match with panel data, otherwise it will drop units in between yers and we get a panel with gaps)
## check imbalance pre-matching
imbalance(
  panel.df$treat_ever,
  as.data.frame(panel.df),
  drop = c("uid_myear","UID", "year", "wdpa_id", "first_year", "disbursement_proj", "treat_ever", "treatment_disb", "disb_sqkm", "AREA_KM2", "year_standard", "strata" ))
## conduct CEM
cem_matched_panel1 <-
  cem("treat_ever",
      as.data.frame(panel.df),
      drop = c("uid_myear","UID", "year", "wdpa_id", "first_year", "disbursement_proj", "treat_ever", "treatment_disb", "disb_sqkm", "AREA_KM2", "year_standard", "strata" ),
      eval.imbalance = TRUE)
## check matched successes
cem_matched_panel1$tab
## check imbalance
cem_matched_panel1$imbalance




# ---- regression after matching-----

## run models
m1 <- plm(loss ~ treatment_disb, data=panel_match.df, model=("within"), weights = cem_weights) 
summary(m1)

m2<- plm(loss ~ treatment_disb + year, data=panel_match.df, model=("within"), weights = cem_weights) 
summary(m2)

m3 <- plm(loss ~ disb_sqkm, data=panel_match.df, model=("within"), weights = cem_weights) 
summary(m3)

m4<- plm(loss ~ disb_sqkm + year, data=panel_match.df, model=("within"), weights = cem_weights) 
summary(m4)

## get heteroskedastic std. errors
m1.rob.se <- sqrt(diag(vcovHC(m1, type = "HC1")))
m2.rob.se <- sqrt(diag(vcovHC(m2, type = "HC1")))
m3.rob.se <- sqrt(diag(vcovHC(m3, type = "HC1")))
m4.rob.se <- sqrt(diag(vcovHC(m4, type = "HC1")))
se.list2 <- list(m1.rob.se, m2.rob.se, m3.rob.se, m4.rob.se)

## call function with align=TRUE for pretty output
stargazer(m1, m2, m3, m4,
          type = "text",
          se = se.list2,
          dep.var.labels = "Forest cover loss",
          title = "Regression Table after matching",
          omit.stat = c("ser","f"),
          no.space = TRUE, align = TRUE)


stargazer(m1, m2, m3, m4,
          type = "text",
          dep.var.labels = "Forest cover loss",
          title = "Regression Table after matching",
          omit.stat = c("ser","f"),
          no.space = TRUE, align = TRUE)




# to do
- include forest cover and forest cover loss in matching from matching frame (time_invariant_vars$fcfc_area_matchingyear)
- create various descriptive statistics
- Add BMZ Nummers to uid




