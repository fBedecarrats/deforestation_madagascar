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
out2015.df <- read.csv("./output/tabular/regression_input/out2015.csv")
panel.df <- pdata.frame(out2015.df, index=c("uid_myear","year_standard"))

### run models
m1 <- plm(loss ~ treatment_disb, data=panel.df, model=("within")) 
summary(m1)

m2<- plm(loss ~ treatment_disb + year, data=panel.df, model=("within")) 
summary(m2)

m3 <- plm(loss ~ disbursement_proj, data=panel.df, model=("within")) 
summary(m3)

m4<- plm(loss ~ disbursement_proj + year, data=panel.df, model=("within")) 
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






# ---- apply Coarse Exact Matching -----

## check imbalance pre-matching
imbalance(
  panel.df$treat_ever,
  as.data.frame(panel.df),
  drop = c("uid_myear","UID", "year", "wdpa_id", "first_year", "disbursement_proj", "treat_ever", "treatment_disb", "disb_sqkm", "AREA_KM2", "year_standard", "strata" ))

## conduct CEM
cem_matched <-
  cem("treat_ever",
      as.data.frame(panel.df),
      drop = c("uid_myear","UID", "year", "wdpa_id", "first_year", "disbursement_proj", "treat_ever", "treatment_disb", "disb_sqkm", "AREA_KM2", "year_standard", "strata" ),
      eval.imbalance = TRUE)

## check matched successes
cem_matched$tab

## check imbalance
cem_matched$imbalance

## check variable breaks
cem_matched$breaks

cem_matched$w



## retain the matching weights (which will be used later) and keep only the *exactly* matched samples (i.e., treatment & controll grids), based on the exact matching with the cem package
panel.df$cem_weights <- cem_matched$w
panel_match.df <- panel.df[cem_matched$matched,] 





# ---- regression after matching-----

## run models
m1 <- plm(loss ~ treatment_disb, data=panel_match.df, model=("within"), weights = cem_weights) 
summary(m1)

m2<- plm(loss ~ treatment_disb + year, data=panel_match.df, model=("within"), weights = cem_weights) 
summary(m2)

m3 <- plm(loss ~ disbursement_proj, data=panel_match.df, model=("within"), weights = cem_weights) 
summary(m3)

m4<- plm(loss ~ disbursement_proj + year, data=panel_match.df, model=("within"), weights = cem_weights) 
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
