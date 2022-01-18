#####                         KfW Schutzgebiete                        #####                                                                          #
# Authors: Melvin Wong                                                     #
############################################################################

#### ---- clean workspace, set options -----
rm(list=ls())
options(scipen = 999)
# get packages
lop <- c("dplyr", "plm", "stargazer", "tidyverse", "cem")
newp <- lop[!(lop %in% installed.packages()[,"Package"])]
if(length(newp)) install.packages(newp)
lapply(lop, require, character.only = TRUE)

# set working directory
setwd("~/shared/datalake/mapme.protectedareas")





### define panel
out2015.df <- read.csv("./output/tabular/regression_input/matched_panel_2015.csv")
panel.df <- pdata.frame(out2015.df, index=c("uid_myear","year_standard"))


###  ---- run models post matching ----
m1 <- plm(fc_loss ~ treatment_disb + year, data=panel.df, model=("within")) 
summary(m1)

m2<- plm(fc_loss ~ treatment_disb + year + factor(year), data=panel.df, model=("within")) 
summary(m2)

m3 <- plm(fc_area ~ treatment_disb + year, data=panel.df, model=("within")) 
summary(m3)

m4<- plm(fc_area ~ treatment_disb + year + factor(year), data=panel.df, model=("within")) 
summary(m4)


# get heteroskedastic std. errors
library("sandwich")
m1.rob.se <- sqrt(diag(vcovHC(m1, type = "HC1")))
m2.rob.se <- sqrt(diag(vcovHC(m2, type = "HC1")))
m3.rob.se <- sqrt(diag(vcovHC(m3, type = "HC1")))
m4.rob.se <- sqrt(diag(vcovHC(m4, type = "HC1")))

se.list <- list(m1.rob.se, m2.rob.se, m3.rob.se, m4.rob.se)
### check table
stargazer(m1, m2, m3, m4,
          type = "text",
          title="Post-matching Results",
          se = se.list,
          column.labels=c(rep("fc_loss",2),
                          rep("fc_area",2)),
          omit.stat = c("ser","f"),
          no.space = TRUE, align = TRUE)




## ---- run models pre matching ----
out2015_preCEM.df <- read.csv("./output/tabular/regression_input/out2015.csv")
panel_preCEM.df <- pdata.frame(out2015_preCEM.df, index=c("uid_myear","year_standard"))



m1_pre <- plm(fc_loss ~ treatment_disb, data=panel_preCEM.df, model=("within")) 
summary(m1_pre)

m2_pre<- plm(fc_loss ~ treatment_disb + year, data=panel_preCEM.df, model=("within")) 
summary(m2_pre)

m3_pre <- plm(fc_area ~ treatment_disb, data=panel_preCEM.df, model=("within")) 
summary(m3_pre)

m4_pre <- plm(fc_area ~ treatment_disb + year, data=panel_preCEM.df, model=("within")) 
summary(m4_pre)

# get heteroskedastic std. errors
library("sandwich")
m1.rob.se_pre <- sqrt(diag(vcovHC(m1_pre, type = "HC1")))
m2.rob.se_pre <- sqrt(diag(vcovHC(m2_pre, type = "HC1")))
m3.rob.se_pre <- sqrt(diag(vcovHC(m3_pre, type = "HC1")))
m4.rob.se_pre <- sqrt(diag(vcovHC(m4_pre, type = "HC1")))

se.list <- list(m1.rob.se, m2.rob.se, m3.rob.se, m4.rob.se)
### check table
stargazer(m1_pre, m2_pre, m3_pre, m4_pre,
          type = "text",
          title="Pre-matching Results",
          se = se.list,
          column.labels=c(rep("fc_loss",2),
                          rep("fc_area",2)),
          omit.stat = c("ser","f"),
          no.space = TRUE, align = TRUE)
