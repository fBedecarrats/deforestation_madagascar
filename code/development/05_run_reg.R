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
m1 <- plm(fc_loss ~ treatment_disb + factor(year), data=panel.df, model=("within")) 
summary(m1)

m2<- plm(fc_loss ~ treatment_disb + factor(year), data=panel.df, model=("within")) 
summary(m2)

m3 <- plm(fc_area ~ treatment_disb+ factor(year), data=panel.df, model=("within")) 
summary(m3)

m4<- plm(fc_area ~ treatment_disb + factor(year), data=panel.df, model=("within")) 
summary(m4)


# get heteroskedastic std. errors
library("sandwich")
m1.rob.se <- sqrt(diag(vcovHC(m1, type = "HC1")))
m2.rob.se <- sqrt(diag(vcovHC(m2, type = "sss", cluster = "group")))
m3.rob.se <- sqrt(diag(vcovHC(m3, type = "HC1")))
m4.rob.se <- sqrt(diag(vcovHC(m4, type = "sss", cluster = "group")))

se.list <- list(m1.rob.se, m2.rob.se, m3.rob.se, m4.rob.se)
### check table
stargazer(m1, m2, m3, m4,
          type = "text",
          title="Post-matching Results",
          se = se.list,
          column.labels=c(rep("fc_loss",2),
                          rep("fc_area",2)),
          omit.stat = c("ser","f"),
          no.space = TRUE, align = TRUE,
          out = "./output/tabular/regression_output/reg_single2015_post_test_cluster.html")




## ---- run models pre matching ----
out2015_preCEM.df <- read.csv("./output/tabular/regression_input/out2015.csv")
panel_preCEM.df <- pdata.frame(out2015_preCEM.df, index=c("uid_myear","year_standard"))



m1_pre <- plm(fc_loss ~ treatment_disb + year, data=panel_preCEM.df, model=("within")) 
summary(m1_pre)

m2_pre<- plm(fc_loss ~ treatment_disb + year + factor(year), data=panel_preCEM.df, model=("within")) 
summary(m2_pre)

m3_pre <- plm(fc_area ~ treatment_disb + year, data=panel_preCEM.df, model=("within")) 
summary(m3_pre)

m4_pre <- plm(fc_area ~ treatment_disb + year + factor(year), data=panel_preCEM.df, model=("within")) 
summary(m4_pre)

# # get heteroskedastic std. errors
# library("sandwich")
# m1.rob.se_pre <- sqrt(diag(vcovHC(m1_pre, type = "HC1")))
# m2.rob.se_pre <- sqrt(diag(vcovHC(m2_pre, type = "HC1")))
# m3.rob.se_pre <- sqrt(diag(vcovHC(m3_pre, type = "HC1")))
# m4.rob.se_pre <- sqrt(diag(vcovHC(m4_pre, type = "HC1")))
# 
# se.list <- list(m1.rob.se, m2.rob.se, m3.rob.se, m4.rob.se)
### check table
stargazer(m1_pre, m2_pre, m3_pre, m4_pre,
          type = "text",
          title="Pre-matching Results",
          column.labels=c(rep("fc_loss",2),
                          rep("fc_area",2)),
          omit.stat = c("ser","f"),
          no.space = TRUE, align = TRUE,
          out = "./output/tabular/regression_output/reg_single2015_pre.html")








# Run regression for all years (fc_area)
library("sandwich")
T_year <- c(2004, 2006:2013, 2015, 2016, 2019)
# problem with 2005
#T_year <- c(2004:2005)
for (t in T_year) {
  print(t)
  ### define panel
  assign(paste0("out", t, ".df"), read.csv(paste0("./output/tabular/regression_input/matched_panel_", t, ".csv")))
  assign(paste0("panel", t, ".df"), pdata.frame(get(paste0("out", t, ".df")), index=c("uid_myear","year_standard")))
  ###  ---- run models post matching ----
  assign(paste0("m",t), plm(fc_area ~ treatment_disb + year + factor(year), data=get(paste0("panel", t, ".df")), model=("within")) )
  ### get heteroskedastic std. errors
  assign(paste0("m", t, ".rob.se"), sqrt(diag(vcovHC(get(paste0("m",t)), type = "HC1"))))

}
se.list <- list(m2004.rob.se, m2006.rob.se, m2007.rob.se, m2008.rob.se
                , m2009.rob.se, m2010.rob.se, m2011.rob.se , m2012.rob.se, m2013.rob.se
                , m2015.rob.se, m2016.rob.se, m2019.rob.se)
### check table
stargazer(m2004, m2006, m2007, m2008, m2009, m2010, m2011, m2012, m2013, m2015, m2016, m2019,
          type = "text",
          title="Post-matching Results",
          se = se.list,
          omit = "year",
          omit.stat = c("ser","f"),
          no.space = TRUE, align = TRUE,
          notes = "Dependent variable: Forest cover, unit of observation: cell-level, Fixed-effect regression include year trends and time FE.",
          column.labels = as.character(c(2004, 2006:2013, 2015, 2016, 2019)),
          out = "./output/tabular/regression_output/reg_all_post_fc.html")


# Run regression for all years (fc_loss)
library("sandwich")
T_year <- c(2004, 2006:2013, 2015, 2016, 2019)
# problem with 2005
#T_year <- c(2004:2005)
for (t in T_year) {
  print(t)
  ### define panel
  assign(paste0("out", t, ".df"), read.csv(paste0("./output/tabular/regression_input/matched_panel_", t, ".csv")))
  assign(paste0("panel", t, ".df"), pdata.frame(get(paste0("out", t, ".df")), index=c("uid_myear","year_standard")))
  ###  ---- run models post matching ----
  assign(paste0("m",t), plm(fc_loss ~ treatment_disb + year + factor(year), data=get(paste0("panel", t, ".df")), model=("within")) )
  ### get heteroskedastic std. errors
  assign(paste0("m", t, ".rob.se"), sqrt(diag(vcovHC(get(paste0("m",t)), type = "HC1"))))
  
}
se.list <- list(m2004.rob.se, m2006.rob.se, m2007.rob.se, m2008.rob.se
                , m2009.rob.se, m2010.rob.se, m2011.rob.se , m2012.rob.se, m2013.rob.se
                , m2015.rob.se, m2016.rob.se, m2019.rob.se)
### check table
stargazer(m2004, m2006, m2007, m2008, m2009, m2010, m2011, m2012, m2013, m2015, m2016, m2019,
          type = "text",
          title="Post-matching Results",
          se = se.list,
          omit = "year",
          omit.stat = c("ser","f"),
          no.space = TRUE, align = TRUE,
          notes = "Dependent variable: Forest cover, unit of observation: cell-level, Fixed-effect regression include year trends and time FE.",
          column.labels = as.character(c(2004, 2006:2013, 2015, 2016, 2019)),
          out = "./output/tabular/regression_output/reg_all_post_fcloss.html")




# Run regression for all years (fc_loss)
library("sandwich")
T_year <- c(2004, 2006:2013, 2015, 2016, 2019)
# problem with 2005
#T_year <- c(2004:2005)
for (t in T_year) {
  print(t)
  ### define panel
  assign(paste0("out", t, ".df"), read.csv(paste0("./output/tabular/regression_input/matched_panel_", t, ".csv")))
  assign(paste0("panel", t, ".df"), pdata.frame(get(paste0("out", t, ".df")), index=c("uid_myear","year_standard")))
  ###  ---- run models post matching ----
  assign(paste0("m",t), plm(fc_loss ~ treatment_disb + year + factor(year), data=get(paste0("panel", t, ".df")), model=("within")) )
  ### get heteroskedastic std. errors
  assign(paste0("m", t, ".rob.se"), sqrt(diag(vcovHC(get(paste0("m",t)), type = "HC1"))))
  
}
se.list <- list(m2004.rob.se, m2006.rob.se, m2007.rob.se, m2008.rob.se
                , m2009.rob.se, m2010.rob.se, m2011.rob.se , m2012.rob.se, m2013.rob.se
                , m2015.rob.se, m2016.rob.se, m2019.rob.se)
### check table
stargazer(m2004, m2006, m2007, m2008, m2009, m2010, m2011, m2012, m2013, m2015, m2016, m2019,
          type = "text",
          title="Post-matching Results",
          se = se.list,
          omit = "year",
          omit.stat = c("ser","f"),
          no.space = TRUE, align = TRUE,
          notes = "Dependent variable: Forest cover, unit of observation: cell-level, Fixed-effect regression include year trends and time FE.",
          column.labels = as.character(c(2004, 2006:2013, 2015, 2016, 2019)),
          out = "./output/tabular/regression_output/reg_all_post_fcloss.html")





# Append all datasets and run regression for all matching frames

out2004.df <- read.csv("./output/tabular/regression_input/matched_panel_2004.csv")
T_year <- c(2006:2013, 2015, 2016, 2019)
for (t in T_year) {
  print(t)
  ### define panel
  x.df <- read.csv(paste0("./output/tabular/regression_input/matched_panel_", t, ".csv"))
  out2004.df <- rbind(out2004.df, x.df)
  
  
}
panel.df <- pdata.frame(out2004.df, index=c("uid_myear","year_standard"))

m1 <- plm(fc_loss ~ treatment_disb + year, data=panel.df, model=("within")) 
m2 <- plm(fc_loss ~ treatment_disb + year + factor(year), data=panel.df, model=("within")) 
m3 <- plm(fc_area ~ treatment_disb + year, data=panel.df, model=("within")) 
m4 <- plm(fc_area ~ treatment_disb + year + factor(year), data=panel.df, model=("within")) 
### check table
stargazer(m1, m2, m3, m4,
          type = "text",
          title="Post-matching Results",
          column.labels=c(rep("fc_loss",2),
                          rep("fc_area",2)),
          omit.stat = c("ser","f"),
          no.space = TRUE, align = TRUE,
          out = "./output/tabular/regression_output/reg_all_matchingframes_post.html")




