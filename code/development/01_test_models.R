############################################################################
#####                         KfW Schutzgebiete                        #####
############################################################################
#                                                                          #
# Authors: Melvin Wong                                                     #
############################################################################
# clean workspace, set options
rm(list=ls())

# get packages
lop <- c("dplyr", "plm", "stargazer", "tidyverse")
newp <- lop[!(lop %in% installed.packages()[,"Package"])]
if(length(newp)) install.packages(newp)
lapply(lop, require, character.only = TRUE)

# set working directory
setwd("~/shared/datalake/mapme.protectedareas")





### define panel
panel.df <- pdata.frame(Conflict_data, index=c("UID","year"))

### run models
m1 <- plm(loss~treatment, data=panel.df, model=("within")) 
summary(m1)


### check table
stargazer(m1,m2,m3 ,title="Results",type="text", align=TRUE)

# get heteroskedastic std. errors
library("sandwich")
m1.rob.se <- sqrt(diag(vcovHC(m1, type = "HC1")))
m2.rob.se <- sqrt(diag(vcovHC(m2, type = "HC1")))
m3.rob.se <- sqrt(diag(vcovHC(m3, type = "HC1")))

se.list <- list(NULL, NULL, NULL,
                m1.rob.se, m2.rob.se, m3.rob.se)

# Compare the standard errors
# call function with align=TRUE for pretty output
stargazer(m1, m2, m3, m1, m2, m3,
          type = "text",
          se = se.list,
          dep.var.labels = "Conflict",
          title = "Regression Table",
          column.labels=c(rep("default",3),
                          rep("robust",3)),
          omit.stat = c("ser","f"),
          no.space = TRUE, align = TRUE)

se.list2 <- list(m1.rob.se, m2.rob.se, m3.rob.se)
# call function with align=TRUE for pretty output
stargazer(m1, m2, m3,
          type = "text",
          se = se.list,
          dep.var.labels = "Conflict",
          title = "Regression Table",
          column.labels=c(rep("default",3),
                          rep("robust",3)),
          omit.stat = c("ser","f"),
          no.space = TRUE, align = TRUE)