############################################################################
#####                         KfW Schutzgebiete                        #####
############################################################################
                                                    #
############################################################################
# clean workspace, set options
rm(list=ls())

# get packages
lop <- c("dplyr", "tidyverse", "ggplot2")
newp <- lop[!(lop %in% installed.packages()[,"Package"])]
if(length(newp)) install.packages(newp)
lapply(lop, require, character.only = TRUE)

# set working directory
setwd("~/shared/datalake/mapme.protectedareas")

data_all <- 
  read.csv("../../datalake/mapme.protectedareas/output/polygon/sampling/fishnets/dec_08/gfw/gfw_10km_all.csv")

area_all <-
  data_all %>% 
  select(poly_id,
         starts_with("area")) %>% 
  pivot_longer(.,
               !poly_id,
               names_to = "name",
               values_to = "value"
  ) %>% 
  separate(name, c(NA,"year",NA), sep="_") %>% 
  mutate(year = as.numeric(year))




# Plot only 2015 pre and post matching with same axis
i=2015

### fc_area (pre)
out <- read.csv(paste0("../../datalake/mapme.protectedareas/output/tabular/regression_input/out", i, ".csv"))

out_gfw <- 
  left_join(out, area_all,
            by=c("poly_id","year"))

area_projstart <- out_gfw %>%
  filter(year_standard == 0) %>%
  select(uid_myear,
         value) %>%
  rename("value_projstart" = value)

out_gfw_rel <- left_join(out_gfw, area_projstart,
                         by=c("uid_myear")) %>%
  mutate(area_pct_projstart = value/value_projstart)

plot_fc_area_pre <- out_gfw_rel %>%
  filter(value_projstart!=0) %>% #what kind of data is dropped here?
  group_by(treat_ever, year_standard) %>%
  summarise(avg_fc_area = mean(fc_area, na.rm=T)) %>%
  ggplot(aes(x=year_standard, y=avg_fc_area, col=as.factor(treat_ever))) +
  geom_vline(xintercept = 0) +
  geom_line(aes(group=as.factor(treat_ever))) +
  geom_point(aes(size=3), show.legend = F) +
  labs(title=paste("Matching Frame", i, "(Not Matched)"),
       x ="Project Years", y = "Forest cover area") +
  theme(legend.position = "bottom")  +
  xlim(c(-12,6)) +
  ylim(c(3800, 8400)) +
  scale_colour_discrete(name= "", breaks=c("0", "1"),
                        labels=c("Non-Protected Forest  Areas", "Protected Forest Areas"))
ggsave(paste0("plot", i, "_fcarea_pre.png"), plot = plot_fc_area_pre, path = "../../datalake/mapme.protectedareas/output/plots/parallel_trends_tmax/same_axis")

### fc_area (post)
mp <- read.csv(paste0("../../datalake/mapme.protectedareas/output/tabular/regression_input/matched_panel_", i, ".csv"))

mp_gfw <- 
  left_join(mp, area_all,
            by=c("poly_id","year"))

area_mp_projstart <- mp_gfw %>%
  filter(year_standard == 0) %>%
  select(uid_myear,
         value) %>%
  rename("value_projstart" = value)

mp_gfw_rel <- left_join(mp_gfw, area_mp_projstart,
                        by=c("uid_myear")) %>%
  mutate(area_pct_projstart = value/value_projstart)

plot_fc_area_post <- mp_gfw_rel %>%
  filter(value_projstart!=0) %>% #what kind of data is dropped here?
  group_by(treat_ever, year_standard) %>%
  summarise(avg_fc_area = mean(fc_area, na.rm=T)) %>%
  ggplot(aes(x=year_standard, y=avg_fc_area, col=as.factor(treat_ever))) +
  geom_vline(xintercept = 0) +
  geom_line(aes(group=as.factor(treat_ever))) +
  geom_point(aes(size=3), show.legend = F) +
  labs(title=paste("Matching Frame", i, "(Matched data)"),
       x ="Project Years", y = "Forest cover area") +
  theme(legend.position = "bottom") +
  xlim(c(-12,6)) +
  ylim(c(3800, 8400)) +
  scale_colour_discrete(name= "", breaks=c("0", "1"),
                        labels=c("Non-Protected Forest  Areas", "Protected Forest Areas"))
ggsave(paste0("plot", i, "_fcarea_post.png"), plot = plot_fc_area_post, path = "../../datalake/mapme.protectedareas/output/plots/parallel_trends_tmax/same_axis")

somePDFPath = "../../datalake/mapme.protectedareas/output/plots/parallel_trends_tmax/same_axis/2015_fcarea.pdf"
pdf(file=somePDFPath)  
plot(plot_fc_area_pre)
plot(plot_fc_area_post)
dev.off() 





# Plot all
somePDFPath = "../../datalake/mapme.protectedareas/output/plots/parallel_trends_tmax/all_plots.pdf"
pdf(file=somePDFPath)  


T_year <- c(2004:2013, 2015, 2016, 2019)
#T_year <- c(2004)
for (i in T_year) {
  print(i)
# Before matching

out <- read.csv(paste0("../../datalake/mapme.protectedareas/output/tabular/regression_input/out", i, ".csv"))

out_gfw <- 
  left_join(out, area_all,
                          by=c("poly_id","year"))

area_projstart <- out_gfw %>%
  filter(year_standard == 0) %>%
  select(uid_myear,
         value) %>%
  rename("value_projstart" = value)

out_gfw_rel <- left_join(out_gfw, area_projstart,
                            by=c("uid_myear")) %>%
  mutate(area_pct_projstart = value/value_projstart)

### percent forest
plot_pctforest_pre <- out_gfw_rel %>%
  filter(value_projstart!=0) %>% #what kind of data is dropped here?
  group_by(treat_ever, year_standard) %>%
  summarise(avg_areapct = mean(area_pct_projstart, na.rm=T)) %>%
  ggplot(aes(x=year_standard, y=avg_areapct, col=as.factor(treat_ever))) +
  geom_vline(xintercept = 0) +
  geom_line(aes(group=as.factor(treat_ever))) +
  geom_point(aes(size=3), show.legend = F) +
  labs(title=paste("Matching Frame", i, "(Not Matched)"),
       x ="Project Years", y = "Relative forest cover")+
  theme(legend.position = "bottom") +
  scale_colour_discrete(name= "", breaks=c("0", "1"),
                        labels=c("Non-Protected Forest  Areas", "Protected Forest Areas"))
plot_pctforest_pre


ggsave(paste0("plot", i, "_pctforest_pre.png"), plot = plot_pctforest_pre, path = "../../datalake/mapme.protectedareas/output/plots/parallel_trends_tmax")


### fc_area
plot_fc_area_pre <- out_gfw_rel %>%
  filter(value_projstart!=0) %>% #what kind of data is dropped here?
  group_by(treat_ever, year_standard) %>%
  summarise(avg_fc_area = mean(fc_area, na.rm=T)) %>%
  ggplot(aes(x=year_standard, y=avg_fc_area, col=as.factor(treat_ever))) +
  geom_vline(xintercept = 0) +
  geom_line(aes(group=as.factor(treat_ever))) +
  geom_point(aes(size=3), show.legend = F) +
  labs(title=paste("Matching Frame", i, "(Not Matched)"),
       x ="Project Years", y = "Forest cover area") +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name= "", breaks=c("0", "1"),
                        labels=c("Non-Protected Forest  Areas", "Protected Forest Areas"))
ggsave(paste0("plot", i, "_fcarea_pre.png"), plot = plot_fc_area_pre, path = "../../datalake/mapme.protectedareas/output/plots/parallel_trends_tmax")


### fc_loss
plot_fc_loss_pre <- out_gfw_rel %>%
  filter(value_projstart!=0) %>% #what kind of data is dropped here?
  group_by(treat_ever, year_standard) %>%
  summarise(avg_fc_loss = mean(fc_loss, na.rm=T)) %>%
  ggplot(aes(x=year_standard, y=avg_fc_loss, col=as.factor(treat_ever))) +
  geom_vline(xintercept = 0) +
  geom_line(aes(group=as.factor(treat_ever))) +
  geom_point(aes(size=3), show.legend = F) +
  labs(title=paste("Matching Frame", i, "(Not Matched)"),
       x ="Project Years", y = "Forest cover loss") +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name= "", breaks=c("0", "1"),
                        labels=c("Non-Protected Forest  Areas", "Protected Forest Areas"))
ggsave(paste0("plot", i, "_fcloss_pre.png"), plot = plot_fc_loss_pre, path = "../../datalake/mapme.protectedareas/output/plots/parallel_trends_tmax")



# After matching
mp <- read.csv(paste0("../../datalake/mapme.protectedareas/output/tabular/regression_input/matched_panel_", i, ".csv"))

mp_gfw <- 
  left_join(mp, area_all,
            by=c("poly_id","year"))

area_mp_projstart <- mp_gfw %>%
  filter(year_standard == 0) %>%
  select(uid_myear,
         value) %>%
  rename("value_projstart" = value)

mp_gfw_rel <- left_join(mp_gfw, area_mp_projstart,
                              by=c("uid_myear")) %>%
  mutate(area_pct_projstart = value/value_projstart)

### percent forest
plot_pctforest_post <- mp_gfw_rel %>%
  filter(value_projstart!=0) %>% #what kind of data is dropped here?
  group_by(treat_ever, year_standard) %>%
  summarise(avg_areapct = mean(area_pct_projstart, na.rm=T)) %>%
  ggplot(aes(x=year_standard, y=avg_areapct, col=as.factor(treat_ever))) +
  geom_vline(xintercept = 0) +
  geom_line(aes(group=as.factor(treat_ever))) +
  geom_point(aes(size=3), show.legend = F) +
  labs(title=paste("Matching Frame", i, "(Matched data)"),
       x ="Project Years", y = "Relative forest cover") +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name= "", breaks=c("0", "1"),
                        labels=c("Non-Protected Forest  Areas", "Protected Forest Areas"))
ggsave(paste0("plot", i, "_pctforest_post.png"), plot = plot_pctforest_post, path = "../../datalake/mapme.protectedareas/output/plots/parallel_trends_tmax")

### fc_area
plot_fc_area_post <- mp_gfw_rel %>%
  filter(value_projstart!=0) %>% #what kind of data is dropped here?
  group_by(treat_ever, year_standard) %>%
  summarise(avg_fc_area = mean(fc_area, na.rm=T)) %>%
  ggplot(aes(x=year_standard, y=avg_fc_area, col=as.factor(treat_ever))) +
  geom_vline(xintercept = 0) +
  geom_line(aes(group=as.factor(treat_ever))) +
  geom_point(aes(size=3), show.legend = F) +
  labs(title=paste("Matching Frame", i, "(Matched data)"),
       x ="Project Years", y = "Forest cover area") +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name= "", breaks=c("0", "1"),
                        labels=c("Non-Protected Forest  Areas", "Protected Forest Areas"))
ggsave(paste0("plot", i, "_fcarea_post.png"), plot = plot_fc_area_post, path = "../../datalake/mapme.protectedareas/output/plots/parallel_trends_tmax")

### fc_loss
plot_fc_loss_post <- mp_gfw_rel %>%
  filter(value_projstart!=0) %>% #what kind of data is dropped here?
  group_by(treat_ever, year_standard) %>%
  summarise(avg_fc_loss = mean(fc_loss, na.rm=T)) %>%
  ggplot(aes(x=year_standard, y=avg_fc_loss, col=as.factor(treat_ever))) +
  geom_vline(xintercept = 0) +
  geom_line(aes(group=as.factor(treat_ever))) +
  geom_point(aes(size=3), show.legend = F) +
  labs(title=paste("Matching Frame", i, "(Matched data)"),
       x ="Project Years", y = "Forest cover loss") +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name= "", breaks=c("0", "1"),
                        labels=c("Non-Protected Forest  Areas", "Protected Forest Areas"))
ggsave(paste0("plot", i, "_fcloss_post.png"), plot = plot_fc_loss_post, path = "../../datalake/mapme.protectedareas/output/plots/parallel_trends_tmax")


## create pdf
plot(plot_fc_area_pre)
plot(plot_fc_area_post)
plot(plot_fc_loss_pre)
plot(plot_fc_loss_post)
plot(plot_pctforest_pre)
plot(plot_pctforest_post)
}

dev.off() 





T_year <- c(2007, 2015)
#T_year <- c(2004)
for (i in T_year) {
# Get plots with only treatment line
  # After matching
  mp <- read.csv(paste0("../../datalake/mapme.protectedareas/output/tabular/regression_input/matched_panel_", i, ".csv"))
  
  mp_gfw <- 
    left_join(mp, area_all,
              by=c("poly_id","year"))
  
  area_mp_projstart <- mp_gfw %>%
    filter(year_standard == 0) %>%
    select(uid_myear,
           value) %>%
    rename("value_projstart" = value)
  
  mp_gfw_rel <- left_join(mp_gfw, area_mp_projstart,
                          by=c("uid_myear")) %>%
    mutate(area_pct_projstart = value/value_projstart)

### percent forest
plot_pctforest_post <- mp_gfw_rel %>%
  filter(value_projstart!=0) %>% #what kind of data is dropped here?
  group_by(treat_ever, year_standard) %>%
  summarise(avg_areapct = mean(area_pct_projstart, na.rm=T)) %>%
  ggplot(aes(x=year_standard, y=avg_areapct, col=as.factor(treat_ever))) +
  geom_vline(xintercept = 0) +
  geom_line(aes(group=as.factor(treat_ever))) +
  geom_point(aes(size=3), show.legend = F) +
  labs(title=paste("Matching Frame", i, "(Matched data)"),
       x ="Project Years", y = "Relative forest cover")+
  theme(legend.position = "bottom") +
  scale_colour_discrete(name= "", breaks=c("0", "1"),
                        labels=c("Non-Protected Forest  Areas", "Protected Forest Areas"))


### get ylim and xlim
layer_scales(plot_pctforest_post)$y$range$range
layer_scales(plot_pctforest_post)$x$range$range

### plot without control regions
plot_pctforest_post_single <- mp_gfw_rel %>%
  filter(value_projstart!=0,) %>% #what kind of data is dropped here?
  group_by(treat_ever, year_standard) %>%
  summarise(avg_areapct = mean(area_pct_projstart, na.rm=T)) %>%
  filter(treat_ever==1) %>% 
  ggplot(aes(x=year_standard, y=avg_areapct, col=as.factor(treat_ever))) +
  geom_vline(xintercept = 0) +
  geom_line(aes(group=as.factor(treat_ever))) +
  geom_point(aes(size=3), show.legend = F) +
  labs(title=paste("Matching Frame", i, "(Matched data)"),
       x ="Project Years", y = "Relative forest cover") +
  theme(legend.position = "bottom") +
  ylim(layer_scales(plot_pctforest_post)$y$range$range) +
  xlim(layer_scales(plot_pctforest_post)$x$range$range) +
  scale_color_manual(name= "", breaks=c("1"),
                     labels=c("Protected Forest Areas"), values=c("#00BFC4"))

plot_pctforest_post
plot_pctforest_post_single
ggsave(paste0("plot", i, "_pctforest_post_single.png"), 
       plot = plot_pctforest_post_single, 
       path = "../../datalake/mapme.protectedareas/output/plots/parallel_trends_tmax",
       width = 2099,
       height = 2099,
       units = "px"
       )


}




get_dims(plot_pctforest_post_single)












# Question: Why do we observe a spike for control units in 2015?
mp_2015 <- read.csv("../../datalake/mapme.protectedareas/output/tabular/regression_input/matched_panel_2015.csv")

mp_2015_gfw <- 
  left_join(mp_2015, area_all,
            by=c("poly_id","year"))

area_mp_projstart <- mp_2015_gfw %>%
  filter(year_standard == 0) %>%
  select(uid_myear,
         value) %>%
  rename("value_projstart" = value)

mp_2015_gfw_rel <- left_join(mp_2015_gfw, area_mp_projstart,
                        by=c("uid_myear")) %>%
  mutate(area_pct_projstart = value/value_projstart)

## number of observations in treatment and control are constant over time
table(mp_2015$treat_ever, mp_2015$year) 

## check if control units are thoughout ever_treat==0
test0 <- mp_2015_gfw_rel %>%
  subset(treat_ever==0)
  

test1 <- mp_2015_gfw_rel %>%
  subset(treat_ever==1)
    
    
    
    
    