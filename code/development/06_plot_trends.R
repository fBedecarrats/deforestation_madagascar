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

# Before matching

out_2015 <- read.csv("../../datalake/mapme.protectedareas/output/tabular/regression_input/out2015.csv")

out_2015_gfw <- 
  left_join(out_2015, area_all,
                          by=c("poly_id","year"))

area_2015_projstart <- out_2015_gfw %>%
  filter(year_standard == 0) %>%
  select(uid_myear,
         value) %>%
  rename("value_projstart" = value)

out_2015_gfw_rel <- left_join(out_2015_gfw, area_2015_projstart,
                            by=c("uid_myear")) %>%
  mutate(area_pct_projstart = value/value_projstart)

out_2015_gfw_rel %>%
  filter(value_projstart!=0) %>% #what kind of data is dropped here?
  group_by(treat_ever, year_standard) %>%
  summarise(avg_areapct = mean(area_pct_projstart, na.rm=T)) %>%
  ggplot(aes(x=year_standard, y=avg_areapct, col=as.factor(treat_ever))) +
  geom_line(aes(group=as.factor(treat_ever))) +
  geom_point(aes(size=3))


### fc_area
out_2015_gfw_rel %>%
  filter(value_projstart!=0) %>% #what kind of data is dropped here?
  group_by(treat_ever, year_standard) %>%
  summarise(avg_fc_area = mean(fc_area, na.rm=T)) %>%
  ggplot(aes(x=year_standard, y=avg_fc_area, col=as.factor(treat_ever))) +
  geom_line(aes(group=as.factor(treat_ever))) +
  geom_point(aes(size=3))


### fc_loss
out_2015_gfw_rel %>%
  filter(value_projstart!=0) %>% #what kind of data is dropped here?
  group_by(treat_ever, year_standard) %>%
  summarise(avg_fc_loss = mean(fc_loss, na.rm=T)) %>%
  ggplot(aes(x=year_standard, y=avg_fc_loss, col=as.factor(treat_ever))) +
  geom_line(aes(group=as.factor(treat_ever))) +
  geom_point(aes(size=3))




# After matching
mp_2015 <- read.csv("../../datalake/mapme.protectedareas/output/tabular/regression_input/matched_panel_2015.csv")

mp_2015_gfw <- 
  left_join(mp_2015, area_all,
            by=c("poly_id","year"))

area_mp_2015_projstart <- mp_2015_gfw %>%
  filter(year_standard == 0) %>%
  select(uid_myear,
         value) %>%
  rename("value_projstart" = value)

mp_2015_gfw_rel <- left_join(mp_2015_gfw, area_mp_2015_projstart,
                              by=c("uid_myear")) %>%
  mutate(area_pct_projstart = value/value_projstart)

mp_2015_gfw_rel %>%
  filter(value_projstart!=0) %>% #what kind of data is dropped here?
  group_by(treat_ever, year_standard) %>%
  summarise(avg_areapct = mean(area_pct_projstart, na.rm=T)) %>%
  ggplot(aes(x=year_standard, y=avg_areapct, col=as.factor(treat_ever))) +
  geom_line(aes(group=as.factor(treat_ever))) +
  geom_point(aes(size=3))


### fc_area
mp_2015_gfw_rel %>%
  filter(value_projstart!=0) %>% #what kind of data is dropped here?
  group_by(treat_ever, year_standard) %>%
  summarise(avg_fc_area = mean(fc_area, na.rm=T)) %>%
  ggplot(aes(x=year_standard, y=avg_fc_area, col=as.factor(treat_ever))) +
  geom_line(aes(group=as.factor(treat_ever))) +
  geom_point(aes(size=3))

### fc_loss
mp_2015_gfw_rel %>%
  filter(value_projstart!=0) %>% #what kind of data is dropped here?
  group_by(treat_ever, year_standard) %>%
  summarise(avg_fc_loss = mean(fc_loss, na.rm=T)) %>%
  ggplot(aes(x=year_standard, y=avg_fc_loss, col=as.factor(treat_ever))) +
  geom_line(aes(group=as.factor(treat_ever))) +
  geom_point(aes(size=3))



# Question: Why do we observe a spike for control units in 2015?

## number of observations in treatment and control are constant over time
table(mp_2015$treat_ever, mp_2015$year) 

## check if control units are thoughout ever_treat==0
test0 <- mp_2015_gfw_rel %>%
  subset(treat_ever==0) %>% 
  

test1 <- mp_2015_gfw_rel %>%
  subset(treat_ever==1)
    
    
    
    
    