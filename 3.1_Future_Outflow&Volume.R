

#*******************************************************************************
# # This script to predict future water turbined from S.Giustina 
# # (model selected in script 2.2)
# # and plotting it as time series and as monthly variations

#*******************************************************************************

#### Libraries ####

library("dplyr")
library("tidyr")
library("tibble")
library("merTools")
library("purrr")
library("lubridate")
library("plotly")
library("scales")
library("cowplot")

####  Setting parameters ####
# Maximum values of water that can physically be turbined
maxQ_d<-66*60*60*24

maxQ_mo<-66*2592000

# quantiles for past values
Q_out10<-quantile(santemp_day2_mo$Out_lmer_mo, 0.10, na.rm=T)
Q_out20<-quantile(santemp_day2_mo$Out_lmer_mo, 0.20, na.rm=T)
Q_out80<-quantile(santemp_day2_mo$Out_lmer_mo, 0.80, na.rm=T)
Q_out90<-quantile(santemp_day2_mo$Out_lmer_mo, 0.90, na.rm=T)


Q_in10<-quantile(santemp_day2_mo$Inflow_sim_mo, 0.10, na.rm=T)
Q_in20<-quantile(santemp_day2_mo$Inflow_sim_mo, 0.20, na.rm=T)
Q_in80<-quantile(santemp_day2_mo$Inflow_sim_mo, 0.80, na.rm=T)
Q_in90<-quantile(santemp_day2_mo$Inflow_sim_mo, 0.90, na.rm=T)

Q_in100<-quantile(santemp_day2_mo$Inflow_sim_mo, 1, na.rm=T)


#### Daily ####
# RCP45 2021-2050 #

# Daily time step
# inflow values RCP4.5 - 2021-2050
Q31_rcp45_50_d<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP45_2021_2050_day.rds") %>%
  mutate(day=day(Date),
         WD=weekdays(Date)) %>% 
  rename(Inflow_sim=Value_day) %>% 
  dplyr::select(Date, yr, mo, WD, day, Inflow_sim)

Q31_rcp45_50_d$Volume_lag<-117418853

Q31_rcp45_50_d$Volume_lag[3:nrow(Q31_rcp45_50_d)]<-NA

Q31_rcp45_50_d<-merge(Q31_rcp45_50_d, mef_past_day, by=c("mo", "day")) %>% 
  merge(., HP_past_day, by=c("mo", "day")) %>% 
  ungroup() %>% 
  rename(Outflow_HP=Outflow_HP_d) %>% 
  arrange(-desc(Date)) %>% 
  as_tibble()


Q31_rcp45_50_d$Outflow_HP[4:nrow(Q31_rcp45_50_d)]<-NA


# confidence levels
Q31_rcp45_50_d$HP_Upr<-Q31_rcp45_50_d$Outflow_HP
Q31_rcp45_50_d$HP_Lwr<-Q31_rcp45_50_d$Outflow_HP

Q31_rcp45_50_d$HP_Upr[4:nrow(Q31_rcp45_50_d)]<-NA
Q31_rcp45_50_d$HP_Lwr[4:nrow(Q31_rcp45_50_d)]<-NA

Q31_rcp45_50_d$Outflow_rel<-NA

for (i in 3:(nrow(Q31_rcp45_50_d)-1)){
  
  Q31_rcp45_50_d$Volume_lag[i]<-sum(Q31_rcp45_50_d$Volume_lag[i-1], Q31_rcp45_50_d$Inflow_sim[i],
                                    -Q31_rcp45_50_d$Outflow_HP[i], -Q31_rcp45_50_d$MEF_d[i], -Q31_rcp45_50_d$Outflow_rel[i],
                                  na.rm=T)
  
  Q31_rcp45_50_d$Outflow_HP[i+1]<-predict(model_lmer5, Q31_rcp45_50_d[i,], allow.new.levels=T)
  
  Q31_rcp45_50_d$Outflow_rel[i+1]<-ifelse(Q31_rcp45_50_d$Volume_lag[i]>=159299502, 14561942, 0)
  
  
  print(((nrow(Q31_rcp45_50_d)-1)-i)/(nrow(Q31_rcp45_50_d)-1)*100)
  
}

Q31_rcp45_50_d$Volume_lag[nrow(Q31_rcp45_50_d)]<-sum(Q31_rcp45_50_d$Volume_lag[nrow(Q31_rcp45_50_d)-1], 
                                                    Q31_rcp45_50_d$Inflow_sim[nrow(Q31_rcp45_50_d)],
                                                    -Q31_rcp45_50_d$Outflow_HP[nrow(Q31_rcp45_50_d)],
                                                    -Q31_rcp45_50_d$MEF_d[nrow(Q31_rcp45_50_d)], na.rm=T)

a<-predictInterval(model_lmer5, Q31_rcp45_50_d, level = 1)

Q31_rcp45_50_d$HP_Lwr<-a$lwr
Q31_rcp45_50_d$HP_Upr<-a$upr


# saving it for later monthly analysis
Q31_rcp45_50_m<-Q31_rcp45_50_d %>%
  group_by(yr, mo) %>% 
  mutate(Volume_lag_mo=mean(Volume_lag, na.rm=T),
         Outflow_HP_mo=sum(Outflow_HP, na.rm=T),
         HP_Lwr_mo=sum(HP_Lwr, na.rm=T),
         HP_Upr_mo=sum(HP_Upr, na.rm=T),
         Inflow_sim_mo=sum(Inflow_sim),
         MEF_mo=sum(MEF_d),
         Outflow_rel_mo=sum(Outflow_rel, na.rm=T)) %>% 
  distinct(yr, mo, .keep_all = T)


Q31_rcp45_50_d$Volume_lagged=lag(Q31_rcp45_50_d$Volume_lag) 
Q31_rcp45_50_d$Volume_lagged[1]=Q31_rcp45_50_d$Volume_lag[1]

Q31_rcp45_50_d<-Q31_rcp45_50_d %>% 
  rowwise() %>% 
  mutate(Vol_Lwr=sum(Volume_lagged, Inflow_sim, -HP_Upr, -MEF_d, -Outflow_rel, na.rm=T),
         Vol_Upr=sum(Volume_lagged, Inflow_sim, -HP_Lwr, -MEF_d, -Outflow_rel, na.rm=T))

# saveRDS(Q31_rcp45_50_d, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Water_balance/Outflow&Volume_RCP45_2021_2050_day.rds")
# Q31_rcp45_50_d<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Water_balance/Outflow&Volume_RCP45_2021_2050_day.rds") %>% 
#   as_tibble()


# daily plot
Q31_rcp45_50_d %>%
  dplyr::select(Date, Outflow_HP, HP_Lwr, HP_Upr) %>%
  gather(Variable, Value, c("Outflow_HP")) %>%
  ggplot()+
  geom_line(aes(x=Date, y=Value/maxQ_d*100, color=Variable))+
  geom_ribbon(aes(x=Date, ymin=HP_Lwr/maxQ_d*100, ymax=HP_Upr/maxQ_d*100),  alpha=0.2, colour=NA)

Q31_rcp45_50_d %>%
  dplyr::select(Date, Volume_lagged, Vol_Lwr, Vol_Upr) %>%
  gather(Variable, Value, c("Volume_lagged")) %>%
  ggplot()+
  geom_line(aes(x=Date, y=Value/159299502*100, color=Variable))


# monthly analysis
Q31_rcp45_50_m$Volume_lagged_mo=lag(Q31_rcp45_50_m$Volume_lag_mo)
Q31_rcp45_50_m$Volume_lagged_mo[1]<-Q31_rcp45_50_m$Volume_lag_mo[1]

Q31_rcp45_50_m<-Q31_rcp45_50_m %>% 
  rowwise() %>% 
  mutate(Vol_Lwr_mo=sum(Volume_lagged_mo, Inflow_sim_mo, -HP_Upr_mo, -MEF_mo, -Outflow_rel_mo, na.rm=T),
         Vol_Upr_mo=sum(Volume_lagged_mo, Inflow_sim_mo, -HP_Lwr_mo, -MEF_mo, -Outflow_rel_mo, na.rm=T))


# monthly plot
Q31_rcp45_50_m %>%
  dplyr::select(Date, Outflow_HP_mo, HP_Lwr_mo, HP_Upr_mo) %>%
  gather(Legend, Value, c("Outflow_HP_mo")) %>%
  ggplot()+
  geom_line(aes(x=Date, y=Value/maxQ_mo*100, color=Legend))+
  geom_ribbon(aes(x=Date, ymin=HP_Lwr_mo/maxQ_mo*100, ymax=HP_Upr_mo/maxQ_mo*100),  alpha=0.2, colour=NA)

Q31_rcp45_50_m %>%
  filter(yr>=2021 & mo>=2) %>% 
  dplyr::select(Date, Volume_lagged_mo, Vol_Lwr_mo, Vol_Upr_mo) %>% 
  gather(Variable, Value, c("Volume_lagged_mo")) %>%
  ggplot()+
  geom_line(aes(x=Date, y=Value/159299502*100, color=Variable)) +
  geom_hline(yintercept=Q_vol90/159299502*100, lty=2, cex=0.8)+
  geom_hline(yintercept=Q_vol10/159299502*100, lty=2, cex=0.8)+
  geom_ribbon(aes(x=Date, ymin=Vol_Lwr_mo/159299502*100, ymax=Vol_Upr_mo/159299502*100), alpha=0.2, colour=NA)

Q31_rcp45_50_m<-Q31_rcp45_50_m %>% 
  dplyr::select(Date, yr, mo, Inflow_sim_mo, Outflow_HP_mo, HP_Lwr_mo, HP_Upr_mo, MEF_mo, Volume_lag_mo, Volume_lagged_mo, Vol_Lwr_mo, Vol_Upr_mo) %>% 
  mutate(id=1,
         Scenario="RCP 4.5", 
         Time_period="2021-2050")

saveRDS(Q31_rcp45_50_m, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Future_scenarios/Outflow&Volume_RCP45_2021_2050_month(no_maxminconditions).rds")
Q31_rcp45_50_m<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Future_scenarios/Outflow&Volume_RCP45_2021_2050_month(no_maxminconditions).rds")
# adding reference for 10-year time slice
seqq<-rep(1:10, each=36)

Q31_rcp45_50_m$con<-seqq


### end for 2021-2050



# RCP45 2041-2070 #
# Daily time step
# inflow values RCP4.5 - 2041-2070
Q31_rcp45_70_d<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP45_2041_2070_day.rds") %>%
  mutate(day=day(Date),
         WD=weekdays(Date)) %>% 
  rename(Inflow_sim=Value_d) %>%
  filter(yr>=2041) %>% 
  dplyr::select(Date, yr, mo, WD, day, Inflow_sim)

Q31_rcp45_70_d$Volume_lag<-117418853

Q31_rcp45_70_d$Volume_lag[3:nrow(Q31_rcp45_70_d)]<-NA

Q31_rcp45_70_d<-merge(Q31_rcp45_70_d, mef_past_day, by=c("mo", "day")) %>% 
  merge(., HP_past_day, by=c("mo", "day")) %>% 
  ungroup() %>% 
  rename(Outflow_HP=Outflow_HP_d) %>% 
  arrange(-desc(Date)) %>% 
  as_tibble()


Q31_rcp45_70_d$Outflow_HP[4:nrow(Q31_rcp45_70_d)]<-NA

Q31_rcp45_70_d$Outflow_rel<-NA

# confidence levels
Q31_rcp45_70_d$HP_Upr<-Q31_rcp45_70_d$Outflow_HP
Q31_rcp45_70_d$HP_Lwr<-Q31_rcp45_70_d$Outflow_HP

Q31_rcp45_70_d$HP_Upr[4:nrow(Q31_rcp45_70_d)]<-NA
Q31_rcp45_70_d$HP_Lwr[4:nrow(Q31_rcp45_70_d)]<-NA

for (i in 3:(nrow(Q31_rcp45_70_d)-1)){
  
  Q31_rcp45_70_d$Volume_lag[i]<-sum(Q31_rcp45_70_d$Volume_lag[i-1], Q31_rcp45_70_d$Inflow_sim[i],
                                    -Q31_rcp45_70_d$Outflow_HP[i], -Q31_rcp45_70_d$MEF_d[i], -Q31_rcp45_70_d$Outflow_rel[i],
                                    na.rm=T)
  
  Q31_rcp45_70_d$Outflow_HP[i+1]<-predict(model_lmer5, Q31_rcp45_70_d[i,], allow.new.levels=T)
  

  Q31_rcp45_70_d$Outflow_rel[i+1]<-ifelse(Q31_rcp45_70_d$Volume_lag[i]>=159299502, 14561942, 0)
  
  
  print(((nrow(Q31_rcp45_70_d)-1)-i)/(nrow(Q31_rcp45_70_d)-1)*100)
  
}

Q31_rcp45_70_d$Volume_lag[nrow(Q31_rcp45_70_d)]<-sum(Q31_rcp45_70_d$Volume_lag[nrow(Q31_rcp45_70_d)-1], 
                                                     Q31_rcp45_70_d$Inflow_sim[nrow(Q31_rcp45_70_d)],
                                                     -Q31_rcp45_70_d$Outflow_HP[nrow(Q31_rcp45_70_d)],
                                                     -Q31_rcp45_70_d$MEF_d[nrow(Q31_rcp45_70_d)],
                                                     -Q31_rcp45_70_d$Outflow_rel[nrow(Q31_rcp45_70_d)], na.rm=T)

a2<-predictInterval(model_lmer5, Q31_rcp45_70_d, level = 1)

Q31_rcp45_70_d$HP_Lwr<-a2$lwr
Q31_rcp45_70_d$HP_Upr<-a2$upr

# saving it for later monthly analysis
Q31_rcp45_70_m<-Q31_rcp45_70_d %>%
  group_by(yr, mo) %>% 
  mutate(Volume_lag_mo=mean(Volume_lag, na.rm=T),
         Outflow_HP_mo=sum(Outflow_HP, na.rm=T),
         HP_Lwr_mo=sum(HP_Lwr, na.rm=T),
         HP_Upr_mo=sum(HP_Upr, na.rm=T),
         Inflow_sim_mo=sum(Inflow_sim),
         MEF_mo=sum(MEF_d),
         Outflow_rel_mo=sum(Outflow_rel, na.rm = T)) %>% 
  distinct(yr, mo, .keep_all = T)


Q31_rcp45_70_d$Volume_lagged=lag(Q31_rcp45_70_d$Volume_lag)


Q31_rcp45_70_d<-Q31_rcp45_70_d %>% 
  rowwise() %>% 
  mutate(Vol_Lwr=sum(Volume_lagged, Inflow_sim, -HP_Upr, -MEF_d, -Outflow_rel, na.rm=T),
         Vol_Upr=sum(Volume_lagged, Inflow_sim, -HP_Lwr, -MEF_d, -Outflow_rel, na.rm=T))

# saveRDS(Q31_rcp45_70_d, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Water_balance/Outflow&Volume_RCP45_2041_2070_day.rds")
# Q31_rcp45_70_d<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Water_balance/Outflow&Volume_RCP45_2041_2070_day.rds") %>% 
#   as_tibble()


# daily plot
Q31_rcp45_70_d %>%
  dplyr::select(Date, Outflow_HP, HP_Lwr, HP_Upr) %>%
  gather(Variable, Value, c("Outflow_HP")) %>%
  ggplot()+
  geom_line(aes(x=Date, y=Value/maxQ_d*100, color=Variable))+
  geom_ribbon(aes(x=Date, ymin=HP_Lwr/maxQ_d*100, ymax=HP_Upr/maxQ_d*100),  alpha=0.2, colour=NA)

Q31_rcp45_70_d %>%
  dplyr::select(Date, Volume_lagged, Vol_Lwr, Vol_Upr) %>%
  gather(Variable, Value, c("Volume_lagged")) %>%
  ggplot()+
  geom_line(aes(x=Date, y=Value/159299502*100, color=Variable))


# monthly analysis
Q31_rcp45_70_m$Volume_lagged_mo=lag(Q31_rcp45_70_m$Volume_lag_mo)
Q31_rcp45_70_m$Volume_lagged_mo[1]<-Q31_rcp45_70_m$Volume_lag_mo[1]


Q31_rcp45_70_m<-Q31_rcp45_70_m %>% 
  rowwise() %>% 
  mutate(Vol_Lwr_mo=sum(Volume_lagged_mo, Inflow_sim_mo, -HP_Upr_mo, -MEF_mo, -Outflow_rel_mo, na.rm=T),
         Vol_Upr_mo=sum(Volume_lagged_mo, Inflow_sim_mo, -HP_Lwr_mo, -MEF_mo, -Outflow_rel_mo, na.rm=T))

# monthly plot
Q31_rcp45_70_m %>%
  dplyr::select(Date, Outflow_HP_mo, HP_Lwr_mo, HP_Upr_mo) %>%
  gather(Legend, Value, c("Outflow_HP_mo")) %>%
  ggplot()+
  geom_line(aes(x=Date, y=Value/maxQ_mo*100, color=Legend))+
  geom_ribbon(aes(x=Date, ymin=HP_Lwr_mo/maxQ_mo*100, ymax=HP_Upr_mo/maxQ_mo*100),  alpha=0.2, colour=NA)

Q31_rcp45_70_m %>%
  filter(yr>=2040 & mo >=2) %>% 
  dplyr::select(Date, Volume_lagged_mo, Vol_Lwr_mo, Vol_Upr_mo) %>%
  gather(Variable, Value, c("Volume_lagged_mo")) %>%
  ggplot()+
  geom_line(aes(x=Date, y=Value/159299502*100, color=Variable)) +
  geom_ribbon(aes(x=Date, ymin=Vol_Lwr_mo/159299502*100, ymax=Vol_Upr_mo/159299502*100), alpha=0.2, colour=NA)+
  geom_hline(yintercept = c(Q_vol90/159299502*100, Q_vol10/159299502*100), lty=2, cex=0.8)

Q31_rcp45_70_m<-Q31_rcp45_70_m %>% 
  dplyr::select(Date, yr, mo, Inflow_sim_mo, Outflow_HP_mo, HP_Lwr_mo, HP_Upr_mo, MEF_mo, Volume_lag_mo, Volume_lagged_mo, Vol_Lwr_mo, Vol_Upr_mo) %>% 
  mutate(id=2,
         Scenario="RCP 4.5", 
         Time_period="2041-2070")

saveRDS(Q31_rcp45_70_m, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Future_scenarios/Outflow&Volume_RCP45_2041_2070_month(no_maxminconditions).rds")
Q31_rcp45_70_m<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Future_scenarios/Outflow&Volume_RCP45_2041_2070_month(no_maxminconditions).rds")

# adding reference for 10-year time slice
Q31_rcp45_70_m$con<-seqq

### end for 2041-2070


# RCP85 2021-2050 #
# Daily time step
# inflow values RCP8.5 - 2021-2050
Q31_rcp85_50_d<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP85_2021_2050_day.rds") %>%
  mutate(day=day(Date),
         WD=weekdays(Date)) %>% 
  rename(Inflow_sim=Value_d) %>%
  filter(yr>=2021) %>% 
  dplyr::select(Date, yr, mo, WD, day, Inflow_sim)

Q31_rcp85_50_d$Volume_lag<-117418853

Q31_rcp85_50_d$Volume_lag[3:nrow(Q31_rcp85_50_d)]<-NA

Q31_rcp85_50_d<-merge(Q31_rcp85_50_d, mef_past_day, by=c("mo", "day")) %>% 
  merge(., HP_past_day, by=c("mo", "day")) %>% 
  ungroup() %>% 
  rename(Outflow_HP=Outflow_HP_d) %>% 
  arrange(-desc(Date)) %>% 
  as_tibble()


Q31_rcp85_50_d$Outflow_HP[4:nrow(Q31_rcp85_50_d)]<-NA

Q31_rcp85_50_d$Outflow_rel<-NA

# confidence levels
Q31_rcp85_50_d$HP_Upr<-Q31_rcp85_50_d$Outflow_HP
Q31_rcp85_50_d$HP_Lwr<-Q31_rcp85_50_d$Outflow_HP

Q31_rcp85_50_d$HP_Upr[4:nrow(Q31_rcp85_50_d)]<-NA
Q31_rcp85_50_d$HP_Lwr[4:nrow(Q31_rcp85_50_d)]<-NA

for (i in 3:(nrow(Q31_rcp85_50_d)-1)){
  
  Q31_rcp85_50_d$Volume_lag[i]<-sum(Q31_rcp85_50_d$Volume_lag[i-1], Q31_rcp85_50_d$Inflow_sim[i],
                                    -Q31_rcp85_50_d$Outflow_HP[i], -Q31_rcp85_50_d$MEF_d[i], -Q31_rcp85_50_d$Outflow_rel[i],
                                    na.rm=T)
  

  Q31_rcp85_50_d$Outflow_HP[i+1]<-predict(model_lmer5, Q31_rcp85_50_d[i,], allow.new.levels=T)
  
  Q31_rcp85_50_d$Outflow_rel[i+1]<-ifelse(Q31_rcp85_50_d$Volume_lag[i]>159299502, 14561942,0)
  
  print(((nrow(Q31_rcp85_50_d)-1)-i)/(nrow(Q31_rcp85_50_d)-1)*100)
  
}

Q31_rcp85_50_d$Volume_lag[nrow(Q31_rcp85_50_d)]<-sum(Q31_rcp85_50_d$Volume_lag[nrow(Q31_rcp85_50_d)-1], 
                                                     Q31_rcp85_50_d$Inflow_sim[nrow(Q31_rcp85_50_d)],
                                                     -Q31_rcp85_50_d$Outflow_HP[nrow(Q31_rcp85_50_d)],
                                                     -Q31_rcp85_50_d$MEF_d[nrow(Q31_rcp85_50_d)],
                                                     -Q31_rcp85_50_d$Outflow_rel[nrow(Q31_rcp85_50_d)],na.rm=T)

a3<-predictInterval(model_lmer5, Q31_rcp85_50_d, level = 1)

Q31_rcp85_50_d$HP_Lwr<-a3$lwr
Q31_rcp85_50_d$HP_Upr<-a3$upr

# saving it for later monthly analysis
Q31_rcp85_50_m<-Q31_rcp85_50_d %>%
  group_by(yr, mo) %>% 
  mutate(Volume_lag_mo=mean(Volume_lag, na.rm=T),
         Outflow_HP_mo=sum(Outflow_HP, na.rm=T),
         HP_Lwr_mo=sum(HP_Lwr, na.rm=T),
         HP_Upr_mo=sum(HP_Upr, na.rm=T),
         Inflow_sim_mo=sum(Inflow_sim),
         MEF_mo=sum(MEF_d),
         Outflow_rel_mo=sum(Outflow_rel, na.rm = T)) %>% 
  distinct(yr, mo, .keep_all = T)



Q31_rcp85_50_d$Volume_lagged=lag(Q31_rcp85_50_d$Volume_lag) 
Q31_rcp85_50_d$Volume_lagged[1]=Q31_rcp85_50_d$Volume_lag[1]


Q31_rcp85_50_d<-Q31_rcp85_50_d %>% 
  rowwise() %>% 
  mutate(Vol_Lwr=sum(Volume_lagged, Inflow_sim, -HP_Upr, -MEF_d, -Outflow_rel, na.rm=T),
         Vol_Upr=sum(Volume_lagged, Inflow_sim, -HP_Lwr, -MEF_d, -Outflow_rel, na.rm=T))

# saveRDS(Q31_rcp85_50_d, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Water_balance/Outflow&Volume_RCP85_2021_2050_day.rds")
# Q31_rcp85_50_d<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Water_balance/Outflow&Volume_RCP85_2021_2050_day.rds") %>% 
#   as_tibble()


# daily plot
Q31_rcp85_50_d %>%
  dplyr::select(Date, Outflow_HP, HP_Lwr, HP_Upr) %>%
  gather(Variable, Value, c("Outflow_HP")) %>%
  ggplot()+
  geom_line(aes(x=Date, y=Value/maxQ_d*100, color=Variable))+
  geom_ribbon(aes(x=Date, ymin=HP_Lwr/maxQ_d*100, ymax=HP_Upr/maxQ_d*100),  alpha=0.2, colour=NA)

Q31_rcp85_50_d %>%
  dplyr::select(Date, Volume_lagged, Vol_Lwr, Vol_Upr) %>%
  gather(Variable, Value, c("Volume_lagged")) %>%
  ggplot()+
  geom_line(aes(x=Date, y=Value/159299502*100, color=Variable))


# monthly analysis
Q31_rcp85_50_m$Volume_lagged_mo=lag(Q31_rcp85_50_m$Volume_lag_mo)
Q31_rcp85_50_m$Volume_lagged_mo[1]<-Q31_rcp85_50_m$Volume_lag_mo[1]


Q31_rcp85_50_m<-Q31_rcp85_50_m %>% 
  rowwise() %>% 
  mutate(Vol_Lwr_mo=sum(Volume_lagged_mo, Inflow_sim_mo, -HP_Upr_mo, -MEF_mo, -Outflow_rel_mo, na.rm=T),
         Vol_Upr_mo=sum(Volume_lagged_mo, Inflow_sim_mo, -HP_Lwr_mo, -MEF_mo, -Outflow_rel_mo, na.rm=T))


# monthly plot
Q31_rcp85_50_m %>%
  dplyr::select(Date, Outflow_HP_mo, HP_Lwr_mo, HP_Upr_mo) %>%
  gather(Legend, Value, c("Outflow_HP_mo")) %>%
  ggplot()+
  geom_line(aes(x=Date, y=Value/maxQ_mo*100, color=Legend))+
  geom_ribbon(aes(x=Date, ymin=HP_Lwr_mo/maxQ_mo*100, ymax=HP_Upr_mo/maxQ_mo*100),  alpha=0.2, colour=NA)

Q31_rcp85_50_m %>%
  filter(yr>2020 & mo>=2) %>% 
  dplyr::select(Date, Volume_lagged_mo, Vol_Lwr_mo, Vol_Upr_mo) %>%
  gather(Variable, Value, c("Volume_lagged_mo")) %>%
  ggplot()+
  geom_line(aes(x=Date, y=Value/159299502*100, color=Variable)) +
  geom_ribbon(aes(x=Date, ymin=Vol_Lwr_mo/159299502*100, ymax=Vol_Upr_mo/159299502*100), alpha=0.2, colour=NA)

Q31_rcp85_50_m<-Q31_rcp85_50_m %>% 
  dplyr::select(Date, yr, mo, Inflow_sim_mo, Outflow_HP_mo, HP_Lwr_mo, HP_Upr_mo, MEF_mo, Volume_lag_mo, Volume_lagged_mo, Vol_Lwr_mo, Vol_Upr_mo) %>% 
  mutate(id=3,
         Scenario="RCP 8.5", 
         Time_period="2021-2050")

saveRDS(Q31_rcp85_50_m, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Future_scenarios/Outflow&Volume_RCP85_2021_2050_month(no_maxminconditions).rds")
Q31_rcp85_50_m<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Future_scenarios/Outflow&Volume_RCP85_2021_2050_month(no_maxminconditions).rds")

# adding reference for 10-year time slice
Q31_rcp85_50_m$con<-seqq

### end for 2021-2050



# RCP85 2041-2070 #
# Daily time step
# inflow values RCP8.5 - 2041-2070
Q31_rcp85_70_d<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP85_2041_2070_day.rds") %>%
  mutate(day=day(Date),
         WD=weekdays(Date)) %>% 
  rename(Inflow_sim=Value_d) %>%
  filter(yr>=2041) %>% 
  dplyr::select(Date, yr, mo, WD, day, Inflow_sim)

Q31_rcp85_70_d$Volume_lag<-117418853

Q31_rcp85_70_d$Volume_lag[3:nrow(Q31_rcp85_70_d)]<-NA

Q31_rcp85_70_d<-merge(Q31_rcp85_70_d, mef_past_day, by=c("mo", "day")) %>% 
  merge(., HP_past_day, by=c("mo", "day")) %>% 
  ungroup() %>% 
  rename(Outflow_HP=Outflow_HP_d) %>% 
  arrange(-desc(Date)) %>% 
  as_tibble()


Q31_rcp85_70_d$Outflow_HP[4:nrow(Q31_rcp85_70_d)]<-NA

Q31_rcp85_70_d$Outflow_rel<-NA

# confidence levels
Q31_rcp85_70_d$HP_Upr<-Q31_rcp85_70_d$Outflow_HP
Q31_rcp85_70_d$HP_Lwr<-Q31_rcp85_70_d$Outflow_HP

Q31_rcp85_70_d$HP_Upr[4:nrow(Q31_rcp85_70_d)]<-NA
Q31_rcp85_70_d$HP_Lwr[4:nrow(Q31_rcp85_70_d)]<-NA

for (i in 3:(nrow(Q31_rcp85_70_d)-1)){
  
  Q31_rcp85_70_d$Volume_lag[i]<-sum(Q31_rcp85_70_d$Volume_lag[i-1], Q31_rcp85_70_d$Inflow_sim[i],
                                    -Q31_rcp85_70_d$Outflow_HP[i], -Q31_rcp85_70_d$MEF_d[i], -Q31_rcp85_70_d$Outflow_rel[i],
                                    na.rm=T)
  
  Q31_rcp85_70_d$Outflow_HP[i+1]<-predict(model_lmer5, Q31_rcp85_70_d[i,], allow.new.levels=T)
  
  Q31_rcp85_70_d$Outflow_rel[i+1]<-ifelse(Q31_rcp85_70_d$Volume_lag[i]>159299502, 14561942,0)
  
  
  print(((nrow(Q31_rcp85_70_d)-1)-i)/(nrow(Q31_rcp85_70_d)-1)*100)
  
}

Q31_rcp85_70_d$Volume_lag[nrow(Q31_rcp85_70_d)]<-sum(Q31_rcp85_70_d$Volume_lag[nrow(Q31_rcp85_70_d)-1], 
                                                     Q31_rcp85_70_d$Inflow_sim[nrow(Q31_rcp85_70_d)],
                                                     -Q31_rcp85_70_d$Outflow_HP[nrow(Q31_rcp85_70_d)],
                                                     -Q31_rcp85_70_d$MEF_d[nrow(Q31_rcp85_70_d)],
                                                     -Q31_rcp85_70_d$Outflow_rel[nrow(Q31_rcp85_70_d)], na.rm=T)

a4<-predictInterval(model_lmer5, Q31_rcp85_70_d, level = 1)

Q31_rcp85_70_d$HP_Lwr<-a4$lwr
Q31_rcp85_70_d$HP_Upr<-a4$upr

# saving it for later monthly analysis
Q31_rcp85_70_m<-Q31_rcp85_70_d %>%
  group_by(yr, mo) %>% 
  mutate(Volume_lag_mo=mean(Volume_lag, na.rm=T),
         Outflow_HP_mo=sum(Outflow_HP, na.rm=T),
         HP_Lwr_mo=sum(HP_Lwr, na.rm=T),
         HP_Upr_mo=sum(HP_Upr, na.rm=T),
         Inflow_sim_mo=sum(Inflow_sim),
         MEF_mo=sum(MEF_d),
         Outflow_rel_mo=sum(Outflow_rel, na.rm=T)) %>% 
  distinct(yr, mo, .keep_all = T)



Q31_rcp85_70_d$Volume_lagged=lag(Q31_rcp85_70_d$Volume_lag)
Q31_rcp85_70_d$Volume_lagged[1]=Q31_rcp85_70_d$Volume_lag[1]


Q31_rcp85_70_d<-Q31_rcp85_70_d %>% 
  rowwise() %>% 
  mutate(Vol_Lwr=sum(Volume_lagged, Inflow_sim, -HP_Upr, -MEF_d, -Outflow_rel, na.rm=T),
         Vol_Upr=sum(Volume_lagged, Inflow_sim, -HP_Lwr, -MEF_d, -Outflow_rel, na.rm=T))

# saveRDS(Q31_rcp85_70_d, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Water_balance/Outflow&Volume_RCP85_2041_2070_day.rds")
# Q31_rcp85_70_d<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Water_balance/Outflow&Volume_RCP85_2041_2070_day.rds") %>% 
#   as_tibble()


# daily plot
Q31_rcp85_70_d %>%
  dplyr::select(Date, Outflow_HP, HP_Lwr, HP_Upr) %>%
  gather(Variable, Value, c("Outflow_HP")) %>%
  ggplot()+
  geom_line(aes(x=Date, y=Value/maxQ_d*100, color=Variable))+
  geom_ribbon(aes(x=Date, ymin=HP_Lwr/maxQ_d*100, ymax=HP_Upr/maxQ_d*100),  alpha=0.2, colour=NA)

Q31_rcp85_70_d %>%
  dplyr::select(Date, Volume_lagged, Vol_Lwr, Vol_Upr) %>%
  gather(Variable, Value, c("Volume_lagged")) %>%
  ggplot()+
  geom_line(aes(x=Date, y=Value/159299502*100, color=Variable))


# monthly analysis
Q31_rcp85_70_m$Volume_lagged_mo=lag(Q31_rcp85_70_m$Volume_lag_mo)
Q31_rcp85_70_m$Volume_lagged_mo[1]<-Q31_rcp85_70_m$Volume_lagged_mo[1]


Q31_rcp85_70_m<-Q31_rcp85_70_m %>% 
  rowwise() %>% 
  mutate(Vol_Lwr_mo=sum(Volume_lagged_mo, Inflow_sim_mo, -HP_Upr_mo, -MEF_mo, -Outflow_rel_mo, na.rm=T),
         Vol_Upr_mo=sum(Volume_lagged_mo, Inflow_sim_mo, -HP_Lwr_mo, -MEF_mo, -Outflow_rel_mo, na.rm=T))


# monthly plot
Q31_rcp85_70_m %>%
  dplyr::select(Date, Outflow_HP_mo, HP_Lwr_mo, HP_Upr_mo) %>%
  gather(Legend, Value, c("Outflow_HP_mo")) %>%
  ggplot()+
  geom_line(aes(x=Date, y=Value/maxQ_mo*100, color=Legend))+
  geom_ribbon(aes(x=Date, ymin=HP_Lwr_mo/maxQ_mo*100, ymax=HP_Upr_mo/maxQ_mo*100),  alpha=0.2, colour=NA)

Q31_rcp85_70_m %>%
  filter(yr>2040 & mo>=2) %>% 
  dplyr::select(Date, Volume_lagged_mo, Vol_Lwr_mo, Vol_Upr_mo) %>%
  gather(Variable, Value, c("Volume_lagged_mo")) %>%
  ggplot()+
  geom_line(aes(x=Date, y=Value/159299502*100, color=Variable)) +
  geom_ribbon(aes(x=Date, ymin=Vol_Lwr_mo/159299502*100, ymax=Vol_Upr_mo/159299502*100), alpha=0.2, colour=NA)

Q31_rcp85_70_m<-Q31_rcp85_70_m %>% 
  dplyr::select(Date, yr, mo, Inflow_sim_mo, Outflow_HP_mo, HP_Lwr_mo, HP_Upr_mo, MEF_mo, Volume_lag_mo, Volume_lagged_mo, Vol_Lwr_mo, Vol_Upr_mo) %>% 
  mutate(id=4,
         Scenario="RCP 8.5", 
         Time_period="2041-2070")

saveRDS(Q31_rcp85_70_m, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Future_scenarios/Outflow&Volume_RCP85_2041_2070_month(no_maxminconditions).rds")
Q31_rcp85_70_m<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Future_scenarios/Outflow&Volume_RCP85_2041_2070_month(no_maxminconditions).rds")

# adding reference for 10-year time slice
Q31_rcp85_70_m$con<-seqq

### end for 2041-2070


#### Monthly ####

#### Plotting them all ####
con<-rep(1:6, each=60)

Q31_rcp45_50_m<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Future_scenarios/Outflow&Volume_RCP45_2021_2050_month(no_maxminconditions).rds")%>% 
  cbind(., con) %>% 
  as_tibble()

Q31_rcp45_70_m<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Future_scenarios/Outflow&Volume_RCP45_2041_2070_month(no_maxminconditions).rds")%>% 
  cbind(., con) %>% 
  as_tibble()

Q31_rcp85_50_m<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Future_scenarios/Outflow&Volume_RCP85_2021_2050_month(no_maxminconditions).rds")%>% 
  cbind(., con) %>% 
  as_tibble()

Q31_rcp85_70_m<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Future_scenarios/Outflow&Volume_RCP85_2041_2070_month(no_maxminconditions).rds") %>% 
  cbind(., con) %>% 
  as_tibble()

superturbina<-rbind(Q31_rcp45_50_m, Q31_rcp45_70_m, Q31_rcp85_50_m, Q31_rcp85_70_m) %>% 
  as_tibble() %>% 
  rename(Turbined_outflow=Outflow_HP_mo) %>%
  mutate(id=as.factor(id),
         Turbined_outflow=Turbined_outflow/171072000*100) %>% 
  gather(Variable, Value, Turbined_outflow)

# setting colours for volume
col_out<-c("#abd9e9", "#74add1", "#fee090", "#fdae61")
col_out2<-c("#A7D3D5", "#009B9F",  "#E4C1D8", "#C75DAA")
col_out_ros<-c("#B5BBE3", "#4A6FE3",  "#E6AFB9", "#D33F6A")
col_out_ros<-c("#8D9BE0", "#476DE0", "#E08397", "#D03C67")

# plot time series
p_turb<-superturbina %>% 
  rename(., Legend=Variable) %>% 
  ggplot(., aes(x=Date, y=Value, color=id, group=Scenario))+
  geom_line(size=1.1)+
  geom_ribbon(aes(x=Date, ymin=HP_Lwr_mo/171072000*100, ymax=HP_Upr_mo/171072000*100), alpha = .2, colour=NA)+
  geom_hline(yintercept=c(Q_out10/159299502*100, Q_out90/159299502*100), lty=2, cex=0.1)+
  facet_grid(Scenario ~ Time_period, scales="free_x")+
  labs(x="\nTime [Months]", y="Outflow [%]\n",
       title="Future turbined outflows")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20))+
  scale_x_date(breaks = pretty_breaks(15), expand=c(0.01, 0))+
  scale_colour_manual(values=c(col_out_ros))

# visualization
p_turb

# ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/S.Giustina_outflow_all2(no_maxminconditions).png", plot=p_turb,
#        width = 16,
#        height = 8, 
#        dpi=600)

# with percentiles ribbons

# ribbon references
b1<-superturbina %>% 
  dplyr::select(Date, Time_period) %>%
  mutate(From=0,
         "10th"=Q_out10/171072000*100,
         "20th"=Q_out20/171072000*100,
         "80th"=Q_out90/171072000*100,
         "90th"=100) %>% 
  gather(Legend, Value, c("10th","20th", "80th", "90th")) %>% 
  mutate(From=case_when(Legend=="10th" ~ 0,
                        Legend=="20th" ~ Q_out10/171072000*100,
                        Legend=="80th" ~ Q_out80/171072000*100,
                        Legend=="90th" ~ Q_out90/171072000*100)) %>% 
  mutate(Legend=as.factor(Legend))

p_turb2<-rbind(Q31_rcp45_50_m, Q31_rcp45_70_m, Q31_rcp85_50_m, Q31_rcp85_70_m) %>%
  as_tibble() %>%
  rename(Turbined_outflow=Outflow_HP_mo) %>%
  mutate(id=as.factor(id),
         Turbined_outflow=Turbined_outflow/171072000*100) %>%
  gather(Legend, Value, c("Turbined_outflow")) %>%
  mutate("10th"=Q_out10/171072000*100,
         "20th"=Q_out20/171072000*100,
         "80th"=Q_out90/171072000*100,
         "90th"=100) %>%
  gather(Legend2, Value2, c("10th","20th", "80th", "90th")) %>%
  mutate(From=case_when(Legend2 =="10th" ~ 0,
                 Legend2 =="20th" ~ Q_out10/171072000*100,
                 Legend2 =="80th" ~ Q_out80/171072000*100,
                 Legend2 =="90th" ~ Q_out90/171072000*100)) %>%
  dplyr::select(Date, id, Scenario, Time_period, Legend, Value, Legend2, Value2, From, HP_Lwr_mo, HP_Upr_mo) %>%
  ggplot()+
  geom_ribbon(aes(x=Date, ymin=From, ymax=Value2, fill=Legend2), alpha=0.5, colour=NA)+
  geom_ribbon(aes(x=Date, ymin=HP_Lwr_mo/171072000*100, ymax=HP_Upr_mo/171072000*100), alpha = .2, colour=NA)+
  geom_line(aes(x=Date, y=Value, color=id, group=Scenario), size=1.1)+
  facet_grid(Scenario ~ Time_period, scales="free_x")+
  labs(x="\nTime [Months]", y="Outflow [%]\n",
       title="Future turbined outflows")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20))+
  scale_x_date(breaks = pretty_breaks(15), expand=c(0.01, 0))+
  scale_colour_manual(values=c(col_out_ros))+
  scale_fill_manual("", values=c("#fdae61","#fee090","#abd9e9", "#74add1"))

p_turb2


# boxplot outflow 2
secc1<-rep(seq(2023, 2048, 5), each=60)
secc2<-rep(seq(2043, 2068, 5), each=60)
longsecc<-rep(c(secc1, secc2, secc1, secc2),4)


turb_boxplot2<-rbind(Q31_rcp45_50_m, Q31_rcp45_70_m, Q31_rcp85_50_m, Q31_rcp85_70_m) %>%
  as_tibble() %>%
  rename(Turbined_outflow=Outflow_HP_mo) %>%
  mutate(id=as.factor(id),
         Turbined_outflow=Turbined_outflow/171072000*100) %>%
  gather(Legend, Value, c("Turbined_outflow")) %>%
  mutate("10th"=Q_out10/171072000*100,
         "20th"=Q_out20/171072000*100,
         "80th"=Q_out90/171072000*100,
         "90th percentile"=100) %>%
  gather(Legend2, Value2, c("10th","20th", "80th", "90th percentile")) %>%
  mutate(From=case_when(Legend2 =="10th" ~ 0,
                        Legend2 =="20th" ~ Q_out10/171072000*100,
                        Legend2 =="80th" ~ Q_out80/171072000*100,
                        Legend2 =="90th percentile" ~ Q_out90/171072000*100)) %>% 
  dplyr::select(Date, yr, id, con, Scenario, Time_period, Legend, Value, Legend2, Value2, From, HP_Lwr_mo, HP_Upr_mo) %>% 
  mutate(x_min=case_when(
    Time_period == "2021-2050" ~  2025,
    Time_period == "2041-2070" ~  2045),
    x_max=case_when(
      Time_period == "2021-2050" ~ 2045,
      Time_period == "2041-2070" ~ 2065),
    yr1=longsecc)


turb_boxplot2_plot<-ggplot(data=turb_boxplot2)+
  geom_ribbon(aes(x=yr, ymin=From, ymax=Value2, fill=Legend2), alpha=0.7, colour=NA)+
  geom_ribbon(aes(x=yr, ymin=HP_Lwr_mo/171072000*100, ymax=HP_Upr_mo/171072000*100), alpha = .2, colour=NA)+
  geom_boxplot(aes(x=yr1, y=Value, color=id, group=con, stat="identity"), size=1.2)+
  facet_grid(Scenario ~ Time_period, scales="free_x")+
  labs(x="", y="Outflow [%]\n",
       title="Future turbined outflows")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 30, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        legend.position = "none",
        panel.spacing = unit(0.75, "lines"),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100,25))+
  scale_colour_manual(values=c(col_out_ros))+
  scale_fill_manual("", values=c("#fdae61","#fee090","#abd9e9", "#74add1"))+
  scale_x_continuous(breaks = unique(turb_boxplot2$yr1))+
  geom_blank(aes(x = x_min))+
  geom_blank(aes(x = x_max))

# turb_boxplot2_plot

# ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/S.Giustina_outflow_fut_boxplot(no_maxminconditions).png", plot=turb_boxplot,
#        width = 16,
#        height = 8,
#        dpi=600)

  
supervolume<-rbind(Q31_rcp45_50_m, Q31_rcp45_70_m, Q31_rcp85_50_m, Q31_rcp85_70_m) %>% 
  as_tibble() %>% 
  rename(Volume=Volume_lagged_mo) %>% 
  mutate(id=as.factor(id),
         Volume=Volume/159299502*100) %>% 
  gather(Variable, Value, Volume)

col_vol<-c("#a6dba0", "#5aae61", "#c2a5cf", "#9970ab")

# plot
p_vol<-supervolume %>% 
  rename(., Legend=Variable) %>% 
  ggplot(., aes(x=Date, y=Value, color=id, group=Scenario))+
  geom_line(size=1.1)+
  geom_ribbon(aes(x=Date, ymin=Vol_Lwr_mo/159299502*100, ymax=Vol_Upr_mo/159299502*100), alpha = .2, colour=NA)+
  geom_hline(yintercept=c(Q_vol10/159299502*100, Q_vol90/159299502*100), lty=2, cex=0.8)+
  facet_grid(Scenario ~ Time_period, scales="free_x")+
  labs(x="\nTime [Months]", y="Volume [%]\n",
       title="Future stored volume")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20))+
  scale_x_date(breaks = pretty_breaks(15), expand=c(0.01, 0))+
  scale_colour_manual(values=c(col_vol))

p_vol

# ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/S.Giustina_outflow_all2(no_maxminconditions).png", plot=p_vol,
#        width = 16,
#        height = 8,
#        dpi=600)

# boxplot 1 volume
vol_boxplot<-supervolume %>% 
  ggplot(aes(x=Date, y=Value, color=id, group=con, stat="identity"))+
  geom_boxplot(size=1)+
  geom_hline(yintercept=c(Q_vol10/159299502*100, Q_vol90/159299502*100), lty=2, cex=0.8)+
  facet_grid(Scenario ~ Time_period, scales="free_x")+
  labs(x="\nTime [Months]", y="Volume [%]\n",
       title="Future stored volume ")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20))+
  scale_x_date(breaks = pretty_breaks(15), expand=c(0.01, 0))+
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100,25))+
  scale_colour_manual(values=c(col_vol))+
  scale_fill_manual("", values=c("#fdae61","#fee090","#abd9e9", "#74add1"))

vol_boxplot

# ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/S.Giustina_volume_fut_boxplot(no_maxminconditions).png", plot=vol_boxplot,
#        width = 16,
#        height = 8, 
#        dpi=600)

# boxplot 2 volume 
vol_boxplot2<-rbind(Q31_rcp45_50_m, Q31_rcp45_70_m, Q31_rcp85_50_m, Q31_rcp85_70_m) %>%
  as_tibble() %>%
  rename(Volume=Volume_lag_mo) %>%
  mutate(id=as.factor(id),
         Volume=Volume/159299502*100) %>%
  gather(Legend, Value, c("Volume")) %>%
  mutate("10th"=Q_vol10/159299502*100,
         "20th"=Q_vol20/159299502*100,
         "80th"=Q_vol90/159299502*100,
         "90th percentile"=100) %>%
  gather(Legend2, Value2, c("10th","20th", "80th", "90th percentile")) %>%
  mutate(From=case_when(Legend2 =="10th" ~ 0,
                        Legend2 =="20th" ~ Q_vol10/159299502*100,
                        Legend2 =="80th" ~ Q_vol80/159299502*100,
                        Legend2 =="90th percentile" ~ Q_vol90/159299502*100)) %>% 
  dplyr::select(Date, yr, id, con, Scenario, Time_period, Legend, Value, Legend2, Value2, From) %>% 
  mutate(x_min=case_when(
           Time_period == "2021-2050" ~  2025,
           Time_period == "2041-2070" ~  2045),
         x_max=case_when(
           Time_period == "2021-2050" ~ 2045,
           Time_period == "2041-2070" ~ 2065),
         yr1=longsecc)



vol_boxplot2_plot<-ggplot(data=vol_boxplot2)+
  geom_ribbon(aes(x=yr, ymin=From, ymax=Value2, fill=Legend2), alpha=0.7, colour=NA)+
  geom_boxplot(aes(x=yr1, y=Value, color=id, group=con, stat="identity"), size=1.2)+
  facet_grid(Scenario ~ Time_period, scales="free_x")+
  labs(x="\nTime [Months]", y="Volume [%]\n",
       title="Future stored volume ")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 30, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        legend.position = "none",
        panel.spacing = unit(0.75, "lines"),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100,25))+
  scale_colour_manual(values=c(col_vol))+
  scale_fill_manual("", values=c("#fdae61","#fee090","#abd9e9", "#74add1"))+
  scale_x_continuous(breaks = unique(vol_boxplot2$yr1))+
  geom_blank(aes(x = x_min))+
  geom_blank(aes(x = x_max))

vol_boxplot2_plot


# boxplot inflow
superinflow<-rbind(Q31_rcp45_50_m, Q31_rcp45_70_m, Q31_rcp85_50_m, Q31_rcp85_70_m) %>% 
  as_tibble() %>% 
  rename(Inflow=Inflow_sim_mo) %>% 
  mutate(id=as.factor(id),
         Inflow=Inflow) %>% 
  gather(Variable, Value, Inflow)


col_inflo<-c("#9a9a9a", "#838383", "#676767", "#323232")
col_inflo2<-c("#9a9a9a", "#838383", "#676767", "#323232")


# plot time series
inflow_plot<-superinflow %>% 
  rename(., Legend=Variable) %>% 
  ggplot(., aes(x=Date, y=Value/10^6, color=id, group=Legend))+
  geom_line(size=1)+
  facet_grid(Scenario ~ Time_period, scales="free_x")+
  labs(x="", y="Inflow [Mm3/month]",
       title="Future inflows")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20))+
  scale_x_date(breaks = pretty_breaks(15), expand=c(0.01, 0))+ 
  scale_colour_manual(values=c(col_inflo))

inflow_plot


# boxplot without ribbons
inflow_boxplot<- superinflow %>%
  ggplot(aes(x=Date, y=Value/10^6, color=id, group=con, stat="identity"))+
  geom_boxplot(size=1)+
  facet_grid(Scenario ~ Time_period, scales="free_x")+
  labs(x="", y="Inflow [Mm3/month]\n",
       title="Future water inflows")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20))+
  scale_x_date(breaks = pretty_breaks(15), expand=c(0.01, 0))+
  scale_colour_manual(values=c(col_inflo))+
  scale_fill_manual(values=c(col_inflo))

inflow_boxplot

# boxplot with ribbons
inflow_boxplot2<-rbind(Q31_rcp45_50_m, Q31_rcp45_70_m, Q31_rcp85_50_m, Q31_rcp85_70_m) %>%
  as_tibble() %>%
  rename(Inflow=Inflow_sim_mo) %>%
  mutate(id=as.factor(id)) %>%
  gather(Legend, Value, c("Inflow")) %>%
  mutate("10th"=Q_in10/10^6,
         "20th"=Q_in20/10^6,
         "80th"=Q_in90/10^6,
         "90th percentile"=Q_in100/10^6) %>%
  gather(Legend2, Value2, c("10th","20th", "80th", "90th percentile")) %>%
  mutate(From=case_when(Legend2 =="10th" ~ 0,
                        Legend2 =="20th" ~ Q_in10/10^6,
                        Legend2 =="80th" ~ Q_in80/10^6,
                        Legend2 =="90th percentile" ~ Q_in90/10^6)) %>%
  dplyr::select(Date, yr, id, con, Scenario, Time_period, Legend, Value, Legend2, Value2, From) %>% 
  mutate(x_min=case_when(
    Time_period == "2021-2050" ~  2025,
    Time_period == "2041-2070" ~  2045),
    x_max=case_when(
      Time_period == "2021-2050" ~ 2045,
      Time_period == "2041-2070" ~ 2065),
    yr1=longsecc)


inflow_boxplot2_plot<-ggplot(data=inflow_boxplot2)+
  geom_ribbon(aes(x=yr, ymin=From, ymax=Value2, fill=Legend2), alpha=0.7, colour=NA)+
  geom_boxplot(aes(x=yr1, y=Value/10^6, color=id, group=con, stat="identity"), size=1.2)+
  facet_grid(Scenario ~ Time_period, scales="free_x")+
  labs(x="", y="Inflow [Mm3/month]\n",
       title="Future inflows")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 30, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        legend.position = "none",
        panel.spacing = unit(0.75, "lines"),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_colour_manual(values=c(col_inflo2))+
  scale_fill_manual("", values=c("#fdae61","#fee090","#abd9e9", "#74add1"))+
  scale_x_continuous(breaks = unique(vol_boxplot2$yr1))+
  geom_blank(aes(x = x_min))+
  geom_blank(aes(x = x_max))



# ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/Water_balance/S.Giustina_inflow_fut_boxplot.png", plot=inflow_boxplot2,
#        width = 16,
#        height = 8,
#        dpi=600)

#### 3 figure together (Figure 5) ####
fig5<-plot_grid(inflow_boxplot2_plot, turb_boxplot2_plot, vol_boxplot2_plot,
                labels=c("A", "B", "C"), label_size = 26, 
                ncol = 1, nrow = 3)

save_plot("~/R/Noce/HydroNoce/S.Giustina/02_images/Water_balance/fig5_3boxplots_6.png", fig5, 
          ncol = 1, nrow = 2,
          base_height = 10, base_width = 14)

#### Outflow monthly variation ####

datasan_past<-santemp_day2_mo %>% 
  dplyr::select(Date, yr, mo, Inflow_sim_mo, Out_lmer_mo, Volume_pred_mo) %>% 
  group_by(mo) %>% 
  mutate(Outflow_meanmo=mean(Out_lmer_mo, na.rm=T)) %>% 
  distinct(mo, Outflow_meanmo)

# for each predicted volume scenario
#1
out1_mo<-Q31_rcp45_50_m %>%
  dplyr::select(Date, yr, mo, Outflow_HP_mo) %>%
  group_by(mo) %>%
  mutate(Outflow_HP1_mo_meanmo=mean(Outflow_HP_mo, na.rm=T)) %>%
  distinct(mo, Outflow_HP1_mo_meanmo)

#2
out2_mo<-Q31_rcp45_70_m %>%
  dplyr::select(Date, yr, mo, Outflow_HP_mo) %>%
  group_by(mo) %>%
  mutate(Outflow_HP2_mo_meanmo=mean(Outflow_HP_mo, na.rm=T)) %>%
  distinct(mo, Outflow_HP2_mo_meanmo)

#3
out3_mo<-Q31_rcp85_50_m %>%
  dplyr::select(Date, yr, mo, Outflow_HP_mo) %>%
  group_by(mo) %>%
  mutate(Outflow_HP3_mo_meanmo=mean(Outflow_HP_mo, na.rm=T)) %>%
  distinct(mo, Outflow_HP3_mo_meanmo)

#4
out4_mo<-Q31_rcp85_70_m %>%
  dplyr::select(Date, yr, mo, Outflow_HP_mo) %>%
  group_by(mo) %>%
  mutate(Outflow_HP4_mo_meanmo=mean(Outflow_HP_mo, na.rm=T)) %>%
  distinct(mo, Outflow_HP4_mo_meanmo)

out_month<-merge(out1_mo, out2_mo, by="mo") %>% 
  merge(., out3_mo, by="mo") %>% 
  merge(., out4_mo, by="mo") %>% 
  merge(., datasan_past, by="mo") %>% 
  mutate("RCP4.5 2021-2050"=(Outflow_HP1_mo_meanmo-Outflow_meanmo)/Outflow_meanmo*100,
         "RCP4.5 2041-2070"=(Outflow_HP2_mo_meanmo-Outflow_meanmo)/Outflow_meanmo*100,
         "RCP8.5 2021-2050"=(Outflow_HP3_mo_meanmo-Outflow_meanmo)/Outflow_meanmo*100,
         "RCP8.5 2041-2070"=(Outflow_HP4_mo_meanmo-Outflow_meanmo)/Outflow_meanmo*100) %>% 
  dplyr::select(mo, "RCP4.5 2021-2050","RCP4.5 2041-2070", "RCP8.5 2021-2050", "RCP8.5 2041-2070") %>% 
  mutate(mo_nom=as.factor(month.abb[mo])) %>% 
  gather(Variable, Value, c(2:5)) %>% 
  rename(Legend=Variable) %>% 
  as_tibble()

# plot
out_month_plot<-ggplot()+
  geom_bar(data=out_month, aes(x=mo_nom, y=Value, fill=Legend),
           stat="identity", width=0.7, position = position_dodge(width=0.7))+
  labs(x="\nTime [Months]", y="Outflow differences [%]",
       title="Comparison of past and future turbined water outflows")+
  theme_bw()+
  theme(axis.text.x=element_text(hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        panel.spacing = unit(2, "lines"),
        plot.caption = element_text(size=14),
        legend.position="bottom",
        legend.box.margin=margin(5,5,5,5),
        legend.box.background = element_rect(colour = "black", size=0.5))+
  scale_x_discrete(limits = month.abb)+
  scale_fill_manual(values=c(col_out_ros))

# visualization
out_month_plot

# ggplotly
library("plotly")
ggplotly(out_month_plot)

# saving plot
ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/Water_balance/S.Giustina_outflow_differences2.png", plot=out_month_plot,
       width = 16,
       height = 8,
       dpi=600)

#### Volume monthly variation ####

datasan_past2<-santemp_day2_mo %>% 
  dplyr::select(Date, yr, mo, Inflow_sim_mo, Out_lmer_mo, Volume_pred_mo) %>% 
  group_by(mo) %>% 
  mutate(Volume_meanmo=mean(Volume_pred_mo, na.rm=T)) %>% 
  distinct(mo, Volume_meanmo)

# for each predicted volume scenario
#1
dam1_mo<-Q31_rcp45_50_m %>%
  dplyr::select(Date, yr, mo, Volume_lagged_mo) %>%
  group_by(mo) %>%
  mutate(vol_fut45.2050_meanmo=mean(Volume_lagged_mo, na.rm=T)) %>%
  distinct(mo, vol_fut45.2050_meanmo)

#2
dam2_mo<-Q31_rcp45_70_m %>%
  dplyr::select(Date, yr, mo, Volume_lagged_mo) %>%
  group_by(mo) %>%
  mutate(vol_fut45.2070_meanmo=mean(Volume_lagged_mo, na.rm=T)) %>%
  distinct(mo, vol_fut45.2070_meanmo)

#3
dam3_mo<-Q31_rcp85_50_m %>%
  dplyr::select(Date, yr, mo, Volume_lagged_mo) %>%
  group_by(mo) %>%
  mutate(vol_fut85.2050_meanmo=mean(Volume_lagged_mo, na.rm=T)) %>%
  distinct(mo, vol_fut85.2050_meanmo)

#4
dam4_mo<-Q31_rcp85_70_m %>%
  dplyr::select(Date, yr, mo, Volume_lagged_mo) %>%
  group_by(mo) %>%
  mutate(vol_fut85.2070_meanmo=mean(Volume_lagged_mo, na.rm=T)) %>%
  distinct(mo, vol_fut85.2070_meanmo)

vol_month<-merge(dam1_mo, dam2_mo, by="mo") %>% 
  merge(., dam3_mo, by="mo") %>% 
  merge(., dam4_mo, by="mo") %>% 
  merge(., datasan_past2, by="mo") %>% 
  mutate("RCP4.5 2021-2050"=(vol_fut45.2050_meanmo-Volume_meanmo)/Volume_meanmo*100,
         "RCP4.5 2041-2070"=(vol_fut45.2070_meanmo-Volume_meanmo)/Volume_meanmo*100,
         "RCP8.5 2021-2050"=(vol_fut85.2050_meanmo-Volume_meanmo)/Volume_meanmo*100,
         "RCP8.5 2041-2070"=(vol_fut85.2070_meanmo-Volume_meanmo)/Volume_meanmo*100) %>% 
  dplyr::select(mo, "RCP4.5 2021-2050","RCP4.5 2041-2070", "RCP8.5 2021-2050", "RCP8.5 2041-2070") %>% 
  mutate(mo_nom=as.factor(month.abb[mo])) %>% 
  gather(Variable, Value, c(2:5)) %>% 
  rename(Legend=Variable) %>%
  as_tibble()

# plot
vol_diff<-ggplot()+
  geom_bar(data=vol_month, aes(x= mo_nom, y=Value, fill=Legend),
           stat="identity", width=0.7, position = position_dodge(width=0.7))+
  labs(x="\nTime [Months]", y="Volume differences [%]",
       title="Comparison of past and future water volumes stored")+
  theme_bw()+
  theme(axis.text.x=element_text(hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        panel.spacing = unit(2, "lines"),
        plot.caption = element_text(size=14),
        legend.position="bottom",
        legend.box.margin=margin(5,5,5,5),
        legend.box.background = element_rect(colour = "black", size=0.5))+
  scale_x_discrete(limits = month.abb)+
  scale_fill_manual(values=c(col_vol))


# visualization
vol_diff

ggplotly(vol_diff)

# saving plot
ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/Water_balance/S.Giustina_volume_differences2.png", plot=vol_diff,
       width = 16,
       height = 8,
       dpi=600)

#### Non-parametric test ####

# comparing two means on the whole time series
dam1_mo2<-Q31_rcp45_50_m %>%
  dplyr::select(Date, yr, mo, Volume_lagged_mo) 

dam2_mo2<-Q31_rcp45_70_m %>%
  dplyr::select(Date, yr, mo, Volume_lagged_mo) 

dam3_mo2<-Q31_rcp85_50_m %>%
  dplyr::select(Date, yr, mo, Volume_lagged_mo) 

dam4_mo2<-Q31_rcp85_70_m %>%
  dplyr::select(Date, yr, mo, Volume_lagged_mo) 

# test comparing the whole two time series
# dam1_mo2, dam2_mo2, dam3_mo2, dam4_mo2

dam_past1<-santemp_day2_mo %>%
  ungroup() %>% 
  dplyr::select(Volume_pred_mo) %>% 
  rename("Baseline_1999_2004&2009_2016"=Volume_pred_mo) %>% 
  as.matrix()


wilcox.test(dam_past1[,1], as.matrix(dam1_mo2[,4]))[3]
wilcox.test(dam_past1[,1], as.matrix(dam2_mo2[,4]))[3]
wilcox.test(dam_past1[,1], as.matrix(dam3_mo2[,4]))[3]
wilcox.test(dam_past1[,1], as.matrix(dam4_mo2[,4]))[3]

# test at monthly level only
vol_month2<-merge(dam1_mo, dam2_mo, by="mo") %>% 
  merge(., dam3_mo, by="mo") %>% 
  merge(., dam4_mo, by="mo") %>% 
  merge(., datasan_past2, by="mo") 


wilcox.test(vol_month2[,2], vol_month2[,6], paired=TRUE)

wilcox.test(vol_month2[,3], vol_month2[,6], paired=TRUE)

wilcox.test(vol_month2[,4], vol_month2[,6], paired=TRUE)

wilcox.test(vol_month2[,5], vol_month2[,6], paired=TRUE)



# comparing two means on the whole time series
out1_mo2<-Q31_rcp45_50_m %>%
  dplyr::select(Date, yr, mo, Outflow_HP_mo) 

out2_mo2<-Q31_rcp45_70_m %>%
  dplyr::select(Date, yr, mo, Outflow_HP_mo) 

out3_mo2<-Q31_rcp85_50_m %>%
  dplyr::select(Date, yr, mo, Outflow_HP_mo) 

out4_mo2<-Q31_rcp85_70_m %>%
  dplyr::select(Date, yr, mo, Outflow_HP_mo) 


# test comparing the whole two time series
out_past1<-santemp_day2_mo %>%
  ungroup() %>% 
  dplyr::select(Out_lmer_mo) %>% 
  rename("Baseline_1999_2004&2009_2016"=Out_lmer_mo) %>% 
  as.matrix()

wilcox.test(out_past1[,1], as.matrix(out1_mo2[,4]))
wilcox.test(out_past1[,1], as.matrix(out2_mo2[,4]))
wilcox.test(out_past1[,1], as.matrix(out3_mo2[,4]))
wilcox.test(out_past1[,1], as.matrix(out4_mo2[,4]))



datasan_outpast2<-santemp_day2_mo %>% 
  dplyr::select(Date, yr, mo, Inflow_sim_mo, Out_lmer_mo, Volume_pred_mo) %>% 
  group_by(mo) %>% 
  mutate(Out_lmer_mo_meanmo=mean(Out_lmer_mo, na.rm=T)) %>% 
  distinct(mo, Out_lmer_mo_meanmo)

# test at monthly level only
out_month2<-merge(out1_mo, out2_mo, by="mo") %>% 
  merge(., out3_mo, by="mo") %>% 
  merge(., out4_mo, by="mo") %>% 
  merge(., datasan_outpast2, by="mo")


wilcox.test(out_month2[,2], out_month2[,6], paired=TRUE)
wilcox.test(out_month2[,3], out_month2[,6], paired=TRUE)
wilcox.test(out_month2[,4], out_month2[,6], paired=TRUE)
wilcox.test(out_month2[,5], out_month2[,6], paired=TRUE)
