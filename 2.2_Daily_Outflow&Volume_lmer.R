

#*******************************************************************************
# # This script to carry out linear mixed effect regression of past turbined water
# # in S.Giustina and compute the stored volume applying the water balance equation

#*******************************************************************************

#### Libraries ####
library("dplyr")
library("ggplot2")
library("tibble")
library("tidyr")
library("lubridate")
library("MuMIn")
library("caret")
library("merTools")
library("scales")


# Daily dataset
sgiudaily<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/Vulnerability/Daily_balance_sgiu") %>% 
  dplyr::select(Date, yr, mo, Outflow_HP, Outflow_MEF, Outflow_rel, Volume, Inflow_sim) %>% 
  mutate(Volume_lag=lag(Volume)*10^6,
         Inflow_sim=Inflow_sim*86400,
         Outflow_HP=Outflow_HP*86400,
         Outflow_MEF=Outflow_MEF*86400,
         Outflow_rel=Outflow_rel*86400,
         Inflow_sim_lag=lag(Inflow_sim),
         Volume=Volume*10^6,
         Date=as.Date(Date),
         WD=weekdays(Date)) %>% 
  as_tibble()


#### Multi-model comparison with moving window ####
santemp_day$id<-seq(1:nrow(santemp_day))

santemp_day<-santemp_day %>% 
  mutate(WD=weekdays(Date))

# list for RMSE
li1<-list()

# list for R-squared
li2<-list()

# list for final results
li_res<-list()


for(i in 3650:(nrow(santemp_day)-1)){
  
  train = subset(santemp_day, id <= i)
  test = subset(santemp_day, id > i)
  
  li_pred<-list(model_lmer1 = lme4::lmer(Outflow_HP ~ Inflow_sim + (1|WD) + (1|mo), train),
                model_lmer2 = lme4::lmer(Outflow_HP ~ Inflow_sim_lag + (1|WD) + (1|mo), train),
                model_lmer3 = lme4::lmer(Outflow_HP ~ Inflow_sim + Volume + (1|WD) + (1|mo), train),
                model_lmer4 = lme4::lmer(Outflow_HP ~ Inflow_sim_lag + Volume + (1|WD) + (1|mo), train),
                model_lmer5 = lme4::lmer(Outflow_HP ~ Inflow_sim + Volume_lag + (1|WD) + (1|mo), train),
                model_lmer6 = lme4::lmer(Outflow_HP ~ Inflow_sim_lag + Volume_lag + (1|WD) + (1|mo), train))
  
  test<-cbind.data.frame(test, predict(li_pred, test, allow.new.levels=T))
  
  # RMSE
  li1[i-3649]<-list(cbind(caret::postResample(test$Outflow_HP, test$model_lmer1),
                         caret::postResample(test$Outflow_HP, test$model_lmer2),
                         caret::postResample(test$Outflow_HP, test$model_lmer3),
                         caret::postResample(test$Outflow_HP, test$model_lmer4),
                         caret::postResample(test$Outflow_HP, test$model_lmer5),
                         caret::postResample(test$Outflow_HP, test$model_lmer6)))
  
  li_res[i-3649]<-test %>% 
    dplyr::select(Date, mo, Inflow_sim, Outflow_HP, model_lmer1, 
                  model_lmer2, model_lmer3, model_lmer4, model_lmer5, model_lmer6) %>% 
    list()
  
  print((nrow(santemp_day)-1)-i)
}

# storing RMSE
rmse2.2<-do.call(rbind,lapply(li1, `[`,1,))


# storing R-squared
r2.2<-do.call(rbind,lapply(li1, `[`,2,))
r2.2<-r2.2[-c(nrow(r2.2)-1,nrow(r2.2)), ]

# storing results in a dataframe
moving_res2.2<-do.call(rbind, li_res)

# averaging R-squared
r2.2 %>% 
  as.data.frame() %>% 
  summarise(R2_lmer1=mean(V1, na.rm=T),
            R2_lmer2=mean(V2, na.rm=T),
            R2_lmer3=mean(V3, na.rm=T),
            R2_lmer4=mean(V4, na.rm=T),
            R2_lmer5=mean(V5, na.rm=T),
            R2_lmer6=mean(V6, na.rm=T)
            )

# averaging RMSE
rmse2.2 %>% 
  as.data.frame() %>% 
  summarise(rmse_lmer1=mean(V1, na.rm=T),
            rmse_lmer2=mean(V2, na.rm=T),
            rmse_lmer3=mean(V3, na.rm=T),
            rmse_lmer4=mean(V4, na.rm=T),
            rmse_lmer5=mean(V5, na.rm=T),
            rmse_lmer6=mean(V6, na.rm=T)
            )

# plotting observed and predicted values
moving_res2.2 %>% 
  group_by(Date) %>% 
  mutate(predict_lmer1_avrg=mean(model_lmer1),
         predict_lmer2_avrg=mean(model_lmer2),
         predict_lmer3_avrg=mean(model_lmer3),
         predict_lmer4_avrg=mean(model_lmer4),
         predict_lmer5_avrg=mean(model_lmer5),
         predict_lmer6_avrg=mean(model_lmer6)
         ) %>%
  distinct(Date, , .keep_all = T) %>% 
  dplyr::select(Date, Outflow_HP, predict_lmer1_avrg, 
                predict_lmer2_avrg, predict_lmer3_avrg, predict_lmer4_avrg, predict_lmer5_avrg, predict_lmer6_avrg) %>%
  gather(Variable, Value, c(2:4)) %>%
  ggplot(., aes(x=Date, y=Value, color=Variable))+
  geom_line()+
  geom_point()


# at monthly level
moving_res_mo2.2<-moving_res2.2 %>% 
  group_by(Date) %>% 
  mutate(predict_lmer1_avrg=mean(model_lmer1),
         predict_lmer2_avrg=mean(model_lmer2),
         predict_lmer3_avrg=mean(model_lmer3),
         predict_lmer4_avrg=mean(model_lmer4),
         predict_lmer5_avrg=mean(model_lmer5),
         predict_lmer6_avrg=mean(model_lmer6)
         yr=year(Date),
         mo=month(Date)) %>%
  distinct(Date, , .keep_all = T) %>% 
  dplyr::select(Date, yr, mo, Outflow_HP, predict_lmer1_avrg, 
                predict_lmer2_avrg, predict_lmer3_avrg, predict_lmer4_avrg, predict_lmer5_avrg, predict_lmer6_avrg,
                predict_lmer7_avrg
                ) %>% 
  group_by(yr, mo) %>%
  mutate(Outflow_HP_mo=sum(Outflow_HP, na.rm=T),
         predict_lmer1_mo=sum(predict_lmer1_avrg, na.rm = T),
         predict_lmer2_mo=sum(predict_lmer2_avrg, na.rm = T),
         predict_lmer3_mo=sum(predict_lmer3_avrg, na.rm = T),
         predict_lmer4_mo=sum(predict_lmer4_avrg, na.rm = T),
         predict_lmer5_mo=sum(predict_lmer5_avrg, na.rm = T),
         predict_lmer6_mo=sum(predict_lmer6_avrg, na.rm = T)) %>% 
  distinct(yr, mo , .keep_all = T) %>% 
  dplyr::select(Date, yr, mo, Outflow_HP_mo, predict_lmer1_mo, 
                predict_lmer2_mo, predict_lmer3_mo, predict_lmer4_mo, predict_lmer5_mo, predict_lmer6_mo) 

caret::RMSE(moving_res_mo2.2$Outflow_HP_mo, moving_res_mo2.2$predict_lmer5_mo, na.rm=T)
caret::R2(moving_res_mo2.2$Outflow_HP_mo, moving_res_mo2.2$predict_lmer5_mo, na.rm=T)

moving_res_mo2.2 %>% 
  gather(Variable, Value, c(4:10)) %>%
  ggplot(., aes(x=Date, y=Value, color=Variable))+
  geom_line()+
  geom_point()

#### Fitting all data ####
model_lmer1 = lme4::lmer(Outflow_HP ~ Inflow_sim + (1|WD) + (1|mo), santemp_day)
model_lmer2 = lme4::lmer(Outflow_HP ~ Inflow_sim_lag + (1|WD) + (1|mo), santemp_day)
model_lmer3 = lme4::lmer(Outflow_HP ~ Inflow_sim + Volume + (1|WD) + (1|mo), santemp_day)
model_lmer4 = lme4::lmer(Outflow_HP ~ Inflow_sim_lag + Volume + (1|WD) + (1|mo), santemp_day)
model_lmer5 = lme4::lmer(Outflow_HP ~ Inflow_sim + Volume_lag + (1|WD) + (1|mo), santemp_day)
model_lmer6 = lme4::lmer(Outflow_HP ~ Inflow_sim_lag + Volume_lag + (1|WD) + (1|mo), santemp_day)


santemp_day$Out_lmer<-predict(model_lmer5, santemp_day)

# 
santemp_day$Out_lmer1<-predict(model_lmer1, santemp_day)
santemp_day$Out_lmer2<-predict(model_lmer2, santemp_day)
santemp_day$Out_lmer3<-predict(model_lmer3, santemp_day)
santemp_day$Out_lmer4<-predict(model_lmer4, santemp_day)
santemp_day$Out_lmer5<-predict(model_lmer5, santemp_day)
santemp_day$Out_lmer6<-predict(model_lmer6, santemp_day)

# performance over the whole dataset
caret::postResample(santemp_day$Outflow_HP, santemp_day$Out_lmer)
caret::RMSE(santemp_day$Outflow_HP, santemp_day$Out_lmer, na.rm=T)


#### Volume Water Balance ####
santemp_day2<-santemp_day %>% 
  mutate(day=day(Date)) %>% 
  dplyr::select(Date, yr, mo, WD, day, Inflow_sim, Outflow_MEF, Outflow_rel, Volume, Volume_lag, Outflow_HP, Out_lmer, Out_lmer1, 
                Out_lmer2, Out_lmer3, Out_lmer4, Out_lmer6)

# average daily MEF
mef_past_day<-santemp_day2 %>%
  filter(yr>=2009) %>% 
  mutate(day=day(Date)) %>% 
  group_by(mo, day) %>% 
  summarise(MEF_d=mean(Outflow_MEF, na.rm=T)) 

# emptying columns which values will be computed
santemp_day2$Volume_lag<-lag(santemp_day2$Volume)
santemp_day2$Volume_lag[3:nrow(santemp_day2)]<-NA

santemp_day2$Outflow_rel2<-NA

santemp_day2<-santemp_day2 %>%
  right_join(., mef_past_day, by=c("mo", "day")) %>% 
  mutate(MEF_d=ifelse(yr>=2009, MEF_d, 181440))
  
santemp_day2$Out_lmer[4:nrow(santemp_day2)]<-NA

santemp_day2$R2_vol<-0
santemp_day2$RMSE_vol<-0
santemp_day2$R2_outflow<-0
santemp_day2$RMSE_outflow<-0

for (i in 3:(nrow(santemp_day2)-1)){
  
  santemp_day2$Volume_lag[i]<-sum(santemp_day2$Volume_lag[i-1], santemp_day2$Inflow_sim[i],
                                  -santemp_day2$Out_lmer[i], -santemp_day2$MEF_d[i], -santemp_day2$Outflow_rel2[i],
                                na.rm=T)
  
  santemp_day2$Out_lmer[i+1]<-predict(model_lmer5, santemp_day2[i,])
  
  santemp_day2$Outflow_rel2[i+1]<-ifelse(santemp_day2$Volume_lag[i]>=159299502, 14561942, 0)
  
  # storing performance at each time step for volume and outflow
  santemp_day2$R2_vol[i]<-caret::R2(santemp_day2$Volume[3:i], santemp_day2$Volume_lag[3:i], na.rm=T)
  santemp_day2$RMSE_vol[i]<-caret::RMSE(santemp_day2$Volume[3:i], santemp_day2$Volume_lag[3:i], na.rm=T)
  
  santemp_day2$R2_outflow[i]<-caret::R2(santemp_day2$Outflow_HP[3:i], santemp_day2$Out_lmer[3:i], na.rm=T)
  santemp_day2$RMSE_outflow[i]<-caret::RMSE(santemp_day2$Outflow_HP[3:i], santemp_day2$Out_lmer[3:i], na.rm=T)
  
  print(((nrow(santemp_day2)-1)-i)/(nrow(santemp_day2)-1)*100)
  
  
}

# setting values of R2 and RMSE to zero for dataset smaller than 3650 (as for the turbined outflow)
santemp_day2$R2_vol[3:3650]<-NA
santemp_day2$RMSE_vol[3:3650]<-NA
santemp_day2$R2_outflow[3:3650]<-NA
santemp_day2$RMSE_outflow[3:3650]<-NA


# mean value of R2
mean(santemp_day2$R2_vol, na.rm=T)
mean(santemp_day2$RMSE_vol, na.rm=T)
mean(santemp_day2$RMSE_vol, na.rm=T)/mean(santemp_day2$Volume, na.rm=T)

mean(santemp_day2$R2_outflow, na.rm=T)
mean(santemp_day2$RMSE_outflow, na.rm=T)
mean(santemp_day2$RMSE_outflow, na.rm=T)/mean(santemp_day2$Volume, na.rm=T)


a<-predictInterval(model_lmer5, santemp_day2, level = 0.80)

santemp_day2$HP_Lwr<-a$lwr
santemp_day2$HP_Upr<-a$upr

santemp_day2$Volume_lagged=lag(santemp_day2$Volume_lag) 
santemp_day2$Volume_lagged[1]=santemp_day2$Volume_lag[1]

santemp_day2<-santemp_day2 %>% 
  rowwise() %>% 
  mutate(Vol_Lwr=sum(Volume_lagged, Inflow_sim, -HP_Upr, -Outflow_MEF, -Outflow_rel, na.rm=T),
         Vol_Upr=sum(Volume_lagged, Inflow_sim, -HP_Lwr, -Outflow_MEF, -Outflow_rel, na.rm=T))

# daily plot
santemp_day2 %>% 
  dplyr::select(Date, Outflow_HP, Out_lmer) %>% 
  gather(Variable, Value, c("Outflow_HP", "Out_lmer")) %>% 
  ggplot()+
  geom_line(aes(x=Date, y=Value, color=Variable))+
  theme_bw()

santemp_day2 %>% 
  dplyr::select(Date, Volume, Volume_lag) %>% 
  gather(Variable, Value, c("Volume", "Volume_lag")) %>% 
  ggplot()+
  geom_line(aes(x=Date, y=Value, color=Variable))+
  theme_bw()
  
# performance over the whole dataset
caret::R2(santemp_day2$Volume, santemp_day2$Volume_lag, na.rm=T)
caret::RMSE(santemp_day2$Volume, santemp_day2$Volume_lag, na.rm=T)

caret::R2(santemp_day2$Outflow_HP, santemp_day2$Out_lmer, na.rm=T)
caret::RMSE(santemp_day2$Outflow_HP, santemp_day2$Out_lmer, na.rm=T)

# average daily Outflow_HP
HP_past_day<-santemp_day2 %>%
  # filter(yr>=2009) %>% 
  mutate(day=day(Date)) %>% 
  group_by(mo, day) %>% 
  summarise(Outflow_HP_d=mean(Outflow_HP, na.rm=T)) 

santemp_day2_mo <-santemp_day2 %>% 
  group_by(yr, mo) %>% 
  mutate(Outflow_HP_mo=sum(Outflow_HP, na.rm=T),
         Out_lmer_mo=sum(Out_lmer, na.rm=T),
         Out_lmer1_mo=sum(Out_lmer1, na.rm=T),
         Out_lmer2_mo=sum(Out_lmer2, na.rm=T),
         Out_lmer3_mo=sum(Out_lmer3, na.rm=T),
         Out_lmer4_mo=sum(Out_lmer4, na.rm=T),
         Out_lmer6_mo=sum(Out_lmer6, na.rm=T),
         Inflow_sim_mo=sum(Inflow_sim, na.rm=T),
         HP_Upr_mo=sum(HP_Upr, na.rm=T),
         HP_Lwr_mo=sum(HP_Lwr, na.rm=T),
         MEF_mo=sum(Outflow_MEF, na.rm=T),
         Outflow_rel_mo=sum(Outflow_rel, na.rm=T),
         Volume_mo=mean(Volume, na.rm=T),
         Volume_pred_mo=mean(Volume_lag, na.rm=T)) %>% 
  distinct(yr, mo, .keep_all=T)

caret::RMSE(santemp_day2_mo$Volume_mo, santemp_day2_mo$Volume_pred_mo, na.rm=T)
caret::RMSE(santemp_day2_mo$Outflow_HP_mo, santemp_day2_mo$Out_lmer_mo, na.rm=T)

caret::RMSE(santemp_day2_mo$Volume_mo, santemp_day2_mo$Volume_pred_mo, na.rm=T)
caret::RMSE(santemp_day2_mo$Outflow_HP_mo, santemp_day2_mo$Out_lmer_mo, na.rm=T)

caret::R2(santemp_day2_mo$Volume_mo, santemp_day2_mo$Volume_pred_mo, na.rm=T)
caret::R2(santemp_day2_mo$Outflow_HP_mo, santemp_day2_mo$Out_lmer_mo, na.rm=T)

# for other models
caret::R2(santemp_day2_mo$Outflow_HP_mo, santemp_day2_mo$Out_lmer1_mo, na.rm=T)
caret::RMSE(santemp_day2_mo$Outflow_HP_mo, santemp_day2_mo$Out_lmer1_mo, na.rm=T)

caret::R2(santemp_day2_mo$Outflow_HP_mo, santemp_day2_mo$Out_lmer2_mo, na.rm=T)
caret::RMSE(santemp_day2_mo$Outflow_HP_mo, santemp_day2_mo$Out_lmer2_mo, na.rm=T)

caret::R2(santemp_day2_mo$Outflow_HP_mo, santemp_day2_mo$Out_lmer3_mo, na.rm=T)
caret::RMSE(santemp_day2_mo$Outflow_HP_mo, santemp_day2_mo$Out_lmer3_mo, na.rm=T)

caret::R2(santemp_day2_mo$Outflow_HP_mo, santemp_day2_mo$Out_lmer4_mo, na.rm=T)
caret::RMSE(santemp_day2_mo$Outflow_HP_mo, santemp_day2_mo$Out_lmer4_mo, na.rm=T)

caret::R2(santemp_day2_mo$Outflow_HP_mo, santemp_day2_mo$Out_lmer6_mo, na.rm=T)
caret::RMSE(santemp_day2_mo$Outflow_HP_mo, santemp_day2_mo$Out_lmer6_mo, na.rm=T)

#

santemp_day2_mo$Volume_lagged_mo=lag(santemp_day2_mo$Volume_pred_mo)
santemp_day2_mo$Volume_lagged_mo[1]<-santemp_day2_mo$Volume_pred_mo[1]

santemp_day2_mo<-santemp_day2_mo %>% 
  rowwise() %>% 
  mutate(Vol_Lwr_mo=sum(Volume_lagged_mo, Inflow_sim_mo, -HP_Upr_mo, -MEF_mo, -Outflow_rel_mo, na.rm=T),
         Vol_Upr_mo=sum(Volume_lagged_mo, Inflow_sim_mo, -HP_Lwr_mo, -MEF_mo, -Outflow_rel_mo, na.rm=T))

santemp_day2_mo %>% 
  dplyr::select(Date, Vol_Lwr_mo, Vol_Upr_mo, Volume_mo, Volume_pred_mo) %>% 
  gather(Variable, Value, c("Volume_mo", "Volume_pred_mo")) %>% 
  ggplot()+
  geom_line(aes(x=Date, y=Value/159299502*100, color=Variable))+
  geom_ribbon(aes(x=Date, ymin=Vol_Lwr_mo/159299502*100, ymax=Vol_Upr_mo/159299502*100), alpha=0.2, colour=NA)

santemp_day2_mo<-santemp_day2_mo %>% 
  dplyr::select(Date, yr, mo, Inflow_sim_mo, Outflow_HP_mo, Out_lmer_mo, HP_Upr_mo, 
                HP_Lwr_mo, MEF_mo, Outflow_rel_mo, Vol_Lwr_mo, Vol_Upr_mo, Volume_mo, Volume_pred_mo)

saveRDS(santemp_day2_mo, "S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Baseline/Baseline_sgiustina_mo.rds")
santemp_day2_mo<-readRDS("S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Baseline/Baseline_sgiustina_mo.rds")

# average monthly MEF
mef_past_mo<-santemp_day2_mo %>%
  filter(yr>=2009) %>% 
  group_by(mo) %>% 
  summarise(MEF=mean(MEF_mo, na.rm=T))

# percentiles over the whole past time series for past turbined values
Q_out10<-quantile(santemp_day2_mo$Out_lmer_mo, 0.10, na.rm=T)
Q_out20<-quantile(santemp_day2_mo$Out_lmer_mo, 0.20, na.rm=T)

Q_out80<-quantile(santemp_day2_mo$Out_lmer_mo, 0.80, na.rm=T)
Q_out90<-quantile(santemp_day2_mo$Out_lmer_mo, 0.90, na.rm=T)

# percentiles over the whole past time series for past volume values 
Q_vol10<-quantile(santemp_day2_mo$Volume_pred_mo, 0.10, na.rm=T)
Q_vol20<-quantile(santemp_day2_mo$Volume_pred_mo, 0.20, na.rm=T)

Q_vol80<-quantile(santemp_day2_mo$Volume_pred_mo, 0.80, na.rm=T)
Q_vol90<-quantile(santemp_day2_mo$Volume_pred_mo, 0.90, na.rm=T)



# Plotting Outflow and Volume #### 
full_dates<-data.frame(Date=seq(as.Date(min(santemp_day2_mo$Date)), as.Date(max(santemp_day2_mo$Date)), by="months"), x=NA)

full_datasan<-merge(santemp_day2_mo, full_dates, by="Date", all = T)

# setting quantiles based on months 
full_datasan2<-full_datasan %>% 
  group_by(mo) %>% 
  mutate(Average_mo=mean(Volume_mo),
"Very high volume"=(quantile(Volume_mo, 1.0, na.rm=T)/159299502)*100,
"High volume"=(quantile(Volume_mo, 0.75, na.rm=T)/159299502)*100,
"Normal volume"=(quantile(Volume_mo, 0.50, na.rm=T)/159299502)*100,
"Low volume"=(quantile(Volume_mo, 0.25, na.rm=T)/159299502)*100,
"Very low volume"=(quantile(Volume_mo, 0.0, na.rm=T)/159299502)*100,
"Very high outflow"=(quantile(Outflow_HP_mo, 1.0, na.rm=T)/171072000)*100,
"High outflow"=(quantile(Outflow_HP_mo, 0.75, na.rm=T)/171072000)*100,
"Normal outflow"=(quantile(Outflow_HP_mo, 0.50, na.rm=T)/171072000)*100,
"Low outflow"=(quantile(Outflow_HP_mo, 0.25, na.rm=T)/171072000)*100,
"Very low outflow"=(quantile(Outflow_HP_mo, 0.0, na.rm=T)/171072000)*100,
)


a<-full_datasan2 %>% 
  dplyr::select(Date, Volume_mo, Volume_pred_mo, "Very high volume", "High volume", "Normal volume", "Low volume", "Very low volume") %>%
  mutate(Volume_mo=(Volume_mo/159299502)*100,
         Volume_pred_mo=(Volume_pred_mo/159299502)*100) %>%
  rename("Real volume"=Volume_mo,
         "Modelled volume"=Volume_pred_mo) %>%
  mutate(Scenario="Baseline") %>% 
  gather(Legend, Value, c("Real volume", "Modelled volume"))

b<-full_datasan2 %>% 
  dplyr::select(Date, "Very high volume", "High volume", "Normal volume", "Low volume", "Very low volume") %>%
  mutate(Scenario="Baseline") 

b1<-b %>% 
  gather(Status, Value, c("Very high volume", "High volume", "Normal volume", "Low volume", "Very low volume")) %>% 
  mutate(Status=factor(Status, levels=c("Very high volume", "High volume", "Normal volume", "Low volume", "Very low volume"))) %>% 
  arrange(-desc(Status)) %>% 
  group_by(Status) %>%
  dplyr::select(-c(Scenario, mo))

b1$From<-lead(b1$Value, 217)
b1$From<-ifelse(b1$Status=="Very low volume", 0, b1$From)

# plot Volume
plot_vol_lmer<-ggplot(data=a)+
  geom_ribbon(data=b1, aes(x=Date, ymin=From, ymax=Value, fill=Status),  alpha=0.1, linetype = 0)+
  geom_line(aes(x=Date, y=Value, color=Legend), size=1)+
  facet_grid(~ Scenario, scales="free_x")+
  labs(x="Time [Months]", y="Volume [%]",
       title="Past water Volume")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=20),
        legend.title = element_text(size=22),
        panel.spacing = unit(2, "lines"),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20),
        legend.position="bottom",
        legend.box.margin=margin(5,5,5,5),
        legend.box.background = element_rect(colour = "black", size=0.5))+
  scale_x_date(breaks = pretty_breaks(15), expand = c(0, 0))+
  scale_colour_manual(values=c("#f8766d","#4372CF"))+
  scale_fill_manual("", values=c("#CC00FFFF", "#0066FFFF", "#00FF66FF", "#CCFF00FF", "#FF0000FF"))

plot_vol_lmer

# with dotted lines only
ggplot(data=a)+
  geom_line(aes(x=Date, y=Value, color=Legend), size=1)+
  geom_hline(yintercept=Q_vol90/159299502*100, lty=2, cex=0.8)+
  geom_hline(yintercept=Q_vol10/159299502*100, lty=2, cex=0.8)+
  facet_grid(~ Scenario, scales="free_x")+
  labs(x="Time [Months]", y="Volume [%]",
       title="Past water Volume")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=20),
        legend.title = element_text(size=22),
        panel.spacing = unit(2, "lines"),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20),
        legend.position="bottom",
        legend.box.margin=margin(5,5,5,5),
        legend.box.background = element_rect(colour = "black", size=0.5))+
  scale_x_date(breaks = pretty_breaks(15), expand = c(0, 0))+
  scale_colour_manual(values=c("#f8766d","#4372CF"))

# with dotted lines only and colored ribbon
a<-full_datasan2 %>% 
  dplyr::select(Date, Volume_mo, Volume_pred_mo) %>%
  mutate(Volume_mo=(Volume_mo/159299502)*100,
         Volume_pred_mo=(Volume_pred_mo/159299502)*100) %>%
  rename("Real volume"=Volume_mo,
         "Modelled volume"=Volume_pred_mo) %>%
  mutate(Scenario="Baseline",
         Vol_10=Q_vol10/159299502*100,
         Vol_90=100) %>% 
  gather(Legend, Value, c("Real volume", "Modelled volume")) 

b1<-full_datasan2 %>% 
  dplyr::select(Date) %>%
  mutate(From=0,
         Scenario="Baseline",
         "10th"=Q_vol10/159299502*100,
         "20th"=Q_vol20/159299502*100,
         "80th"=Q_vol90/159299502*100,
         "90th percentiles"=100) %>% 
  gather(Legend, Value, c("10th","20th", "80th", "90th percentiles")) %>% 
  mutate(From=case_when(Legend=="10th" ~ 0,
                        Legend=="20th" ~ Q_vol10/159299502*100,
                        Legend=="80th" ~ Q_vol80/159299502*100,
                        Legend=="90th percentiles" ~ Q_vol90/159299502*100))


plot_vol_lmer<-ggplot()+
  geom_ribbon(data=b1, aes(x=Date, ymin=From, ymax=Value, fill=Legend), alpha=0.5, colour=NA)+
  geom_line(data=a, aes(x=Date, y=Value, colour=Legend), size=1)+
  facet_grid(~ Scenario, scales="free_x")+
  labs(x="Time [Months]", y="Stored volume [%]")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=20),
        legend.title = element_text(size=22),
        panel.spacing = unit(2, "lines"),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20),
        legend.position="bottom",
        legend.box = "horizontal",
        legend.box.margin=margin(5,5,5,5),
        legend.box.background = element_rect(colour = "black", size=0.5))+
  scale_x_date(breaks = pretty_breaks(15), expand = c(0.001, 0.001))+
  scale_colour_manual("Legend", values=c("#f8766d","#4372CF"))+
  scale_fill_manual("", values=c("#fdae61","#fee090","#abd9e9", "#74add1"))+
  guides(colour = guide_legend(order = 1), 
         fill = guide_legend(order = 2))

plot_vol_lmer 

# saving plot
ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/S.Giustina_past_vol_lmer_bands.png", plot_vol_lmer,
       width = 16,
       height = 8,
       dpi=600)


aa<-full_datasan2 %>%
  dplyr::select(Date, Outflow_HP_mo, Out_lmer_mo, "Very high outflow", "High outflow", "Normal outflow", "Low outflow", "Very low outflow") %>%
  mutate(Outflow_HP_mo=(Outflow_HP_mo/171072000)*100,
         Out_lmer_mo=(Out_lmer_mo/171072000)*100) %>%  
  rename("Real outflow"=Outflow_HP_mo,
         "Modelled outflow"=Out_lmer_mo) %>%
  mutate(Scenario="Baseline") %>% 
  gather(Legend, Value, c("Real outflow", "Modelled outflow"))


bb<-full_datasan2 %>% 
  dplyr::select(Date, "Very high outflow", "High outflow", "Normal outflow", "Low outflow", "Very low outflow") %>%
  mutate(Scenario="Baseline")  

bb1<-bb %>% 
  gather(Status, Value, c("Very high outflow", "High outflow", "Normal outflow", "Low outflow", "Very low outflow")) %>% 
  mutate(Status=factor(Status, levels=c("Very high outflow", "High outflow", "Normal outflow", "Low outflow", "Very low outflow"))) %>% 
  arrange(-desc(Status)) %>% 
  group_by(Status) %>%
  dplyr::select(-c(Scenario, mo))

bb1$From<-lead(bb1$Value, 217)
bb1$From<-ifelse(bb1$Status=="Very low outflow", 0, bb1$From)

# plot outflow
plot_turb_lmer<-ggplot(data=aa)+
  geom_line(aes(x=Date, y=Value, color=Legend), size=1)+
  geom_hline(yintercept=Q_out90/171072000*100, lty=2, cex=0.8)+
  geom_hline(yintercept=Q_out10/171072000*100, lty=2, cex=0.8)+
  facet_grid(~ Scenario, scales="free_x")+
  labs(x="Time [Months]", y="Outflow [%]",
       title="Turbined outflow")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=20),
        legend.title = element_text(size=22),
        panel.spacing = unit(2, "lines"),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20),
        legend.position="bottom",
        legend.box.margin=margin(5,5,5,5),
        legend.box.background = element_rect(colour = "black", size=0.5))+
  scale_x_date(breaks = pretty_breaks(15), expand = c(0, 0))+
  scale_colour_manual(values=c("#f8766d", "#5ab4ac"))+
  scale_fill_manual("", values=c("#CC00FFFF", "#0066FFFF", "#00FF66FF", "#CCFF00FF", "#FF0000FF"))

plot_turb_lmer

# saving plot
ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/S.Giustina_past_out_lmer_bands.png", plot=plot_turb_lmer,
       width = 16,
       height = 8,
       dpi=600)


# with dotted lines only and colored ribbon
a<-full_datasan2 %>% 
  dplyr::select(Date, Outflow_HP_mo, Out_lmer_mo) %>%
  mutate(Outflow_mo=(Outflow_HP_mo/171072000)*100,
         Outflow_pred_mo=(Out_lmer_mo/171072000)*100) %>%
  rename("Real outflow"=Outflow_mo,
         "Modelled outflow"=Outflow_pred_mo) %>%
  mutate(Scenario="Baseline",
         Out_10=Q_out10/171072000*100,
         Out_90=100) %>% 
  gather(Legend, Value, c("Real outflow", "Modelled outflow"))

b1<-full_datasan2 %>% 
  dplyr::select(Date) %>%
  mutate(From=0,
         Scenario="Baseline",
         "10th"=Q_out10/171072000*100,
         "20th"=Q_out20/171072000*100,
         "80th"=Q_out90/171072000*100,
         "90th percentiles"=100) %>% 
  gather(Legend, Value, c("10th","20th", "80th", "90th percentiles")) %>% 
  mutate(From=case_when(Legend=="10th" ~ 0,
                        Legend=="20th" ~ Q_out10/171072000*100,
                        Legend=="80th" ~ Q_out80/171072000*100,
                        Legend=="90th percentiles" ~ Q_out90/171072000*100))


plot_turb_lmer<-ggplot()+
  geom_ribbon(data=b1, aes(x=Date, ymin=From, ymax=Value, fill=Legend), alpha=0.5, colour=NA)+
  geom_line(data=a, aes(x=Date, y=Value, color=Legend), size=1)+
  facet_grid(~ Scenario, scales="free_x")+
  labs(x="Time [Months]", y="Turbined Outflow [%]")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=20),
        legend.title = element_text(size=22),
        panel.spacing = unit(2, "lines"),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20),
        legend.position="bottom",
        legend.box.margin=margin(5,5,5,5),
        legend.box.background = element_rect(colour = "black", size=0.5))+
  scale_x_date(breaks = pretty_breaks(15), expand = c(0.001, 0.001))+
  scale_colour_manual(values=c("#f8766d", "#5ab4ac"))+
  scale_fill_manual("", values=c("#fdae61","#fee090","#abd9e9", "#74add1"))+
  guides(colour = guide_legend(order = 1), 
         fill = guide_legend(order = 2))


# saving plot
ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/S.Giustina_past_out_lmer_bands.png", plot=plot_turb_lmer,
       width = 16,
       height = 8,
       dpi=600)

#### Extreme values analysis ####

#### * Outflow ####
full_datasan_out2<- full_datasan %>% 
  mutate("YN<q10"= ifelse(Out_lmer_mo <= Q_out10, 1, 0),
         "YN<q20"= ifelse(Out_lmer_mo <= Q_out20, 1, 0),
         "YN>q80"= ifelse(Out_lmer_mo >= Q_out80, 1, 0),
         "YN>q90"= ifelse(Out_lmer_mo >= Q_out90, 1, 0)) %>% 
  mutate(
    `maxduration<q10` = with(rle(Out_lmer_mo <= Q_out10), max(lengths[values], na.rm=TRUE)),
    `maxduration<q20` = with(rle(Out_lmer_mo <= Q_out20), max(lengths[values], na.rm=TRUE)),
    `maxduration>q80` = with(rle(Out_lmer_mo >= Q_out80), max(lengths[values], na.rm=TRUE)),
    `maxduration>q90` = with(rle(Out_lmer_mo >= Q_out90), max(lengths[values], na.rm=TRUE)),
    run_q10=with(rle(Out_lmer_mo <= Q_out10), rep(lengths, lengths)),
    cond_q10=with(rle(Out_lmer_mo <= Q_out10), rep(values, lengths)),
    run_q20=with(rle(Out_lmer_mo <= Q_out20), rep(lengths, lengths)),
    cond_q20=with(rle(Out_lmer_mo <= Q_out20), rep(values, lengths)),
    run_q80=with(rle(Out_lmer_mo >= Q_out80), rep(lengths, lengths)),
    cond_q80=with(rle(Out_lmer_mo >= Q_out80), rep(values, lengths)),
    run_q90=with(rle(Out_lmer_mo >= Q_out90), rep(lengths, lengths)),
    cond_q90=with(rle(Out_lmer_mo >= Q_out90), rep(values, lengths)))

severity_q10<-full_datasan_out2 %>% 
  filter(run_q10==`maxduration<q10` & cond_q10 == TRUE) %>%
  dplyr::select(Date, Outflow_HP_mo, Out_lmer_mo) %>% 
  summarise(Severity=sum(abs(Q_out10-Out_lmer_mo)))

severity_q20<-full_datasan_out2 %>% 
  filter(run_q20==`maxduration<q20` & cond_q20 == TRUE) %>%
  dplyr::select(Date, Outflow_HP_mo, Out_lmer_mo) %>% 
  summarise(Severity=sum(abs(Q_out20-Out_lmer_mo)))

severity_q80<-full_datasan_out2 %>% 
  filter(run_q80==`maxduration>q80` & cond_q80 == TRUE) %>%
  dplyr::select(Date, Outflow_HP_mo, Out_lmer_mo) %>% 
  summarise(Severity=sum(abs(Q_out80-Out_lmer_mo)))

severity_q90<-full_datasan_out2 %>% 
  filter(run_q90==`maxduration>q90` & cond_q90 == TRUE) %>%
  dplyr::select(Date, Outflow_HP_mo, Out_lmer_mo) %>% 
  summarise(Severity=sum(abs(Q_out90-Out_lmer_mo)))

Severity_out_baseline<-rbind(severity_q10, severity_q20, severity_q80, severity_q90) %>% 
  as_tibble() %>% 
  mutate(Variable="Outflow",
         Threshold=c("q_10", "q_20", "q_80", "q_90"),
         Scenario="Baseline",
         Maxduration=c(unique(full_datasan_out2$`maxduration<q10`),
                       unique(full_datasan_out2$`maxduration<q20`),
                       unique(full_datasan_out2$`maxduration>q80`),
                       unique(full_datasan_out2$`maxduration>q90`)),
         Number_of_events=c(sum(full_datasan_out2$`YN<q10`, na.rm=T),
                            sum(full_datasan_out2$`YN<q20`, na.rm=T),
                            sum(full_datasan_out2$`YN>q80`, na.rm=T),
                            sum(full_datasan_out2$`YN>q90`, na.rm=T)),
         Intensity=Severity/Maxduration) %>% 
  dplyr::select(Variable, Scenario, Threshold, Number_of_events, Maxduration, Severity, Intensity)

Severity_out_baseline
# saveRDS(Severity_out_baseline, "S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Indices_outflow_values_baseline.rds")

#### * Volume ####

full_datasan_vol<- full_datasan %>% 
  dplyr::select(Date, Volume_pred_mo) %>% 
  mutate("YN<q10"= ifelse(Volume_pred_mo <= Q_vol10, 1, 0),
         "YN<q20"= ifelse(Volume_pred_mo <= Q_vol20, 1, 0),
         "YN>q80"= ifelse(Volume_pred_mo >= Q_vol80, 1, 0),
         "YN>q90"= ifelse(Volume_pred_mo >= Q_vol90, 1, 0)) %>% 
  mutate(
    `maxduration<q10` = with(rle(Volume_pred_mo<= Q_vol10), max(lengths[values], na.rm=TRUE)),
    `maxduration<q20` = with(rle(Volume_pred_mo<= Q_vol20), max(lengths[values], na.rm=TRUE)),
    `maxduration>q80` = with(rle(Volume_pred_mo>= Q_vol80), max(lengths[values], na.rm=TRUE)),
    `maxduration>q90` = with(rle(Volume_pred_mo>= Q_vol90), max(lengths[values], na.rm=TRUE)),
    run_q10=with(rle(Volume_pred_mo<= Q_vol10), rep(lengths, lengths)),
    cond_q10=with(rle(Volume_pred_mo<= Q_vol10), rep(values, lengths)),
    Event_id_q10=cumsum(!duplicated(run_q10, cond_q10)),
    run_q20=with(rle(Volume_pred_mo<= Q_vol20), rep(lengths, lengths)),
    cond_q20=with(rle(Volume_pred_mo<= Q_vol20), rep(values, lengths)),
    Event_id_q20=cumsum(!duplicated(run_q20, cond_q20)),
    run_q80=with(rle(Volume_pred_mo>= Q_vol80), rep(lengths, lengths)),
    cond_q80=with(rle(Volume_pred_mo>= Q_vol80), rep(values, lengths)),
    Event_id_q80=cumsum(!duplicated(run_q80, cond_q80)),
    run_q90=with(rle(Volume_pred_mo>= Q_vol90), rep(lengths, lengths)),
    cond_q90=with(rle(Volume_pred_mo>= Q_vol90), rep(values, lengths)),
    Event_id_q90=cumsum(!duplicated(run_q90, cond_q90)))


severity_q10<-full_datasan_vol %>%
  filter(cond_q10 == TRUE) %>%
  ungroup() %>% 
  mutate(Number_of_events=length(unique(Event_id_q10))) %>% 
  group_by(Event_id_q10, Number_of_events) %>% 
  summarise(Number_of_events,
            Severity=sum(abs(Q_vol10-Volume_pred_mo))) %>%
  ungroup() %>% 
  summarise(Number_of_events, 
            Mean_severity=mean(Severity)) %>% 
  distinct(Number_of_events, Mean_severity, .keep_all = T)
  

severity_q20<-full_datasan_vol %>%
  filter(cond_q20 == TRUE) %>%
  ungroup() %>% 
  mutate(Number_of_events=length(unique(Event_id_q20))) %>% 
  group_by(Event_id_q20, Number_of_events) %>% 
  summarise(Number_of_events,
            Severity=sum(abs(Q_vol20-Volume_pred_mo))) %>%
  ungroup() %>% 
  summarise(Number_of_events, 
            Mean_severity=mean(Severity)) %>% 
  distinct(Number_of_events, Mean_severity, .keep_all = T)

severity_q80<-full_datasan_vol %>%
  filter(cond_q80 == TRUE) %>%
  ungroup() %>% 
  mutate(Number_of_events=length(unique(Event_id_q80))) %>% 
  group_by(Event_id_q80, Number_of_events) %>% 
  summarise(Number_of_events,
            Severity=sum(abs(Q_vol80-Volume_pred_mo))) %>%
  ungroup() %>% 
  summarise(Number_of_events, 
            Mean_severity=mean(Severity)) %>% 
  distinct(Number_of_events, Mean_severity, .keep_all = T)


severity_q90<-full_datasan_vol %>%
  filter(run_q90==`maxduration>q90` & cond_q90 == TRUE) %>%
  ungroup() %>% 
  mutate(Number_of_events=length(unique(Event_id_q90))) %>% 
  group_by(Event_id_q90, Number_of_events) %>% 
  summarise(Number_of_events,
            Severity=sum(abs(Q_vol90-Volume_pred_mo))) %>%
  ungroup() %>% 
  summarise(Number_of_events, 
            Mean_severity=mean(Severity)) %>% 
  distinct(Number_of_events, Mean_severity, .keep_all = T)

Severity_vol_baseline<-rbind(severity_q10, severity_q20, severity_q80, severity_q90) %>% 
  as_tibble() %>% 
  mutate(Variable="Volume",
         Threshold=c("q_10", "q_20", "q_80", "q_90"),
         Scenario="Baseline",
         Maxduration=c(unique(full_datasan_vol$`maxduration<q10`),
                       unique(full_datasan_vol$`maxduration<q20`),
                       unique(full_datasan_vol$`maxduration>q80`),
                       unique(full_datasan_vol$`maxduration>q90`)),
         Number_of_months=c(sum(full_datasan_vol$`YN<q10`, na.rm=T),
                            sum(full_datasan_vol$`YN<q20`, na.rm=T),
                            sum(full_datasan_vol$`YN>q80`, na.rm=T),
                            sum(full_datasan_vol$`YN>q90`, na.rm=T))) %>% 
  dplyr::select(Variable, Scenario, Threshold, Number_of_months, Number_of_events, Maxduration, Mean_severity)

Severity_vol_baseline

saveRDS(Severity_vol_baseline, "S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Indices/Indices_volume_values_baseline.rds")
# Severity_vol_baseline<-readRDS("S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Indices/Indices_volume_values_baseline.rds")


####  Cumulative comparison ####
#### * Outflow ####

# new dt with real and modelled values
cumul_out<-data.frame(x=cumsum(santemp_day2$Outflow_HP[-c(1,2,3)]), y=cumsum(santemp_day2$Out_lmer[-c(1,2,3)]), 
                      Date=santemp_day2$Date[-c(1,2,3)]) %>% 
  rename("Outflow real"=x,
         "Outflow modelled"=y) %>% 
  merge(., full_dates, by="Date", all = T) %>%
  mutate(Scenario="Baseline") 

# date to pull
date1<-cumul_out %>%
  filter(Date==as_date("2004-12-31")) %>% 
  pull(Date)

date2<-cumul_out %>%
  filter(Date==as_date("2009-01-01")) %>% 
  pull(Date)


# plotting them 
cumul_out_plot<-cumul_out %>% 
  gather(Variable, Value, c("Outflow real", "Outflow modelled")) %>% 
  ggplot(., aes(x=Date, y=Value/10^6, color=Variable))+
  geom_line()+
  geom_point()+              
  scale_x_date(breaks = pretty_breaks(15))+
  facet_wrap(~Scenario, scales="free_x")+
  geom_vline(xintercept = date1, , lty=2, cex=0.8)+
  geom_vline(xintercept = date2, , lty=2, cex=0.8)+
  labs(x="\nTime [Months]", y="Outflows [Mm3/month]",
       title="Cumulative outflow values")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=20),
        legend.title = element_text(size=22),
        panel.spacing = unit(2, "lines"),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20),
        legend.position="bottom",
        legend.box.margin=margin(5,5,5,5),
        legend.box.background = element_rect(colour = "black", size=0.5))+
  scale_colour_manual(values=c("#f8766d", "#5ab4ac"))

# visualization
cumul_out_plot

# saving plot
# ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/Water_balance/S.Giustina_past_cumulative_outflow_dashed.png", plot=cumul_out_plot,
#        width = 16,
#        height = 8,
#        dpi=600)

# calculating final cumulative value
cumul_out$`Outflow modelled`[length(cumul_out$`Outflow modelled`)]-cumul_out$`Outflow real`[length(cumul_out$`Outflow real`)]


#### * Volume ####

full_datacumul<-data.frame(x=cumsum(santemp_day2$Volume[-c(1,2)]), y=cumsum(santemp_day2$Volume_lag[-c(1,2)]), 
                           Date=santemp_day2$Date[-c(1,2)]) %>% 
  rename("Volume real"=x,
         "Volume modelled"=y) %>% 
  merge(., full_dates[-c(1,2),], by="Date", all = T) %>%
  mutate(Scenario="Baseline")

# date to pull
date1<-full_datacumul %>%
  filter(Date==as_date("2004-12-31")) %>% 
  pull(Date)

date2<-full_datacumul %>%
  filter(Date==as_date("2009-01-01")) %>% 
  pull(Date)


# plotting them 
cumul_vol_plot<-full_datacumul %>% 
  gather(Legend, Value, c("Volume real", "Volume modelled")) %>% 
  ggplot(., aes(x=Date, y=Value/10^6, color=Legend))+
  geom_line()+
  geom_point()+              
  scale_x_date(breaks = pretty_breaks(15))+
  facet_wrap(~Scenario, scales="free_x")+
  geom_vline(xintercept = date1, , lty=2, cex=0.8)+
  geom_vline(xintercept = date2, , lty=2, cex=0.8)+
  labs(x="\nTime [Months]", y="Volume [Mm3]",
       title="Cumulative volume values")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=20),
        legend.title = element_text(size=22),
        panel.spacing = unit(2, "lines"),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20),
        legend.position="bottom",
        legend.box.margin=margin(5,5,5,5),
        legend.box.background = element_rect(colour = "black", size=0.5))+
  scale_colour_manual(values=c("#f8766d","#4372CF"))

# visualization
cumul_vol_plot

# saving plot
# ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/Water_balance/S.Giustina_past_cumulative_volume_dashed.png", plot=cumul_vol_plot,
#        width = 16,
#        height = 8,
#        dpi=600)

# final volume differences 
full_datacumul$`Volume real`[length(full_datacumul$`Volume real`)-1]-full_datacumul$`Volume modelled`[length(full_datacumul$`Volume modelled`)-1]


# average difference between the two volumes
full_datacumul<-full_datacumul %>%
  rowwise() %>% 
  mutate(Date,
            Diff=`Volume modelled`-`Volume real`,
            Perc=(`Volume modelled`-`Volume real`)/`Volume real`*100)
