

#### Libraries ####
library("dplyr")
library("ggplot2")
library("tibble")
library("tidyr")
library("lubridate")
library("MuMIn")
library("caret")

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


#### Multi-linear models #####
# omitting NAs. Useful for regressions
options(na.action = "na.omit") 


#### Multi-model comparison with moving window ####
santemp_day$id<-seq(1:nrow(santemp_day))

# list for RMSE
li1<-list()

# list for R-squared
li2<-list()

# list for final results
li_res<-list()

for(i in 3650:(nrow(santemp_day)-1)){
  
  train = subset(santemp_day, id <= i)
  test = subset(santemp_day, id > i)
  
  li_pred<-list(model_lm1 = lm(Outflow_HP ~ Inflow_sim, train),
                model_lm2 = lm(Outflow_HP ~ Inflow_sim_lag, train),
                model_lm3 = lm(Outflow_HP ~ Inflow_sim + Volume, train),
                model_lm4 = lm(Outflow_HP ~ Inflow_sim_lag + Volume, train),
                model_lm5 = lm(Outflow_HP ~ Inflow_sim + Volume_lag, train),
                model_lm6 = lm(Outflow_HP ~ Inflow_sim_lag + Volume_lag, train),
                model_lm7 = lm(Outflow_HP ~ Inflow_sim_lag + Volume_lag + Tempe, train))
  
  test<-cbind.data.frame(test, predict(li_pred, test))
  
  # RMSE
  li1[i-3649]<-list(cbind(caret::postResample(test$Outflow_HP, test$model_lm1),
                         caret::postResample(test$Outflow_HP, test$model_lm2),
                         caret::postResample(test$Outflow_HP, test$model_lm3),
                         caret::postResample(test$Outflow_HP, test$model_lm4),
                         caret::postResample(test$Outflow_HP, test$model_lm5),
                         caret::postResample(test$Outflow_HP, test$model_lm6),
                         caret::postResample(test$Outflow_HP, test$model_lm7)))
  
  li_res[i-3649]<-test %>% 
    dplyr::select(Date, mo, Inflow_sim, Outflow_HP, model_lm1, model_lm2,
                  model_lm3, model_lm4, model_lm5, model_lm6, model_lm7) %>% 
    list()
  
  print((nrow(sgiudaily)-1)-i)  
}

# storing RMSE
rmse<-do.call(rbind,lapply(li1, `[`,1,))


# storing R-squared
r2.1<-do.call(rbind,lapply(li1, `[`,2,))
r2.1<-r2.1[-c(nrow(r2.1)-1,nrow(r2.1)), ]

# storing results in a dataframe
moving_res<-do.call(rbind, li_res)

# averaging R-squared
r2.1 %>% 
  as.data.frame() %>% 
  summarise(R2_lm1=mean(V1, na.rm=T),
            R2_lm2=mean(V2, na.rm=T),
            R2_lm3=mean(V3, na.rm=T),
            R2_lm4=mean(V4, na.rm=T),
            R2_lm5=mean(V5, na.rm=T),
            R2_lm6=mean(V6, na.rm=T),
            R2_lm7=mean(V7, na.rm=T))

# averaging RMSE
rmse %>% 
  as.data.frame() %>% 
  summarise(rmse_lm1=mean(V1, na.rm=T),
            rmse_lm2=mean(V2, na.rm=T),
            rmse_lm3=mean(V3, na.rm=T),
            rmse_lm4=mean(V4, na.rm=T),
            rmse_lm5=mean(V5, na.rm=T),
            rmse_lm6=mean(V6, na.rm=T),
            rmse_lm7=mean(V7, na.rm=T))

# plotting observed and predicted values
moving_res %>% 
  group_by(Date) %>% 
  mutate(predict_lm1_avrg=mean(model_lm1),
         predict_lm2_avrg=mean(model_lm2),
         predict_lm3_avrg=mean(model_lm3),
         predict_lm4_avrg=mean(model_lm4),
         predict_lm5_avrg=mean(model_lm5),
         predict_lm6_avrg=mean(model_lm6),
         predict_lm7_avrg=mean(model_lm7)) %>%
  distinct(Date, , .keep_all = T) %>% 
  dplyr::select(Date, Outflow_HP, predict_lm1_avrg, predict_lm2_avrg, 
                predict_lm3_avrg, predict_lm4_avrg, predict_lm5_avrg, predict_lm6_avrg, predict_lm7_avrg) %>%
  gather(Variable, Value, c(2:8)) %>%
  ggplot(., aes(x=Date, y=Value, color=Variable))+
  geom_line()+
  geom_point()


#### Fitting all data ####


model_lm1 = lm(Outflow_HP ~ Inflow_sim, santemp_day)
model_lm2 = lm(Outflow_HP ~ Inflow_sim_lag, santemp_day)
model_lm3 = lm(Outflow_HP ~ Inflow_sim + Volume, santemp_day)
model_lm4 = lm(Outflow_HP ~ Inflow_sim_lag + Volume, santemp_day)

model_lm5 = lm(Outflow_HP ~ Inflow_sim + Volume_lag, santemp_day)

model_lm6 = lm(Outflow_HP ~ Inflow_sim_lag + Volume_lag, santemp_day)
model_lm7 = lm(Outflow_HP ~ Inflow_sim_lag + Volume_lag + Tempe, santemp_day)


# saving dataset with prediction values (for testing nested models) overfitting the whole dataset
santemp_day$Out_lm1<-predict(model_lm1, santemp_day)
santemp_day$Out_lm2<-predict(model_lm2, santemp_day)
santemp_day$Out_lm3<-predict(model_lm3, santemp_day)
santemp_day$Out_lm4<-predict(model_lm4, santemp_day)
santemp_day$Out_lm<-predict(model_lm5, santemp_day)
santemp_day$Out_lm6<-predict(model_lm6, santemp_day)
santemp_day$Out_lm7<-predict(model_lm7, santemp_day)

# performance over the whole dataset
caret::postResample(santemp_day$Outflow_HP, santemp_day$Out_lm)


santemp_day %>% 
  dplyr::select(Date, Outflow_HP, Out_lm) %>% 
  gather(Variable, Value, c(2:3)) %>% 
  ggplot()+
  geom_line(aes(x=Date, y=Value, color=Variable))


sgiudaily2<-santemp_day %>% 
  dplyr::select(Date, yr, mo, Inflow_sim, Outflow_MEF, Outflow_rel, Volume, Volume_lag, Outflow_HP, Out_lm, 
                Out_lm1, Out_lm2, Out_lm3, Out_lm4, Out_lm6, Out_lm7)


sgiudaily2$Volume_lag<-lag(sgiudaily2$Volume)
sgiudaily2$Volume_lag[3:nrow(sgiudaily2)]<-NA


for (i in 3:(nrow(sgiudaily2)-1)){
  
  sgiudaily2$Volume_lag[i]<-sum(sgiudaily2$Volume_lag[i-1], sgiudaily2$Inflow_sim[i],
                                -sgiudaily2$Out_lm[i], -sgiudaily2$Outflow_MEF[i], -sgiudaily2$Outflow_rel[i],
                                na.rm=T)
  
  sgiudaily2$Volume_lag[i]<-ifelse(sgiudaily2$Volume_lag[i]>159299502, 159299502, sgiudaily2$Volume_lag[i])
  
  sgiudaily2$Out_lm[i+1]<-predict(model_lm5, sgiudaily2[i,])
  
  sgiudaily2$Out_lm[i+1]<-ifelse(sgiudaily2$Out_lm[i+1]<0, 0, sgiudaily2$Out_lm[i+1])
  sgiudaily2$Out_lm[i+1]<-ifelse(sgiudaily2$Out_lm[i+1]>171072000, 171072000, sgiudaily2$Out_lm[i+1])
  
  print((nrow(sgiudaily2)-1)-i)
  
}


#### Plotting ####

sgiudaily21<-sgiudaily2 %>%
  dplyr::select(Date, Volume, Volume_lag, Out_lm, Outflow_HP)

sgiudaily21 %>%
  gather(Variable, Value, c("Volume", "Volume_lag")) %>%
  ggplot(aes(x=Date, y=Value, color=Variable))+
  geom_line(size=0.7)

sgiudaily21 %>%
  gather(Variable, Value, c("Out_lm", "Outflow_HP")) %>%
  ggplot(aes(x=Date, y=Value, color=Variable))+
  geom_line(size=0.7)

caret::RMSE(sgiudaily21$Volume, sgiudaily21$Volume_lag, na.rm=T)
caret::RMSE(sgiudaily21$Outflow_HP, sgiudaily21$Out_lm, na.rm=T)

caret::R2(sgiudaily21$Volume, sgiudaily21$Volume_lag, na.rm=T)
caret::R2(sgiudaily21$Outflow_HP, sgiudaily21$Out_lm, na.rm=T)


# aggregating at monthly level
sgiudaily2

sgiudaily2_mo <-sgiudaily2 %>% 
  group_by(yr, mo) %>% 
  mutate(Outflow_HP_mo=sum(Outflow_HP, na.rm=T),
         Out_lm_mo=sum(Out_lm, na.rm=T),
         Out_lm1_mo=sum(Out_lm1, na.rm=T),
         Out_lm2_mo=sum(Out_lm2, na.rm=T),
         Out_lm3_mo=sum(Out_lm3, na.rm=T),
         Out_lm4_mo=sum(Out_lm4, na.rm=T),
         Out_lm6_mo=sum(Out_lm6, na.rm=T),
         Out_lm7_mo=sum(Out_lm7, na.rm=T),
         Inflow_sim_mo=sum(Inflow_sim, na.rm=T),
         # HP_Upr_mo=sum(HP_Upr, na.rm=T),
         # HP_Lwr_mo=sum(HP_Lwr, na.rm=T),
         MEF_mo=sum(Outflow_MEF, na.rm=T),
         Outflow_rel_mo=sum(Outflow_rel, na.rm=T),
         Volume_mo=mean(Volume, na.rm=T),
         Volume_pred_mo=mean(Volume_lag, na.rm=T)) %>% 
  distinct(yr, mo, .keep_all=T)


caret::RMSE(sgiudaily2_mo$Volume_mo, sgiudaily2_mo$Volume_pred_mo, na.rm=T)
caret::RMSE(sgiudaily2_mo$Outflow_HP_mo, sgiudaily2_mo$Out_lm_mo, na.rm=T)

caret::R2(sgiudaily2_mo$Volume_mo, sgiudaily2_mo$Volume_pred_mo, na.rm=T)
caret::R2(sgiudaily2_mo$Outflow_HP_mo, sgiudaily2_mo$Out_lm_mo, na.rm=T)

# for other models
caret::R2(sgiudaily2_mo$Outflow_HP_mo, sgiudaily2_mo$Out_lm1_mo, na.rm=T)
caret::RMSE(sgiudaily2_mo$Outflow_HP_mo, sgiudaily2_mo$Out_lm1_mo, na.rm=T)

caret::R2(sgiudaily2_mo$Outflow_HP_mo, sgiudaily2_mo$Out_lm2_mo, na.rm=T)
caret::RMSE(sgiudaily2_mo$Outflow_HP_mo, sgiudaily2_mo$Out_lm2_mo, na.rm=T)

caret::R2(sgiudaily2_mo$Outflow_HP_mo, sgiudaily2_mo$Out_lm3_mo, na.rm=T)
caret::RMSE(sgiudaily2_mo$Outflow_HP_mo, sgiudaily2_mo$Out_lm3_mo, na.rm=T)

caret::R2(sgiudaily2_mo$Outflow_HP_mo, sgiudaily2_mo$Out_lm4_mo, na.rm=T)
caret::RMSE(sgiudaily2_mo$Outflow_HP_mo, sgiudaily2_mo$Out_lm4_mo, na.rm=T)

caret::R2(sgiudaily2_mo$Outflow_HP_mo, sgiudaily2_mo$Out_lm6_mo, na.rm=T)
caret::RMSE(sgiudaily2_mo$Outflow_HP_mo, sgiudaily2_mo$Out_lm6_mo, na.rm=T)

caret::R2(sgiudaily2_mo$Outflow_HP_mo, sgiudaily2_mo$Out_lm7_mo, na.rm=T)
caret::RMSE(sgiudaily2_mo$Outflow_HP_mo, sgiudaily2_mo$Out_lm7_mo, na.rm=T)
#
