
#*******************************************************************************
# # This script to carry out multi-linear regression of past water stored in S.Giustina

#*******************************************************************************

#### libraries #####
library("dplyr")
library("tibble")
library("caret")

##### Variables significance to Volume #####

# dataset with price
dt_sanprice<-sanprice %>%
  dplyr::select(mo, Volume_mo, Inflow_sim_mo, Price) %>%
  as_tibble() %>% 
  rename(month=mo, 
         Inflow=Inflow_sim_mo,
         Volume=Volume_mo)

# dataset with rainfall
dt_sanprira<-sanprira %>%
  dplyr::select(mo, Volume_mo, Inflow_sim_mo, Price, Rainfall) %>%
  as_tibble() %>% 
  rename(month=mo, 
         Inflow=Inflow_sim_mo,
         Volume=Volume_mo)

# dataset with price and temperature
dt_sanpritemp<-sanpritemp %>%
  dplyr::select(mo, Volume_mo, Inflow_sim_mo, Price, Tempe) %>%
  as_tibble() %>% 
  rename(month=mo, 
         Inflow=Inflow_sim_mo,
         Volume=Volume_mo,
         Temperature= Tempe)

# dataset with rainfall and temperature
dt_temp_rain<-temp_rain %>%
  dplyr::select(mo, Volume_mo, Inflow_sim_mo, Tempe, Rainfall) %>%
  as_tibble() %>% 
  rename(month=mo, 
         Inflow=Inflow_sim_mo,
         Volume=Volume_mo,
         Temperature= Tempe)

# dataset with rainfall and temperature
dt_sancares<-sancares %>%
  dplyr::select(mo, Volume_mo, Inflow_sim_mo, Rainfall, Out_careser) %>%
  as_tibble() %>% 
  rename(month=mo, 
         Inflow=Inflow_sim_mo,
         Volume=Volume_mo)

# dataset with rainfall and temperature
dt_dt<-dt %>%
  dplyr::select(mo, Volume, GeoTransf_inflow, Rainfall, PUN, Temperature, Outflow_careser) %>%
  as_tibble() %>% 
  rename(month=mo, 
         Inflow=GeoTransf_inflow)

# # full models
a1<-lm(Volume ~., data = dt_sanprice)
a2<-lm(Volume ~., data = dt_sanprira)
a3<-lm(Volume ~., data = dt_sanpritemp)
a4<-lm(Volume ~., data = dt_temp_rain)
a5<-lm(Volume ~., data = dt_sancares)
a6<-lm(Volume ~., data = dt_dt)

# their summaries
summary(a1)
summary(a2)
summary(a3)
summary(a4)
summary(a5)
summary(a6)


#### Multi-linear models using moving window ##### 

datasan$Outflow_turb_lag<-lag(datasan$Outflow_turb_mo)

datasan$id<-seq(1:nrow(datasan))

# list for RMSE
li1<-list()

# list for R-squared
li2<-list()

# list for final results
li_res<-list()

# moving window in for loops
for(i in 110:(nrow(datasan)-1)){
  
  train = subset(datasan, id <= i)
  test = subset(datasan, id > i)
  

  li_pred<-list(model_lm.vol1 = lm(Volume_mo ~ Inflow_sim_mo, train),
                model_lm.vol2 = lm(Volume_mo ~ Inflow_sim_lag, train),
                model_lm.vol3 = lm(Volume_mo ~ Inflow_sim_mo + Outflow_turb_mo, train),
                model_lm.vol4 = lm(Volume_mo ~ Inflow_sim_lag + Outflow_turb_mo, train),
                model_lm.vol5 = lm(Volume_mo ~ Inflow_sim_lag + Outflow_turb_lag, train))
  
  test<-cbind.data.frame(test, predict(li_pred, test))
  
  # RMSE
  li1[i-109]<-list(cbind(caret::postResample(test$Volume_mo, test$model_lm.vol1),
                         caret::postResample(test$Volume_mo, test$model_lm.vol2),
                         caret::postResample(test$Volume_mo, test$model_lm.vol3),
                         caret::postResample(test$Volume_mo, test$model_lm.vol4),
                         caret::postResample(test$Volume_mo, test$model_lm.vol5)))
  
  
  li_res[i-109]<-test %>% 
    dplyr::select(Date, mo, Inflow_sim_mo, Volume_mo, model_lm.vol1, model_lm.vol2,model_lm.vol3, model_lm.vol4, model_lm.vol5) %>% 
    list()
  
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
            R2_lm5=mean(V5, na.rm=T))


# averaging RMSE
rmse %>% 
  as.data.frame() %>% 
  summarise(rmse_lm1=mean(V1, na.rm=T),
            rmse_lm2=mean(V2, na.rm=T),
            rmse_lm3=mean(V3, na.rm=T),
            rmse_lm4=mean(V4, na.rm=T),
            rmse_lm5=mean(V5, na.rm=T))

# plotting observed and predicted values
moving_res %>% 
  group_by(Date) %>% 
  mutate(predict_lm.vol1_avrg=mean(model_lm.vol1),
         predict_lm.vol2_avrg=mean(model_lm.vol2),
         predict_lm.vol3_avrg=mean(model_lm.vol3),
         predict_lm.vol4_avrg=mean(model_lm.vol4),
         predict_lm.vol5_avrg=mean(model_lm.vol5)) %>%
  distinct(Date, , .keep_all = T) %>% 
  dplyr::select(Date, Volume_mo, predict_lm.vol1_avrg, predict_lm.vol2_avrg, 
                predict_lm.vol3_avrg, predict_lm.vol4_avrg, predict_lm.vol5_avrg) %>%
  gather(Variable, Value, c(2:7)) %>%
  ggplot(., aes(x=Date, y=Value, color=Variable))+
  geom_line()+
  geom_point()+              
  scale_x_date(breaks = pretty_breaks(15))+
  theme_light(base_size = 20)

