

#*******************************************************************************
# # This script to cary out multi-linear regression on past outflows from S.Giustina

#*******************************************************************************

#### Libraries ####
library("dplyr")
library("ggplot2")
library("tibble")
library("tidyr")
library("lubridate")
library("MASS")
library("MuMIn")
library("car")
library("caret")


#### Uploading dataset ####
source("1_Datasets/1_SGiu_dataset.R")

options(na.action = "na.omit") 

##### Variables significance to Outflow #####

# dataset with price
dt_sanprice<-sanprice %>%
  dplyr::select(mo, Outflow_turb_mo, Inflow_sim_mo, Price) %>%
  as_tibble() %>% 
  rename(month=mo, 
         Inflow=Inflow_sim_mo,
         Outflow=Outflow_turb_mo)

# dataset with rainfall
dt_sanprira<-sanprira %>%
  dplyr::select(mo, Outflow_turb_mo, Inflow_sim_mo, Price, Rainfall) %>%
  as_tibble() %>% 
  rename(month=mo, 
         Inflow=Inflow_sim_mo,
         Outflow=Outflow_turb_mo)

# dataset with price and temperature
dt_sanpritemp<-sanpritemp %>%
  dplyr::select(mo, Outflow_turb_mo, Inflow_sim_mo, Price, Tempe) %>%
  as_tibble() %>% 
  rename(month=mo, 
         Inflow=Inflow_sim_mo,
         Outflow=Outflow_turb_mo,
         Temperature= Tempe)

# dataset with rainfall and temperature
dt_temp_rain<-temp_rain %>%
  dplyr::select(mo, Outflow_turb_mo, Inflow_sim_mo, Tempe, Rainfall) %>%
  as_tibble() %>% 
  rename(month=mo, 
         Inflow=Inflow_sim_mo,
         Outflow=Outflow_turb_mo,
         Temperature= Tempe)

# dataset with rainfall and temperature
dt_sancares<-sancares %>%
  dplyr::select(mo, Outflow_turb_mo, Inflow_sim_mo, Rainfall, Out_careser) %>%
  as_tibble() %>% 
  rename(month=mo, 
         Inflow=Inflow_sim_mo,
         Outflow=Outflow_turb_mo)

# dataset with rainfall and temperature
dt_dt<-dt %>%
  dplyr::select(mo, Outflow, GeoTransf_inflow, Rainfall, PUN, Temperature, Outflow_careser) %>%
  as_tibble() %>% 
  rename(month=mo, 
         Inflow=GeoTransf_inflow)

# # full models
a1<-lm(Outflow ~., data = dt_sanprice)
a2<-lm(Outflow ~., data = dt_sanprira)
a3<-lm(Outflow ~., data = dt_sanpritemp)
a4<-lm(Outflow ~., data = dt_temp_rain)
a5<-lm(Outflow ~., data = dt_sancares)
a6<-lm(Outflow ~., data = dt_dt)

# their summaries
summary(a1)
summary(a2)
summary(a3)
summary(a4)
summary(a5)
summary(a6)

#### Multi-linear comparison using moving window ####
datasan$id<-seq(1:nrow(datasan))

# list for RMSE
li1<-list()

# list for R-squared
li2<-list()

# list for final results
li_res<-list()

for(i in 110:(nrow(datasan)-1)){
  
  train = subset(datasan, id <= i)
  test = subset(datasan, id > i)
  
  li_pred<-list(model_lm1 = lm(Outflow_turb_mo ~ Inflow_sim_mo, train),
                model_lm2 = lm(Outflow_turb_mo ~ Inflow_sim_lag, train),
                model_lm3 = lm(Outflow_turb_mo ~ Inflow_sim_mo + Volume_mo, train),
                model_lm4 = lm(Outflow_turb_mo ~ Inflow_sim_lag + Volume_mo, train),
                model_lm5 = lm(Outflow_turb_mo ~ Inflow_sim_mo + Volume_lag, train),
                model_lm6 = lm(Outflow_turb_mo ~ Inflow_sim_lag + Volume_lag, train))
  
  test<-cbind.data.frame(test, predict(li_pred, test))

  # RMSE
  li1[i-109]<-list(cbind(caret::postResample(test$Outflow_turb_mo, test$model_lm1),
                         caret::postResample(test$Outflow_turb_mo, test$model_lm2),
                         caret::postResample(test$Outflow_turb_mo, test$model_lm3),
                         caret::postResample(test$Outflow_turb_mo, test$model_lm4),
                         caret::postResample(test$Outflow_turb_mo, test$model_lm5),
                         caret::postResample(test$Outflow_turb_mo, test$model_lm6)))

  li_res[i-109]<-test %>% 
    dplyr::select(Date, mo, Inflow_sim_mo, Outflow_turb_mo, model_lm1, model_lm2,
                  model_lm3, model_lm4, model_lm5, model_lm6) %>% 
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
            R2_lm5=mean(V5, na.rm=T),
            R2_lm6=mean(V6, na.rm=T))

# averaging RMSE
rmse %>% 
  as.data.frame() %>% 
  summarise(rmse_lm1=mean(V1, na.rm=T),
            rmse_lm2=mean(V2, na.rm=T),
            rmse_lm3=mean(V3, na.rm=T),
            rmse_lm4=mean(V4, na.rm=T),
            rmse_lm5=mean(V5, na.rm=T),
            rmse_lm6=mean(V6, na.rm=T))

# plotting observed and predicted values
moving_res %>% 
  group_by(Date) %>% 
  mutate(predict_lm1_avrg=mean(model_lm1),
         predict_lm2_avrg=mean(model_lm2),
         predict_lm3_avrg=mean(model_lm3),
         predict_lm4_avrg=mean(model_lm4),
         predict_lm5_avrg=mean(model_lm5),
         predict_lm6_avrg=mean(model_lm6)) %>%
  distinct(Date, , .keep_all = T) %>% 
  dplyr::select(Date, Outflow_turb_mo, predict_lm1_avrg, predict_lm2_avrg, 
                predict_lm3_avrg, predict_lm4_avrg, predict_lm5_avrg, predict_lm6_avrg) %>%
  gather(Variable, Value, c(2:7)) %>%
  ggplot(., aes(x=Date, y=Value, color=Variable))+
  geom_line()+
  geom_point()
