
#### Library ####
library("mgcv")
library("gridExtra")
library("scales")

####  Gam without random effect ####
data_play<-datasan

data_play$Outflow_turb_lag<-lag(data_play$Outflow_turb_mo)

# list for RMSE
li1<-list()

# list for final results
li_res<-list()

for(i in 110:(nrow(data_play)-1)){
  
  train = subset(data_play, id <= i)
  test = subset(data_play, id > i)
  
  li_pred<-list(gam_v1 = gam(Outflow_turb_mo ~ s(Inflow_sim_mo, k=5) + (mo), train, family=poisson),
                gam_v2 = gam(Outflow_turb_mo ~ s(Inflow_sim_lag, k=5) + (mo), train, family=poisson),
                gam_v3 = gam(Outflow_turb_mo ~ s(Inflow_sim_mo, k=5) +  s(Vol_lmer, k=5) + (mo), train, family=poisson),
                gam_v4 = gam(Outflow_turb_mo ~ s(Inflow_sim_lag, k=5) +  s(Vol_lmer, k=5) + (mo), train, family=poisson),
                gam_v5 = gam(Outflow_turb_mo ~ s(Inflow_sim_mo, k=5) +  s(Vol_lmer_lag, k=5) + (mo), datasan, family=poisson))
  
  
  test<-cbind.data.frame(test, predict(li_pred, test))
  
  # RMSE
  li1[i-109]<-list(cbind(caret::postResample(test$Volume_mo, test$gam_v1),
                         caret::postResample(test$Volume_mo, test$gam_v2),
                         caret::postResample(test$Volume_mo, test$gam_v3),
                         caret::postResample(test$Volume_mo, test$gam_v4),
                         caret::postResample(test$Volume_mo, test$gam_v5)))
  
  
  li_res[i-109]<-test %>% 
    dplyr::select(Date, mo, Inflow_sim_mo, Volume_mo, gam_v1, gam_v2, gam_v3, gam_v4, gam_v5) %>% 
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
  summarise(R2_lmer1=mean(V1, na.rm=T),
            R2_lmer2=mean(V2, na.rm=T),
            R2_lmer3=mean(V3, na.rm=T),
            R2_lmer4=mean(V4, na.rm=T),
            R2_lmer5=mean(V5, na.rm=T))

# averaging RMSE
rmse %>% 
  as.data.frame() %>% 
  summarise(rmse_gam1=mean(V1, na.rm=T),
            rmse_gam2=mean(V2, na.rm=T),
            rmse_gam3=mean(V3, na.rm=T),
            rmse_gam4=mean(V4, na.rm=T),
            rmse_gam5=mean(V5, na.rm=T))

# plotting observed and predicted values
moving_res %>% 
  group_by(Date) %>% 
  mutate(predict_gam1_avrg=mean(gam_v1),
         predict_gam2_avrg=mean(gam_v2),
         predict_gam3_avrg=mean(gam_v3),
         predict_gam4_avrg=mean(gam_v4),
         predict_gam5_avrg=mean(gam_v5)) %>%
  distinct(Date, , .keep_all = T) %>% 
  dplyr::select(Date, Volume_mo, predict_gam1_avrg, predict_gam2_avrg, predict_gam3_avrg, predict_gam4_avrg, predict_gam5_avrg) %>%
  gather(Variable, Value, c(2:5)) %>%
  ggplot(., aes(x=Date, y=Value, color=Variable))+
  geom_line()+
  geom_point()+              
  scale_x_date(breaks = pretty_breaks(15))+
  theme_light(base_size = 20)


####  Gam with random effect ####
data_play<-datasan

data_play$Outflow_turb_lag<-lag(data_play$Outflow_turb_mo)

# list for RMSE
li1<-list()

# list for final results
li_res<-list()

for(i in 110:(nrow(data_play)-1)){
  
  train = subset(data_play, id <= i)
  test = subset(data_play, id > i)
  
  li_pred<-list(gam_random_v1 = gam(Outflow_turb_mo ~ s(Inflow_sim_mo, k=5) +  s(mo, bs="re"), train, family=poisson),
                gam_random_v2 = gam(Outflow_turb_mo ~ s(Inflow_sim_lag, k=5) + s(mo, bs="re"), train, family=poisson)
                gam_random_v3 = gam(Outflow_turb_mo ~ s(Inflow_sim_mo, k=5) +  s(Vol_lmer, k=5) + s(mo, bs="re"), train, family=poisson)
                gam_random_v4 = gam(Outflow_turb_mo ~ s(Inflow_sim_mo, k=5) +  s(Vol_lmer_lag, k=5) + s(mo, bs="re"), train, family=poisson))
  
  
  test<-cbind.data.frame(test, predict(li_pred, test))
  
  # RMSE
  li1[i-109]<-list(cbind(caret::postResample(test$Volume_mo, test$gam_random_v1),
                         caret::postResample(test$Volume_mo, test$gam_random_v2),
                         caret::postResample(test$Volume_mo, test$gam_random_v3),
                         caret::postResample(test$Volume_mo, test$gam_random_v4),
                         caret::postResample(test$Volume_mo, test$gam_random_v5)))
  
  
  li_res[i-109]<-test %>% 
    dplyr::select(Date, mo, Inflow_sim_mo, Volume_mo, gam_random_v1, gam_random_v2, gam_random_v3, gam_random_v4, gam_random_v5) %>% 
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
  summarise(R2_lmer1=mean(V1, na.rm=T),
            R2_lmer2=mean(V2, na.rm=T),
            R2_lmer3=mean(V3, na.rm=T),
            R2_lmer4=mean(V4, na.rm=T),
            R2_lmer5=mean(V5, na.rm=T))

# averaging RMSE
rmse %>% 
  as.data.frame() %>% 
  summarise(rmse_gam_random1=mean(V1, na.rm=T),
            rmse_gam_random2=mean(V2, na.rm=T),
            rmse_gam_random3=mean(V3, na.rm=T),
            rmse_gam_random4=mean(V4, na.rm=T),
            rmse_gam_random5=mean(V5, na.rm=T))

# plotting observed and predicted values
moving_res %>% 
  group_by(Date) %>% 
  mutate(predict_gam_random1_avrg=mean(gam_random_v1),
         predict_gam_random2_avrg=mean(gam_random_v2),
         predict_gam_random3_avrg=mean(gam_random_v3),
         predict_gam_random4_avrg=mean(gam_random_v4),
         predict_gam_random5_avrg=mean(gam_random_v5)) %>%
  distinct(Date, , .keep_all = T) %>% 
  dplyr::select(Date, Volume_mo, predict_gam_random1_avrg, predict_gam_random2_avrg, 
                predict_gam_random3_avrg, predict_gam_random4_avrg, predict_gam_random5_avrg) %>%
  gather(Variable, Value, c(2:5)) %>%
  ggplot(., aes(x=Date, y=Value, color=Variable))+
  geom_line()+
  geom_point()+              
  scale_x_date(breaks = pretty_breaks(15))+
  theme_light(base_size = 20)


#### Fitting all data ####
data_play<-datasan

gam1<- gam(Outflow_turb_mo ~ s(Inflow_sim_mo, k=5) + (mo), datasan, family=poisson)
summary(gam1)$r.sq

gam2<- gam(Outflow_turb_mo ~ s(Inflow_sim_lag, k=5) + (mo), datasan, family=poisson)
summary(gam2)$r.sq

gam3<- gam(Outflow_turb_mo ~ s(Inflow_sim_mo, k=5) +  s(Vol_lmer, k=5) + (mo), datasan, family=poisson)
summary(gam3)$r.sq

gam3<- gam(Outflow_turb_mo ~ s(Inflow_sim_mo, k=5) +  s(Vol_lmer, k=5) + (mo), datasan, family=poisson)
summary(gam3)$r.sq

gam4<- gam(Outflow_turb_mo ~ s(Inflow_sim_lag, k=5) +  s(Vol_lmer, k=5) + (mo), datasan, family=poisson)
summary(gam4)$r.sq

gam5<- gam(Outflow_turb_mo ~ s(Inflow_sim_mo, k=5) +  s(Vol_lmer_lag, k=5) + (mo), datasan, family=poisson)
summary(gam5)$r.sq

# prediction
data_play$out_gam1<-predict(gam1, data_play, type="response")
data_play$out_gam2<-predict(gam2, data_play, type="response")
data_play$out_gam3<-predict(gam3, data_play, type="response")
data_play$out_gam4<-predict(gam4, data_play, type="response")
data_play$out_gam5<-predict(gam5, data_play, type="response")

# rmse
caret::postResample(data_play$Outflow_turb_mo[-c(1,2)], data_play$out_gam1[-c(1,2)])[1]
caret::postResample(data_play$Outflow_turb_mo[-c(1,2)], data_play$out_gam2[-c(1,2)])[1]
caret::postResample(data_play$Outflow_turb_mo[-c(1,2)], data_play$out_gam3[-c(1,2)])[1]
caret::postResample(data_play$Outflow_turb_mo[-c(1,2)], data_play$out_gam4[-c(1,2)])[1]
caret::postResample(data_play$Outflow_turb_mo[-c(1,2)], data_play$out_gam5[-c(1,2)])[1]

# mixed effects
gam_random1<-gam(Outflow_turb_mo ~ s(Inflow_sim_mo, k=5) +  s(mo, bs="re"), datasan, family=poisson)
summary(gam_random1)$r.sq

gam_random2<-gam(Outflow_turb_mo ~ s(Inflow_sim_lag, k=5) + s(mo, bs="re"), datasan, family=poisson)
summary(gam_random2)$r.sq

gam_random3<-gam(Outflow_turb_mo ~ s(Inflow_sim_mo, k=5) +  s(Vol_lmer, k=5) + s(mo, bs="re"), datasan, family=poisson)
summary(gam_random3)$r.sq

gam_random4<-gam(Outflow_turb_mo ~ s(Inflow_sim_mo, k=5) +  s(Vol_lmer_lag, k=5) + s(mo, bs="re"), datasan, family=poisson)
summary(gam_random4)$r.sq


# prediction
data_play$out_gam_random1<-predict(gam_random1, data_play, type="response")
data_play$out_gam_random2<-predict(gam_random2, data_play, type="response")
data_play$out_gam_random3<-predict(gam_random3, data_play, type="response")
data_play$out_gam_random4<-predict(gam_random4, data_play, type="response")

# rmse
caret::postResample(data_play$Outflow_turb_mo[-c(1,2)], data_play$out_gam_random1[-c(1,2)])[1]
caret::postResample(data_play$Outflow_turb_mo[-c(1,2)], data_play$out_gam_random2[-c(1,2)])[1]
caret::postResample(data_play$Outflow_turb_mo[-c(1,2)], data_play$out_gam_random3[-c(1,2)])[1]
caret::postResample(data_play$Outflow_turb_mo[-c(1,2)], data_play$out_gam_random4[-c(1,2)])[1]


p1<-data_play %>% 
  gather(Variable, Value, c(5,15)) %>%
  ggplot(., aes(x=Date, y=Value, color=Variable))+
  geom_line()+
  geom_point()+
  scale_x_date(breaks = pretty_breaks(15))+
  theme_light(base_size = 20)

p2<-data_play %>% 
  gather(Variable, Value, c(5,16)) %>%
  ggplot(., aes(x=Date, y=Value, color=Variable))+
  geom_line()+
  geom_point()+
  scale_x_date(breaks = pretty_breaks(15))+
  theme_light(base_size = 20)

p3<-data_play %>% 
  gather(Variable, Value, c(5,17)) %>%
  ggplot(., aes(x=Date, y=Value, color=Variable))+
  geom_line()+
  geom_point()+
  scale_x_date(breaks = pretty_breaks(15))+
  theme_light(base_size = 20)

p4<-data_play %>% 
  gather(Variable, Value, c(5,18)) %>%
  ggplot(., aes(x=Date, y=Value, color=Variable))+
  geom_line()+
  geom_point()+
  scale_x_date(breaks = pretty_breaks(15))+
  theme_light(base_size = 20)


grid.arrange(p1,p2,p3,p4)

data_play %>% 
  gather(Variable, Value, c(Outflow_turb_mo, Out_lmer,out_gam_random2)) %>%
  ggplot(., aes(x=Date, y=Value, color=Variable))+
  geom_line()+
  geom_point()+
  scale_x_date(breaks = pretty_breaks(15))+
  theme_light(base_size = 20)


f<-data.frame(real=cumsum(data_play$Outflow_turb_mo[-c(1,2)]), lmer=cumsum(data_play$Out_lmer[-c(1,2)]), gam=cumsum(data_play$out_gam_random2[-c(1,2)]), Date=data_play$Date[-c(1,2)]) %>% 
  as_tibble()


p5<-f %>% 
  gather(Variable, Value, c(real, lmer, gam)) %>% 
  ggplot(., aes(x=Date, y=Value, color=Variable))+
  geom_line()+
  geom_point()+              
  scale_x_date(breaks = pretty_breaks(15))+
  theme_light(base_size = 20)+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))




