

#*******************************************************************************

# # Script with linear mixed effect models to predict past water stored in S.Giustina

#*******************************************************************************


#### Libraries  #### 
library("caret")
library("lme4")
library("MuMIn")
library("car")
library("MASS")
library("scales")
library("merTools")


#### Multi-model comparison ####

datasan$id<-seq(1:nrow(datasan))

# list for RMSE
li1<-list()

# list for final results
li_res<-list()

for(i in 110:(nrow(datasan)-1)){
  
  train = subset(datasan, id <= i)
  test = subset(datasan, id > i)
  
  li_pred<-list(model_lmer1 = lme4::lmer(Volume_mo ~ Inflow_sim_mo + (1|mo), train),
                model_lmer2 = lme4::lmer(Volume_mo ~  Inflow_sim_lag + (1|mo), train),
                model_lmer3 = lme4::lmer(Volume_mo ~  Inflow_sim_mo + Outflow_turb_mo + (1|mo), train))
  
  
  test<-cbind.data.frame(test, predict(li_pred, test))
  
  # RMSE
  li1[i-109]<-list(cbind(caret::postResample(test$Volume_mo, test$model_lmer1),
                         caret::postResample(test$Volume_mo, test$model_lmer2),
                         caret::postResample(test$Volume_mo, test$model_lmer3)))
  
  
  li_res[i-109]<-test %>% 
    dplyr::select(Date, mo, Inflow_sim_mo, Volume_mo, model_lmer1, model_lmer2, model_lmer3) %>% 
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
            R2_lmer3=mean(V3, na.rm=T))

# averaging RMSE
rmse %>% 
  as.data.frame() %>% 
  summarise(rmse_lmer1=mean(V1, na.rm=T),
            rmse_lmer2=mean(V2, na.rm=T),
            rmse_lmer3=mean(V3, na.rm=T))

# plotting observed and predicted values
moving_res %>% 
  group_by(Date) %>% 
  mutate(predict_lmer1_avrg=mean(model_lmer1),
         predict_lmer2_avrg=mean(model_lmer2),
         predict_lmer3_avrg=mean(model_lmer3)) %>%
  distinct(Date, , .keep_all = T) %>% 
  dplyr::select(Date, Volume_mo, predict_lmer1_avrg, predict_lmer2_avrg, predict_lmer3_avrg) %>%
  gather(Variable, Value, c(2:5)) %>%
  ggplot(., aes(x=Date, y=Value, color=Variable))+
  geom_line()+
  geom_point()+              
  scale_x_date(breaks = pretty_breaks(15))+
  theme_light(base_size = 20)

#### Fitting all data ####
model_vol_lmer1 = lme4::lmer(Volume_mo ~  Inflow_sim_mo + (1|mo), data=datasan)

datasan$Vol_lmer<-predict(model_vol_lmer1, datasan)
datasan$Vol_lmer_lag<-lag(predict(model_vol_lmer1, datasan))

# setting max and minimum values
datasan[which(datasan$Vol_lmer > 159299502), c("Vol_lmer")] <- 159299502
datasan[which(datasan$Vol_lmer < 0), c("Vol_lmer")]<-0

# setting max and minimum values for volume lag
datasan[which(datasan$Vol_lmer_lag > 159299502), c("Vol_lmer_lag")] <- 159299502
datasan[which(datasan$Vol_lmer_lag < 0), c("Vol_lmer_lag")]<-0

# quantiles for past volume values 
Q_vol10<-quantile(datasan$Volume_mo, 0.10, na.rm=T)
Q_vol90<-quantile(datasan$Volume_mo, 0.90, na.rm=T)

Q_vol30<-quantile(datasan$Volume_mo, 0.30, na.rm=T)
Q_vol80<-quantile(datasan$Volume_mo, 0.80, na.rm=T)

# saving dataset with volume and CI
san_baseline_vol<-datasan
# prediction with lmer 
baseline_vol_lmer<-predictInterval(model_vol_lmer1, san_baseline_vol)

# prediction values and uncertainty
san_baseline_vol$baseline_vol<-baseline_vol_lmer$fit
san_baseline_vol$Upper_values<-baseline_vol_lmer$upr
san_baseline_vol$Lower_values<-baseline_vol_lmer$lwr

# getting rid of unrealistic values
san_baseline_vol[which(san_baseline_vol$baseline_vol > 159299502), c("baseline_vol")]<-159299502
san_baseline_vol[which(san_baseline_vol$Upper_values > 159299502), c("Upper_values")]<-159299502

san_baseline_vol[which(san_baseline_vol$baseline_vol < 0 ), c("baseline_vol")]<-0
san_baseline_vol[which(san_baseline_vol$Lower_values < 0 ), c("Lower_values")]<-0

# saving baseline volume dataset
saveRDS(san_baseline_vol, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/4-5_Past/4_Past_volume/sangiu_volume_baseline.rds")

# full date sequence
full_dates<-data.frame(Date=seq(as.Date(min(datasan$Date)), as.Date(max(datasan$Date)), by="months"), x=NA)

full_datasan<-merge(datasan, full_dates, by="Date", all = T)


# plotting real and modelled
past_vol<-full_datasan %>%
  rename("Volume real"=Volume_mo,
         "Volume modelled"=Vol_lmer) %>%
  mutate(Scenario="Baseline") %>% 
  gather(Legend, Value, c("Volume real", "Volume modelled")) %>% 
  ggplot(., aes(x=Date, y=Value/10^6, color=Legend))+
  geom_line(size=1)+
  geom_hline(yintercept=c(Q_vol30/10^6, Q_vol80/10^6), lty=2, cex=0.8)+
  facet_grid(~ Scenario, scales="free_x")+
  labs(x="\nTime [Months]", y="Volume [Mm3]",
       title="Past water volumes")+
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
  scale_x_date(breaks = pretty_breaks(15))+
  scale_colour_manual(values=c("#f8766d","#4372CF"))


# visualization
past_vol

# saving plot
ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/S.Giustina_past_volume.png", plot=past_vol,
       width = 16,
       height = 8,
       dpi=600)


####  Cumulative real and modelled values  ####
full_datacumul<-data.frame(x=cumsum(datasan$Volume_mo[-1]), y=cumsum(datasan$Vol_lmer[-1]), Date=datasan$Date[-1]) %>% 
  rename("Volume real"=x,
         "Volume modelled"=y) %>% 
  merge(., full_dates, by="Date", all = T) %>%
  mutate(Scenario="Baseline")

# date to pull
date1<-full_datacumul %>%
  filter(Date==as_date("2004-12-01")) %>% 
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
ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/S.Giustina_past_cumulative_volume_dashed.png", plot=cumul_vol_plot,
       width = 16,
       height = 8,
       dpi=600)

# final volume differences 
full_datacumul$`Volume real`[length(full_datacumul$`Volume real`)]-full_datacumul$`Volume modelled`[length(full_datacumul$`Volume modelled`)]
# resulting in 
# 61469912

