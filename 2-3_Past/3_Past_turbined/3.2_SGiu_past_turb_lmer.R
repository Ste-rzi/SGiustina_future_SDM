
#*******************************************************************************

# # Script with linear mixed effect models to predict past water turbined from S.Giustina

#*******************************************************************************

#### libraries ####
library("caret")
library("lme4")
library("MuMIn")
library("lmerTest")
library("purrr")
library("leaps")
library("scales")


#### Multi-model comparison with volume ####

datasan$id<-seq(1:nrow(datasan))

# list for RMSE
li1<-list()

# list for final results
li_res<-list()

for(i in 110:(nrow(datasan)-1)){
  
  train = subset(datasan, id <= i)
  test = subset(datasan, id > i)
  
  li_pred<-list(model_lmer1 = lme4::lmer(Outflow_turb_mo ~ Inflow_sim_mo + (1|mo), train),
                model_lmer2 = lme4::lmer(Outflow_turb_mo ~ Inflow_sim_lag + (1|mo), train),
                model_lmer3 = lme4::lmer(Outflow_turb_mo ~ Inflow_sim_mo + Vol_lmer + (1|mo), train),
                model_lmer4 = lme4::lmer(Outflow_turb_mo ~ Inflow_sim_mo + Vol_lmer_lag + (1|mo), train))
  
  
  test<-cbind.data.frame(test, predict(li_pred, test))
  
  # RMSE
  li1[i-109]<-list(cbind(caret::postResample(test$Outflow_turb_mo, test$model_lmer1),
                         caret::postResample(test$Outflow_turb_mo, test$model_lmer2),
                         caret::postResample(test$Outflow_turb_mo, test$model_lmer3),
                         caret::postResample(test$Outflow_turb_mo, test$model_lmer4)))
  
  
  li_res[i-109]<-test %>% 
    dplyr::select(Date, mo, Inflow_sim_mo, Outflow_turb_mo, model_lmer1, model_lmer2, model_lmer3, model_lmer4) %>% 
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
            R2_lmer4=mean(V4, na.rm=T))

# averaging RMSE
rmse %>% 
  as.data.frame() %>% 
  summarise(rmse_lmer1=mean(V1, na.rm=T),
            rmse_lmer2=mean(V2, na.rm=T),
            rmse_lmer3=mean(V3, na.rm=T),
            rmse_lmer4=mean(V4, na.rm=T))

moving_res %>% 
  group_by(Date) %>% 
  mutate(Out_lmer1=mean(model_lmer1),
         Out_lmer2=mean(model_lmer2),
         Out_lmer3=mean(model_lmer3),
         Out_lmer4=mean(model_lmer4)) %>%
  distinct(Date, , .keep_all = T) %>% 
  dplyr::select(Date, Outflow_turb_mo, Out_lmer1, Out_lmer2, Out_lmer3, Out_lmer4) %>%
  gather(Variable, Value, c(2:6)) %>%
  ggplot(., aes(x=Date, y=Value, color=Variable))+
  geom_line()+
  geom_point()+              
  scale_x_date(breaks = pretty_breaks(15))+
  theme_light(base_size = 20)


#### Fitting all data ####
model_out_lmer1 = lme4::lmer(Outflow_turb_mo ~ Inflow_sim_mo + Volume_lag + (1|mo), data=datasan)

datasan$Out_lmer<-predict(model_out_lmer1, datasan)

# performance over the whole dataset
caret::postResample(datasan$Outflow_turb_mo, datasan$Out_lmer)


# setting max and min values
datasan[which(datasan$Out_lmer > 171072000), c("Out_lmer")] <- 171072000
datasan[which(datasan$Out_lmer < 0), c("Out_lmer")]<-0

# quantiles for past turbined values
Q_out10<-quantile(datasan$Out_lmer, 0.10, na.rm=T)
Q_out90<-quantile(datasan$Out_lmer, 0.90, na.rm=T)

Q_out30<-quantile(datasan$Out_lmer, 0.30, na.rm=T)
Q_out80<-quantile(datasan$Out_lmer, 0.80, na.rm=T)

# saving baseline outflow dataset
saveRDS(datasan, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/4-5_Past/4_Past_volume/sangiu_outflow_baseline.rds")


# Past values plot
# full date sequence
full_dates<-data.frame(Date=seq(as.Date(min(datasan$Date)), as.Date(max(datasan$Date)), by="months"), x=NA)

full_datasan<-merge(datasan, full_dates, by="Date", all = T)

# plotting real and modelled
past_out<-full_datasan %>%
  rename("Outflow real"=Outflow_turb_mo,
         "Outflow modelled"=Out_lmer) %>%
  mutate(Scenario="Baseline") %>% 
  gather(Legend, Value, c("Outflow real", "Outflow modelled")) %>% 
  ggplot(., aes(x=Date, y=Value/10^6, color=Legend))+
  geom_line(size=1)+
  geom_hline(yintercept=c(Q_out30/10^6, Q_out80/10^6), lty=2, cex=0.8)+
  facet_grid(~ Scenario, scales="free_x")+
  labs(x="\nTime [Months]", y="Outflow [Mm3]",
       title="Past turbined water outflows")+
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
  scale_colour_manual(values=c("#f8766d", "#5ab4ac"))


# visualization
past_out

# saving plot
ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/S.Giustina_past_outflow.png", plot=past_out,
       width = 16,
       height = 8,
       dpi=600)


####  Cumulative real and modelled comparison ####

# new dt with real and modelled values
cumul_out<-data.frame(x=cumsum(datasan$Outflow_turb_mo[-c(1,2)]), y=cumsum(datasan$Out_lmer[-c(1,2)]), Date=datasan$Date[-c(1,2)]) %>% 
  rename("Outflow real"=x,
         "Outflow modelled"=y) %>% 
  merge(., full_dates, by="Date", all = T) %>%
  mutate(Scenario="Baseline") 

# date to pull
date1<-cumul_out %>%
  filter(Date==as_date("2004-12-01")) %>% 
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
ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/S.Giustina_past_cumulative_outflow_dashed.png", plot=cumul_out_plot,
       width = 16,
       height = 8,
       dpi=600)

# calculating final cumulative value
cumul_out$`Outflow modelled`[length(cumul_out$`Outflow modelled`)]-cumul_out$`Outflow real`[length(cumul_out$`Outflow real`)]
#16281091

