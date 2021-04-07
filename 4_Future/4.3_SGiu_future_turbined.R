

#*******************************************************************************
# # This script to predict future water turbined from S.Giustina and plotting them
# # as time series and as monthly variations

#*******************************************************************************

#### Libraries ####

library("dplyr")
library("tidyr")
library("tibble")
library("merTools")
library("purrr")
library("lubridate")
library("plotly")

####  Setting parameters ####
# Maximum values of water that can physically be turbined
maxQ<-66*2592000

# quantiles for past volume values 
Q_out10<-quantile(datasan$Out_lmer, 0.10, na.rm=T)
Q_out90<-quantile(datasan$Out_lmer, 0.90, na.rm=T)

Q_out30<-quantile(datasan$Out_lmer, 0.30, na.rm=T)
Q_out80<-quantile(datasan$Out_lmer, 0.80, na.rm=T)

#### RCP45 2021-2050 ####

# inflow values RCP4.5 - 2020-2050
Q31_rcp45_50<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/SanGiu.Vol_rcp45_50.rds") %>%
  mutate(Volume_mo=vol_fut45.2050,
         Volume_lag=lag(vol_fut45.2050)) %>% 
  dplyr::select(id, Date, yr, mo, Inflow_sim_mo, Volume_mo, Volume_lag, Upper_values, Lower_values, Scenario, Time_period)

# prediction and CI
Q31_rcp45_50$turb_pred1<-predict(model_out_lmer1, Q31_rcp45_50)

# setting max and min values with physical constraints
Q31_rcp45_50[which(Q31_rcp45_50$turb_pred1 < 0), c("turb_pred1")] <- 0
Q31_rcp45_50[which(Q31_rcp45_50$turb_pred1 >= maxQ), c("turb_pred1")]<-maxQ

rcp45_50_turb_lmer<-predictInterval(model_out_lmer1, Q31_rcp45_50)

Q31_rcp45_50$Upper_values<-rcp45_50_turb_lmer$upr
Q31_rcp45_50$Lower_values<-rcp45_50_turb_lmer$lwr

# conditions:
 # on minimum values
Q31_rcp45_50[which(Q31_rcp45_50$Upper_values < 0), c("Upper_values")] <- 0
Q31_rcp45_50[which(Q31_rcp45_50$Lower_values < 0), c("Lower_values")] <- 0

 # on maximum values
Q31_rcp45_50[which(Q31_rcp45_50$Upper_values >= maxQ), c("Upper_values")]<-maxQ
Q31_rcp45_50[which(Q31_rcp45_50$Lower_values >= maxQ), c("Lower_values")]<-maxQ


# dt for plotting
out1<-Q31_rcp45_50 %>% 
  gather(Variable, Value, c("turb_pred1")) %>% 
  dplyr::select(id, Date, yr, mo, Upper_values, Lower_values, Scenario, Time_period, Variable, Value) %>% 
  as_tibble()

# plot
out1 %>% 
  ggplot(., aes(x=Date, y=Value/10^6, color=Variable))+
  geom_point(size=0.5)+
  geom_line(size=0.5)+
  geom_hline(yintercept = c(Q_out30/10^6, Q_out80/10^6), lty=2, na.rm=T)+
  geom_ribbon(aes(x=Date, ymin=Lower_values/10^6, ymax=Upper_values/10^6), alpha = .1, colour = NA)+
  geom_smooth()+
  labs(x="Time [months]", 
       y="Water outflow [Mm3/month]", 
       title="S.Giustina dam reservoir water outflow RCP4.5")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 24),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 30, hjust = 0.5),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        plot.caption = element_text(size=14))+
  scale_x_date(date_breaks = "12 months")


#### RCP45 2041-2070 ####
Q31_rcp45_70<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/SanGiu.Vol_rcp45_70.rds") %>% 
  mutate(Volume_mo=vol_fut45.2070,
       Volume_lag=lag(vol_fut45.2070),
       Inflow_sim_mo=Inflow_sim_mo) %>% 
  dplyr::select(id, Date, yr, mo, Inflow_sim_mo, Volume_mo, Volume_lag, Upper_values, Lower_values, Scenario, Time_period)

# prediction and CI
Q31_rcp45_70$turb_pred1<-predict(model_out_lmer1, Q31_rcp45_70)

# setting max and min values with physical constraints
Q31_rcp45_70[which(Q31_rcp45_70$turb_pred1 < 0), c("turb_pred1")] <- 0
Q31_rcp45_70[which(Q31_rcp45_70$turb_pred1 >= maxQ), c("turb_pred1")]<-maxQ

rcp45_70_turb_lmer<-predictInterval(model_out_lmer1, Q31_rcp45_70)

Q31_rcp45_70$Upper_values<-rcp45_70_turb_lmer$upr
Q31_rcp45_70$Lower_values<-rcp45_70_turb_lmer$lwr

# condition on:
  # minimum values
Q31_rcp45_70[which(Q31_rcp45_70$Upper_values < 0), c("Upper_values")] <- 0
Q31_rcp45_70[which(Q31_rcp45_70$Lower_values < 0), c("Lower_values")] <- 0

  # maximum values
Q31_rcp45_70[which(Q31_rcp45_70$Upper_values >= maxQ), c("Upper_values")]<-maxQ
Q31_rcp45_70[which(Q31_rcp45_70$Lower_values >= maxQ), c("Lower_values")]<-maxQ

# dt for plotting
out2<-Q31_rcp45_70 %>% 
  gather(Variable, Value, c("turb_pred1")) %>% 
  dplyr::select(id, Date, yr, mo, Upper_values, Lower_values, Scenario, Time_period, Variable, Value) %>% 
  as_tibble()

# plot
out2 %>% 
  ggplot(., aes(x=Date, y=Value/10^6, color=Variable))+
  geom_point(size=0.5)+
  geom_line(size=0.5)+
  geom_ribbon(aes(x=Date, ymin=Lower_values/10^6, ymax=Upper_values/10^6), alpha = .1, colour = NA)+
  geom_smooth()+
  labs(x="Time [months]", 
       y="Water outflow [Mm3/month]", 
       title="S.Giustina dam reservoir water outflow RCP4.5")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 24),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 30, hjust = 0.5),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        plot.caption = element_text(size=14))+
  scale_x_date(date_breaks = "12 months")


#### RCP85 2021-2050 ####
Q31_rcp85_50<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/SanGiu.Vol_rcp85_50.rds") %>% 
  mutate(Volume_mo=vol_fut85.2050,
         Volume_lag=lag(vol_fut85.2050)) %>% 
  dplyr::select(id, Date, yr, mo, Inflow_sim_mo, Volume_mo, Volume_lag, Upper_values, Lower_values, Scenario, Time_period)

# prediction and CI
Q31_rcp85_50$turb_pred1<-predict(model_out_lmer1, Q31_rcp85_50)

# setting max and min values with physical constraints
Q31_rcp85_50[which(Q31_rcp85_50$turb_pred1 < 0), c("turb_pred1")] <- 0
Q31_rcp85_50[which(Q31_rcp85_50$turb_pred1 >= maxQ), c("turb_pred1")]<-maxQ

rcp85_50_turb_lmer<-predictInterval(model_out_lmer1, Q31_rcp85_50)

Q31_rcp85_50$Upper_values<-rcp85_50_turb_lmer$upr
Q31_rcp85_50$Lower_values<-rcp85_50_turb_lmer$lwr

# condition on:
  #minimum values
Q31_rcp85_50[which(Q31_rcp85_50$Upper_values < 0), c("Upper_values")] <- 0
Q31_rcp85_50[which(Q31_rcp85_50$Lower_values < 0), c("Lower_values")] <- 0

  #maximum values
Q31_rcp85_50[which(Q31_rcp85_50$Upper_values >= maxQ), c("Upper_values")]<-maxQ
Q31_rcp85_50[which(Q31_rcp85_50$Lower_values >= maxQ), c("Lower_values")]<-maxQ


# dt for plotting 
out3<-Q31_rcp85_50 %>% 
  gather(Variable, Value, c("turb_pred1")) %>% 
  dplyr::select(id, Date, yr, mo, Upper_values, Lower_values, Scenario, Time_period, Variable, Value) %>% 
  as_tibble()

# plot
out3 %>% 
  ggplot(., aes(x=Date, y=Value/10^6, color=Variable))+
  geom_point(size=0.5)+
  geom_line(size=0.5)+
  geom_ribbon(aes(x=Date, ymin=Lower_values/10^6, ymax=Upper_values/10^6), alpha = .1, colour = NA)+
  geom_smooth()+
  labs(x="Time [months]", 
       y="Water outflow [Mm3/month]", 
       title="S.Giustina dam reservoir water outflow RCP8.5")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 24),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 30, hjust = 0.5),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        plot.caption = element_text(size=14))+
  scale_x_date(date_breaks = "12 months")


#### RCP85 2041-2070 ####
Q31_rcp85_70<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/SanGiu.Vol_rcp85_70.rds") %>% 
  mutate(Volume_mo=vol_fut85.2070,
         Volume_lag=lag(vol_fut85.2070)) %>% 
  dplyr::select(id, Date, yr, mo, Inflow_sim_mo, Volume_mo, Volume_lag, Upper_values, Lower_values, Scenario, Time_period)


# prediction and CI
Q31_rcp85_70$turb_pred1<-predict(model_out_lmer1, Q31_rcp85_70)

# condition on:
  #maximum values
Q31_rcp85_70[which(Q31_rcp85_70$turb_pred1 < 0), c("turb_pred1")] <- 0

  #minimum values
Q31_rcp85_70[which(Q31_rcp85_70$turb_pred1 >= maxQ), c("turb_pred1")]<-maxQ

# predction function for upper and lower values
rcp85_70_turb_lmer<-predictInterval(model_out_lmer1, Q31_rcp85_70)
Q31_rcp85_70$Upper_values<-rcp85_70_turb_lmer$upr
Q31_rcp85_70$Lower_values<-rcp85_70_turb_lmer$lwr

# condition on:
  #minimum values
Q31_rcp85_70[which(Q31_rcp85_70$Upper_values < 0), c("Upper_values")] <- 0
Q31_rcp85_70[which(Q31_rcp85_70$Lower_values < 0), c("Lower_values")] <- 0

  #maximum values
Q31_rcp85_70[which(Q31_rcp85_70$Upper_values >= maxQ), c("Upper_values")]<-maxQ
Q31_rcp85_70[which(Q31_rcp85_70$Lower_values >= maxQ), c("Lower_values")]<-maxQ


# dt for plotting
out4<-Q31_rcp85_70 %>% 
  gather(Variable, Value, c("turb_pred1")) %>% 
  dplyr::select(id, Date, yr, mo, Upper_values, Lower_values, Scenario, Time_period, Variable, Value) %>% 
  as_tibble()

# plot
out4 %>% 
  ggplot(., aes(x=Date, y=Value/10^6, color=Variable))+
  geom_point(size=0.5)+
  geom_line(size=0.5)+
  geom_ribbon(aes(x=Date, ymin=Lower_values/10^6, ymax=Upper_values/10^6), alpha = .1, colour = NA)+
  geom_smooth()+
  labs(x="Time [months]", 
       y="Water outflow [Mm3/month]", 
       title="S.Giustina dam reservoir water outflow RCP8.5")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 24),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 30, hjust = 0.5),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        plot.caption = element_text(size=14))+
  scale_x_date(date_breaks = "12 months")


#### Plotting them all ####
superturbina<-rbind(out1, out2, out3, out4) %>% 
  as_tibble() %>% 
  mutate(id=as.factor(id))

superturbina$Variable<-"Turbined outflow"

# setting colours for volume
col_out<-c("#abd9e9", "#74add1", "#fee090", "#fdae61")

# plot
p_turb<-superturbina %>% 
  rename(., Legend=Variable) %>% 
  ggplot(., aes(x=Date, y=Value/10^6, color=id, group=Scenario))+
  geom_line(size=1.1)+
  geom_ribbon(aes(x=Date, ymin=Lower_values/10^6, ymax=Upper_values/10^6), alpha = .2, colour=NA)+
  geom_hline(yintercept=c(Q_out30/10^6, Q_out80/10^6), lty=2, cex=0.8)+
  geom_smooth()+
  facet_grid(Scenario ~ Time_period, scales="free_x")+
  labs(x="\nTime [Months]", y="Outflow [Mm3/month]\n",
       title="Future turbined water outflows")+
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
  scale_colour_manual(values=c(col_out))
  
# visualization
p_turb

ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/S.Giustina_outflow_all.png", plot=p_turb,
       width = 16,
       height = 8, 
       dpi=600)

#### Monthly variation ####

datasan_past<-datasan %>% 
  dplyr::select(Date, yr, mo, Inflow_sim_mo, Out_lmer, Vol_lmer) %>% 
  group_by(mo) %>% 
  mutate(Outflow_meanmo=mean(Out_lmer, na.rm=T)) %>% 
  distinct(mo, Outflow_meanmo)

# for each predicted volume scenario
#1
out1_mo<-out1 %>%
  dplyr::select(Date, yr, mo, Variable, Value) %>%
  spread(Variable, Value) %>% 
  group_by(mo) %>%
  mutate(turb_pred1_meanmo=mean(turb_pred1, na.rm=T)) %>%
  distinct(mo, turb_pred1_meanmo)

#2
out2_mo<-out2 %>%
  dplyr::select(Date, yr, mo, Variable, Value) %>%
  spread(Variable, Value) %>% 
  group_by(mo) %>%
  mutate(turb_pred2_meanmo=mean(turb_pred1, na.rm=T)) %>%
  distinct(mo, turb_pred2_meanmo)

#3
out3_mo<-out3 %>%
  dplyr::select(Date, yr, mo, Variable, Value) %>%
  spread(Variable, Value) %>% 
  group_by(mo) %>%
  mutate(turb_pred3_meanmo=mean(turb_pred1, na.rm=T)) %>%
  distinct(mo, turb_pred3_meanmo)

#4
out4_mo<-out4 %>%
  dplyr::select(Date, yr, mo, Variable, Value) %>%
  spread(Variable, Value) %>% 
  group_by(mo) %>%
  mutate(turb_pred4_meanmo=mean(turb_pred1, na.rm=T)) %>%
  distinct(mo, turb_pred4_meanmo)

out_month<-merge(out1_mo, out2_mo, by="mo") %>% 
  merge(., out3_mo, by="mo") %>% 
  merge(., out4_mo, by="mo") %>% 
  merge(., datasan_past, by="mo") %>% 
  mutate("RCP4.5 2021-2050"=(turb_pred1_meanmo-Outflow_meanmo)/Outflow_meanmo*100,
         "RCP4.5 2041-2070"=(turb_pred2_meanmo-Outflow_meanmo)/Outflow_meanmo*100,
         "RCP8.5 2021-2050"=(turb_pred3_meanmo-Outflow_meanmo)/Outflow_meanmo*100,
         "RCP8.5 2041-2070"=(turb_pred4_meanmo-Outflow_meanmo)/Outflow_meanmo*100) %>% 
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
  # scale_x_continuous(breaks=seq(1,12,1), lim = c(0.5, 12.5), expand=c(0.01, 0))+
  scale_x_discrete(limits = month.abb)+
  scale_fill_manual(values=c(col_out))

# visualization
out_month_plot

# ggplotly
ggplotly(out_month_plot)

# saving plot
ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/S.Giustina_outflow_differences.png", plot=out_month_plot,
       width = 16,
       height = 8,
       dpi=600)
