

#*******************************************************************************
# # This script to predict future water stored in S.Giustina and plotting them
# # as time series and as monthly variations

#*******************************************************************************


#### Libraries ####
library("dplyr")
library("tidyr")
library("tibble")
library("merTools")
library("purrr")



#### All in one ####
Q31_rcp45_50<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP45_2020_2050.rds") %>% 
  dplyr::select(Date, yr, mo, Value_mo) %>% 
  mutate(Inflow_sim_mo=Value_mo) %>% 
  as_tibble()

Q31_rcp45_50$Inflow_sim_lag<-lag(Q31_rcp45_50$Inflow_sim_mo)


Q31_rcp45_70<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP45_2040_2070.rds") %>% 
  dplyr::select(Date, yr, mo, Value_mo) %>% 
  mutate(Inflow_sim_mo=Value_mo) %>%
  as_tibble()

Q31_rcp45_70$Inflow_sim_lag<-lag(Q31_rcp45_70$Inflow_sim_mo)


Q31_rcp85_50<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP85_2020_2050.rds") %>% 
  dplyr::select(Date, yr, mo, Value_mo) %>%
  mutate(Inflow_sim_mo=Value_mo) %>% 
  as_tibble()

Q31_rcp85_50$Inflow_sim_lag<-lag(Q31_rcp85_50$Inflow_sim_mo)


Q31_rcp85_70<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP85_2040_2070.rds") %>% 
  dplyr::select(Date, yr, mo, Value_mo) %>% 
  mutate(Inflow_sim_mo=Value_mo) %>% 
  as_tibble()

Q31_rcp85_70$Inflow_sim_lag<-lag(Q31_rcp85_70$Inflow_sim_mo)


# all in one list
li.sgiu.vol<-list(Q31_rcp45_50, Q31_rcp45_70, Q31_rcp85_50, Q31_rcp85_70)

# prediction with lmer
li.sgiu.volpred<- li.sgiu.vol %>% 
  purrr::map(function(x) cbind.data.frame(x, predictInterval(model_vol_lmer1, x)))


# adding info on the scenario type
li.sgiu.volpred[[1]][["Scenario"]]<-"RCP4.5 2021-2050"
li.sgiu.volpred[[2]][["Scenario"]]<-"RCP4.5 2041-2070"  

li.sgiu.volpred[[3]][["Scenario"]]<-"RCP8.5 2021-2050"  
li.sgiu.volpred[[4]][["Scenario"]]<-"RCP8.5 2041-2070"  

# plotting 
li.sgiu.volpred %>%
  purrr::map(function(x) x %>% gather(Variable, Value, c("fit"))) %>% 
  purrr::map(function(x) x %>% rename(., Legend=Variable)) %>%
  do.call(rbind, .) %>% 
  ggplot(., aes(x=Date, y=Value, color=Scenario, group=Scenario))+
  geom_line()+
  geom_hline(yintercept = c(Q_vol30, Q_vol80), lty=2)+
  geom_ribbon(aes(x=Date, ymin=lwr, ymax=upr), alpha = .1, colour = NA)+
  geom_smooth()+
  facet_wrap(~ Scenario, scales="free_x", ncol=2)+
  labs(x="\nTime [Months]", y="[Mm3]", title="Future water volume stored for RCP 4.5 and 8.5")+
  theme_light(base_size = 20)+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 20),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20))+              
  scale_x_date(breaks = pretty_breaks(15))


#### RCP45 2021-2050 ####

# inflow values RCP4.5 - 2020-2050
Q31_rcp45_50<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP45_2020_2050.rds") %>% 
  dplyr::select(Date, yr, mo, Value_mo) %>% 
  mutate(Inflow_sim_mo=Value_mo) %>% 
  as_tibble()

Q31_rcp45_50$Inflow_sim_lag<-lag(Q31_rcp45_50$Inflow_sim_mo)


# prediction with lmer 
rcp45_50_vol_lmer<-predictInterval(model_vol_lmer1, Q31_rcp45_50)

# prediction, upper and lower confidence intervals 
Q31_rcp45_50$vol_fut45.2050<-rcp45_50_vol_lmer$fit
Q31_rcp45_50$Upper_values<-rcp45_50_vol_lmer$upr
Q31_rcp45_50$Lower_values<-rcp45_50_vol_lmer$lwr

# getting rid of unrealistic values
Q31_rcp45_50[which(Q31_rcp45_50$vol_fut45.2050 > 159299502), c("vol_fut45.2050")]<-159299502
Q31_rcp45_50[which(Q31_rcp45_50$Upper_values > 159299502), c("Upper_values")]<-159299502
Q31_rcp45_50[which(Q31_rcp45_50$Lower_values > 159299502), c("Lower_values")]<-159299502


Q31_rcp45_50[which(Q31_rcp45_50$vol_fut45.2050 < 0 ), c("vol_fut45.2050")]<-0
Q31_rcp45_50[which(Q31_rcp45_50$Lower_values < 0 ), c("Lower_values")]<-0

# setting identifiers for plotting all scenarios later
Q31_rcp45_50$Scenario<-"RCP4.5"
Q31_rcp45_50$Time_period<-"2021-2050"
Q31_rcp45_50$id<-1

# test if lower values are greater than upper values
# which(Q31_rcp45_50$Lower_values>Q31_rcp45_50$Upper_values)


# saving it
saveRDS(Q31_rcp45_50, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/SanGiu.Vol_rcp45_50.rds")


# plot volume
dam1<-Q31_rcp45_50 %>% 
  ungroup() %>% 
  dplyr::select(Date, yr, vol_fut45.2050, Upper_values, Lower_values) %>% 
  rename(., RCP45_2021_2050 = vol_fut45.2050) %>% 
  gather(Variable, Value, c(3)) %>% 
  mutate(id=1) %>% 
  filter(yr>2020) %>% 
  ungroup() %>% 
  mutate(Scenario="RCP4.5",
         Time_period="2021-2050")

# saving it
saveRDS(dam1, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/san_future_vol_45_50.rds")


# plotting
dam1 %>% 
  ggplot()+
  geom_point(aes(x=Date, y=Value, color=Variable))+
  geom_line(aes(x=Date, y=Value, color=Variable), size=1)+
  geom_ribbon(aes(x=Date, ymin=Lower_values, ymax=Upper_values), alpha = .2)+
  labs(x="Time [months]", 
       y="Water volume [Mm3]", 
       title="S.Giustina dam reservoir water volume")+
  theme_light(base_size = 20)+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 24),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 30, hjust = 0.5),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        plot.caption = element_text(size=14))+
  scale_x_date(date_breaks = "12 months")


#### RCP45 2041-2070 ####
Q31_rcp45_70<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP45_2040_2070.rds") %>% 
  # readRDS("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/RisultatiOrientGate/Time_series/Antropico/prova/FutureRCP45_2040_2070/Q031_RCP45_2040_2070.rds") %>% 
  dplyr::select(Date, yr, mo, Value_mo) %>% 
  mutate(Inflow_sim_mo=Value_mo) %>%
  as_tibble()

Q31_rcp45_70$Inflow_sim_lag<-lag(Q31_rcp45_70$Inflow_sim_mo)

# prediction with lmer
rcp45_70_vol_lmer<-predictInterval(model_vol_lmer1, Q31_rcp45_70)

# prediction, upper and lower confidence intervals 
Q31_rcp45_70$vol_fut45.2070<-rcp45_70_vol_lmer$fit
Q31_rcp45_70$Upper_values<-rcp45_70_vol_lmer$upr
Q31_rcp45_70$Lower_values<-rcp45_70_vol_lmer$lwr

# getting rid of unrealistic values
Q31_rcp45_70[which(Q31_rcp45_70$vol_fut45.2070 > 159299502), c("vol_fut45.2070")]<-159299502
Q31_rcp45_70[which(Q31_rcp45_70$Upper_values > 159299502), c("Upper_values")]<-159299502
Q31_rcp45_70[which(Q31_rcp45_70$Lower_values > 159299502), c("Lower_values")]<-159299502

Q31_rcp45_70[which(Q31_rcp45_70$vol_fut45.2070 < 0 ), c("vol_fut45.2070")]<-0
Q31_rcp45_70[which(Q31_rcp45_70$Lower_values < 0 ), c("Lower_values")]<-0

# setting identifiers for plotting all scenarios later
Q31_rcp45_70$Scenario<-"RCP4.5"
Q31_rcp45_70$Time_period<-"2041-2070"
Q31_rcp45_70$id<-2

# test if lower values are greater than upper values
 # which(Q31_rcp45_70$Lower_values>Q31_rcp45_70$Upper_values)

# saving it
saveRDS(Q31_rcp45_70, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/SanGiu.Vol_rcp45_70.rds")

# plot volume
dam2<-Q31_rcp45_70 %>% ungroup() %>% 
  dplyr::select(Date, yr, vol_fut45.2070, Upper_values, Lower_values) %>% 
  rename(., RCP45_2041_2070 =vol_fut45.2070) %>% 
  gather(Variable, Value, c(3)) %>% 
  mutate(id=2) %>% 
  filter(yr>2040) %>% 
  ungroup() %>% 
  mutate(Scenario="RCP4.5",
         Time_period="2041-2070")

# saving it
saveRDS(dam2, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/san_future_vol_45_70.rds")


dam2 %>% 
  ggplot()+
  geom_point(aes(x=Date, y=Value, color=Variable))+
  geom_line(aes(x=Date, y=Value, color=Variable), size=1)+
  geom_ribbon(aes(x=Date, ymin=Lower_values, ymax=Upper_values), alpha = .2)+
  labs(x="Time [months]", 
       y="Water volume [Mm3]", 
       title="S.Giustina dam reservoir water volume")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 24),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 30, hjust = 0.5),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        plot.caption = element_text(size=14))+
  scale_x_date(date_breaks = "12 months")


#### RCP85 2021-2050 ####
Q31_rcp85_50<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP85_2020_2050.rds") %>% 
  # readRDS("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/RisultatiOrientGate/Time_series/Antropico/prova/FutureRCP85_2020_2050/Q031_RCP85_2020_2050.rds") %>% 
  dplyr::select(Date, yr, mo, Value_mo) %>% 
  mutate(Inflow_sim_mo=Value_mo) %>% 
  as_tibble()

Q31_rcp85_50$Inflow_sim_lag<-lag(Q31_rcp85_50$Inflow_sim_mo)


rcp85_50_ci_modelle<-predictInterval(model_vol_lmer1, Q31_rcp85_50)

# prediction, upper and lower confidence intervals 
Q31_rcp85_50$vol_fut85.2050<-rcp85_50_ci_modelle$fit
Q31_rcp85_50$Upper_values<-rcp85_50_ci_modelle$upr
Q31_rcp85_50$Lower_values<-rcp85_50_ci_modelle$lwr


# getting rid of unrealistic values
Q31_rcp85_50[which(Q31_rcp85_50$vol_fut85.2050 > 159299502), c("vol_fut85.2050")]<-159299502
Q31_rcp85_50[which(Q31_rcp85_50$Upper_values > 159299502), c("Upper_values")]<-159299502
Q31_rcp85_50[which(Q31_rcp85_50$Lower_values > 159299502), c("Lower_values")]<-159299502

Q31_rcp85_50[which(Q31_rcp85_50$vol_fut85.2050 < 0 ), c("vol_fut85.2050")]<-0
Q31_rcp85_50[which(Q31_rcp85_50$Lower_values < 0 ), c("Lower_values")]<-0

# setting identifiers for plotting all scenarios later 
Q31_rcp85_50$Scenario<-"RCP8.5"
Q31_rcp85_50$Time_period<-"2021-2050"
Q31_rcp85_50$id<-3

# test if lower values are greater than upper values
which(Q31_rcp85_50$Lower_values>Q31_rcp85_50$Upper_values)

# saving it
saveRDS(Q31_rcp85_50, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/SanGiu.Vol_rcp85_50.rds")


dam3<-Q31_rcp85_50 %>% ungroup() %>% 
  dplyr::select(Date, yr, vol_fut85.2050, Upper_values, Lower_values) %>% 
  rename(., RCP85_2021_2050 =vol_fut85.2050) %>% 
  gather(Variable, Value, c(3)) %>% 
  mutate(id=3) %>%   
  filter(yr>2020) %>% 
  ungroup() %>% 
  mutate(Scenario="RCP8.5",
         Time_period="2021-2050")

# saving it
saveRDS(dam3, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/san_future_vol_85_50.rds")

# plot
dam3 %>% 
  ggplot()+
  geom_point(aes(x=Date, y=Value, color=Variable))+
  geom_line(aes(x=Date, y=Value, color=Variable), size=1)+
  geom_ribbon(aes(x=Date, ymin=Lower_values, ymax=Upper_values), alpha = .2)+
  labs(x="Time [months]", 
       y="Water volume [Mm3]", 
       title="S.Giustina dam reservoir water volume")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 24),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 30, hjust = 0.5),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        plot.caption = element_text(size=14))+
  scale_x_date(date_breaks = "12 months")


#### RCP85 2041-2070 ####
Q31_rcp85_70<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP85_2040_2070.rds") %>% 
  dplyr::select(Date, yr, mo, Value_mo) %>% 
  mutate(Inflow_sim_mo=Value_mo) %>% 
  as_tibble()

Q31_rcp85_70$Inflow_sim_lag<-lag(Q31_rcp85_70$Inflow_sim_mo)

# prediction and CI
rcp85_70_ci_modelle<-predictInterval(model_vol_lmer1, Q31_rcp85_70)

Q31_rcp85_70$vol_fut85.2070<-rcp85_70_ci_modelle$fit
Q31_rcp85_70$Upper_values<-rcp85_70_ci_modelle$upr
Q31_rcp85_70$Lower_values<-rcp85_70_ci_modelle$lwr


# getting rid of unrelaistic values
Q31_rcp85_70[which(Q31_rcp85_70$vol_fut85.2070 > 159299502), c("vol_fut85.2070")]<-159299502
Q31_rcp85_70[which(Q31_rcp85_70$Upper_values > 159299502), c("Upper_values")]<-159299502
Q31_rcp85_70[which(Q31_rcp85_70$Lower_values > 159299502), c("Lower_values")]<-159299502

Q31_rcp85_70[which(Q31_rcp85_70$vol_fut85.2070 < 0 ), c("vol_fut85.2070")]<-0
Q31_rcp85_70[which(Q31_rcp85_70$Lower_values < 0 ), c("Lower_values")]<-0

# setting identifiers for plotting all scenarios later 
Q31_rcp85_70$Scenario<-"RCP8.5"
Q31_rcp85_70$Time_period<-"2041-2070"
Q31_rcp85_70$id<-4

# test if lower values are greater than upper values
which(Q31_rcp85_70$Lower_values>Q31_rcp85_70$Upper_values)

# saving it
saveRDS(Q31_rcp85_70, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/SanGiu.Vol_rcp85_70.rds")

dam4<-Q31_rcp85_70 %>% ungroup() %>% 
  dplyr::select(Date, yr, vol_fut85.2070, Upper_values, Lower_values) %>% 
  rename(., RCP85_2041_2070 =vol_fut85.2070) %>% 
  gather(Variable, Value, c(3)) %>% 
  mutate(id=4) %>% 
  filter(yr>2040) %>% 
  ungroup() %>% 
  mutate(Scenario="RCP8.5",
         Time_period="2041-2070")

# saving it
saveRDS(dam4, "C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/san_future_vol_85_70.rds")


dam4 %>%
  ggplot()+
  geom_point(aes(x=Date, y=Value, color=Variable))+
  geom_line(aes(x=Date, y=Value, color=Variable), size=1)+
  geom_ribbon(aes(x=Date, ymin=Lower_values, ymax=Upper_values), alpha = .2)+
  labs(x="Time [months]", 
       y="Water volume [Mm3]", 
       title="S.Giustina dam reservoir water volume")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 24),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 30, hjust = 0.5),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        plot.caption = element_text(size=14))+
  scale_x_date(date_breaks = "12 months")


#### Plotting all ####
dam1<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/SanGiu.Vol_rcp45_50.rds") %>% 
gather(Variable, Value, c("vol_fut45.2050")) %>% 
  dplyr::select(id, Date, yr, mo, Upper_values, Lower_values, Scenario, Time_period, Variable, Value) %>% 
  as_tibble()

dam2<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/SanGiu.Vol_rcp45_70.rds") %>% 
  gather(Variable, Value, c("vol_fut45.2070")) %>% 
  dplyr::select(id, Date, yr, mo, Upper_values, Lower_values, Scenario, Time_period, Variable, Value) %>% 
  as_tibble()

dam3<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/SanGiu.Vol_rcp85_50.rds") %>% 
  gather(Variable, Value, c("vol_fut85.2050")) %>% 
  dplyr::select(id, Date, yr, mo, Upper_values, Lower_values, Scenario, Time_period,Variable, Value) %>% 
  as_tibble()

dam4<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/SanGiu.Vol_rcp85_70.rds") %>% 
  gather(Variable, Value, c("vol_fut85.2070")) %>% 
  dplyr::select(id, Date, yr, mo, Upper_values, Lower_values, Scenario, Time_period, Variable, Value) %>% 
  as_tibble()


supervolume<-rbind(dam1, dam2, dam3, dam4) %>%
  as_tibble() %>% 
  mutate(id=as.factor(id))

# setting colours for volume
col_vol<-c("#a6dba0", "#5aae61", "#c2a5cf", "#9970ab")


vol_plot<-supervolume %>% 
  rename(., Legend=Variable) %>% 
  ggplot(., aes(x=Date, y=Value/10^6, color=id, group=Legend))+
  geom_line(size=1)+
  geom_ribbon(aes(x=Date, ymin=Lower_values/10^6, ymax=Upper_values/10^6), alpha = .2, colour=NA)+
  geom_hline(yintercept=c(Q_vol30/10^6, Q_vol80/10^6), lty=2, cex=0.8)+
  geom_smooth()+
  facet_grid(Scenario ~ Time_period, scales="free_x")+
  labs(x="", y="Volume [Mm3]",
       title="Future water volumes stored")+
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

vol_plot

ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/S.Giustina_volume_all.png", plot=vol_plot,
       width = 16,
       height = 8,
       dpi=600)

#### Monthly variation ####

# extracting montlhy values from datasan for Volume and Outflow
datasan_past<-datasan %>% 
dplyr::select(Date, yr, mo, Inflow_sim_mo, Out_lmer, Vol_lmer) %>% 
  group_by(mo) %>% 
  mutate(Volume_meanmo=mean(Vol_lmer, na.rm=T)) %>% 
  distinct(mo, Volume_meanmo)

# for each predicted volume scenario
#1
dam1_mo<-dam1 %>%
  dplyr::select(Date, yr, mo, Variable, Value) %>%
  spread(Variable, Value) %>% 
  group_by(mo) %>%
  mutate(vol_fut45.2050_meanmo=mean(vol_fut45.2050, na.rm=T)) %>%
  distinct(mo, vol_fut45.2050_meanmo)

#2
dam2_mo<-dam2 %>%
  dplyr::select(Date, yr, mo, Variable, Value) %>%
  spread(Variable, Value) %>% 
  group_by(mo) %>%
  mutate(vol_fut45.2070_meanmo=mean(vol_fut45.2070, na.rm=T)) %>%
  distinct(mo, vol_fut45.2070_meanmo)

#3
dam3_mo<-dam3 %>%
  dplyr::select(Date, yr, mo, Variable, Value) %>%
  spread(Variable, Value) %>% 
  group_by(mo) %>%
  mutate(vol_fut85.2050_meanmo=mean(vol_fut85.2050, na.rm=T)) %>%
  distinct(mo, vol_fut85.2050_meanmo)

#4
dam4_mo<-dam4 %>%
  dplyr::select(Date, yr, mo, Variable, Value) %>%
  spread(Variable, Value) %>% 
  group_by(mo) %>%
  mutate(vol_fut85.2070_meanmo=mean(vol_fut85.2070, na.rm=T)) %>%
  distinct(mo, vol_fut85.2070_meanmo)

vol_month<-merge(dam1_mo, dam2_mo, by="mo") %>% 
  merge(., dam3_mo, by="mo") %>% 
  merge(., dam4_mo, by="mo") %>% 
  merge(., datasan_past, by="mo") %>% 
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

# saving plot
ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/S.Giustina_volume_differences.png", plot=vol_diff,
       width = 16,
       height = 8,
       dpi=600)

####  Averages over period  ####

test<-supervolume %>% 
  group_by(Scenario, Time_period) %>% 
  summarise(Avrg_vol=mean(Value)) %>% 
  mutate(Percent=(Avrg_vol-test3$Avrg_vol)/test3$Avrg_vol*100)
