
# Libraries    -----------------------------------------------------------------
library("ggplot2")
library("tidyr")
library("dplyr")
library("scales")


#### Uploading datasets ####
# past values
san_baseline_vol<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/4-5_Past/4_Past_volume/sangiu_volume_baseline.rds")
  
san_baseline_out<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/4-5_Past/4_Past_volume/sangiu_outflow_baseline.rds")


Inflow_rcp45_2050<-readRDS(file="~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP45_2020_2050.rds") %>% 
  rename(Inflow=Value_mo) %>% 
  mutate(id=1,
         Scenario="RCP4.5",
         Time_period="2021-2050")

Inflow_rcp45_2070<-readRDS(file="~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP45_2040_2070.rds") %>% 
  rename(Inflow=Value_mo) %>% 
  mutate(id=2, 
         Scenario="RCP4.5",
         Time_period="2041-2070")

Inflow_rcp85_2050<-readRDS(file="~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP85_2020_2050.rds") %>% 
  rename(Inflow=Value_mo) %>% 
  mutate(id=3, 
         Scenario="RCP8.5",
         Time_period="2021-2050")

Inflow_rcp85_2070<-readRDS(file="~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP85_2040_2070.rds") %>% 
  rename(Inflow=Value_mo) %>% 
  mutate(id=4, 
         Scenario="RCP8.5",
         Time_period="2041-2070")

# creating one dataframe
inflow_all<-rbind(Inflow_rcp45_2050, Inflow_rcp45_2070, Inflow_rcp85_2050, Inflow_rcp85_2070) %>% 
  gather(Variable, Value, Inflow) %>% 
  mutate(id=as.factor(id))

# computing average over the whole time period per scenario
test<-inflow_all %>% 
  group_by(Scenario, Time_period, Unit, mo) %>% 
  summarise(Avrg_mo_in=mean(Value/10^6)) %>% 
  ggplot()+
  geom_bar(aes(x=mo, y=Avrg_mo_in, fill=Scenario),
           stat="identity", width=0.7, position = position_dodge(width=0.7))+
  geom_hline(yintercept = 71.38, lty=2)+
  facet_grid(Scenario ~ Time_period, scales="free_x")

test

test1<-inflow_all %>% 
  group_by(Scenario, Time_period, Unit) %>% 
  summarise(Avrg=mean(Value)) %>% 
  mutate(Percent=(Avrg-test3$Avrg_in)/test3$Avrg_in*100)

test2<-san_baseline_vol %>% 
  dplyr::select(Date, yr, mo, Outflow_turb_mo, Inflow_sim_mo, Vol_lmer) %>%
  group_by(mo) %>% 
  summarise(Avrg_mo_in=mean(Inflow_sim_mo),
        Avrg_mo_vol=mean(Vol_lmer))

test3<-san_baseline_vol %>% 
  dplyr::select(Date, yr, mo, Outflow_turb_mo, Inflow_sim_mo, Vol_lmer) %>%
  summarise(Avrg_in=mean(Inflow_sim_mo),
            Avrg_vol=mean(Vol_lmer))

test4<-san_baseline_out %>% 
  dplyr::select(Date, yr, mo, Out_lmer) %>%
  summarise(Avrg_out=mean(Out_lmer, na.rm=T))
  

# saving quantiles from past real values
Q_in10<-quantile(san_baseline_vol$Inflow_sim_mo, 0.10, na.rm=T)
Q_in30<-quantile(san_baseline_vol$Inflow_sim_mo, 0.30, na.rm=T)
Q_in80<-quantile(san_baseline_vol$Inflow_sim_mo, 0.80, na.rm=T)
Q_in90<-quantile(san_baseline_vol$Inflow_sim_mo, 0.90, na.rm=T)


#### Plot inflow values ####

col_inflo<-c("#9a9a9a", "#838383", "#676767", "#323232")
  # c("#a6373f", "#530006", "#50A162", "#00400E")

inflow_plot<-inflow_all %>% 
  rename(., Legend=Variable) %>% 
  ggplot(., aes(x=Date, y=Value/10^6, color=id, group=Legend))+
  geom_line(size=1)+
  geom_hline(yintercept=c(Q_in30/10^6, Q_in80/10^6), lty=2, cex=0.8)+
  geom_smooth()+
  facet_grid(Scenario ~ Time_period, scales="free_x")+
  labs(x="", y="Inflow [Mm3/month]",
       title="Future water inflows")+
  # \nTime [Months]
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

ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/S.Giustina_inflow_all.png", plot=inflow_plot,
       width = 16,
       height = 8,
       dpi=600)


#### Monthly variation ####

# extracting montlhy values from datasan for Volume and Outflow
inflow_all_mo<-inflow_all %>% 
  group_by(Scenario, Time_period, mo) %>% 
  mutate(Inflow_meanmo=mean(Value, na.rm=T)) %>% 
  distinct(mo, Inflow_meanmo) %>% 
  mutate(Percentage=(Inflow_meanmo-test3$Avrg_in)/test3$Avrg_in*100) %>% 
  ggplot()+
  geom_bar(aes(x=mo, y=Percentage, fill=Scenario),
           stat="identity", width=0.7, position = position_dodge(width=0.7))+
  scale_x_discrete(limits = month.abb)+
  facet_grid(Time_period ~ Scenario, scales="free")+
  labs(x="\nTime [Months]", y="Volume differences [%]",
       title="Comparison of past and future water inflow")+
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
        legend.box.background = element_rect(colour = "black", size=0.5))

inflow_all_mo
