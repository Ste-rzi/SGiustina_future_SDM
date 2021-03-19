
# Libraries    -----------------------------------------------------------------
library("lubridate")
library("ggplot2")
library("fitdistrplus")
library("purrr")
library("tidyr")
library("car")
library("MASS")
library("actuar")
library("dplyr")
library("scales")

# SANTA GIUSTINA #
# scenario antropico  ####

#### Baseline AttualeRef ####
setwd("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/RisultatiOrientGate/Time_series/Antropico/prova/AttualeRef/")

fil<-list.files()
Noce_031<-read.delim(fil, header=F, dec=".", sep = "", stringsAsFactors = F)

# deleting first row with header
Noce_031<-Noce_031[-1,]

# selecting V53 catchment closure assigning corresponding dates 
Noce_031_53<-data.frame(Value=Noce_031$V53, Date=seq.Date(as.Date('1980-01-01'), 
                                                          as.Date('2010-12-31'), by = 1))

# deleting first year of simulation
Noce_031_53<-Noce_031_53[-c(1:366),]

# creating daily dataset
Noce_031_53_day <- Noce_031_53 %>% 
  mutate(yr=year(Date)) %>% 
  mutate(mo=month(Date)) %>% 
  mutate(Value_d=Value*86400,
         Unit="m3/day") %>% 
  as_tibble()

# montlhy dataset
Noce_031_53_mo <- Noce_031_53_day %>%
  group_by(yr, mo) %>%
  mutate(Value_mo=sum(Value_d, na.rm=T),
         Unit="m3/month") %>%
  distinct(., yr, mo, .keep_all = T) %>%
  dplyr::select(Date, yr, mo, Value_mo, Unit) %>% 
  as_tibble()

# per analisi giornaliere
Noce_031_53_day<- Noce_031_53_day %>% 
  rename(., GeoTransf_inflow_day=Value_d)

# plotting daily inflow data
ggplot(Noce_031_53_day, aes(x=Date, y=GeoTransf_inflow_day))+
  geom_point()+
  geom_line()

# changing name to monthly values
colnames(Noce_031_53_mo)[4]<-"GeoTransf_inflow_mo"

# plotting monthly inflow values (reported in Mm3/month)
ggplot(Noce_031_53_mo, aes(x=Date, y=GeoTransf_inflow_mo/1000000))+
  geom_point()+
  geom_line()


#### Baseline AttualeClim ####
setwd("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/RisultatiOrientGate/Time_series/Antropico/prova/AttualeClim")

fil<-list.files()
Noce_031<-read.delim(fil[2], header=F, dec=".", sep = "", stringsAsFactors = F)

Noce_031<-Noce_031[-1,]
Noce_031_53<-data.frame(Value=Noce_031$V53, Date=seq.Date(as.Date('1980-01-01'), 
                                                           as.Date('2010-12-31'), by = 1))

Noce_031_53<-Noce_031_53[-c(1:366),]

# daily dataset
Noce_031_53_day <- Noce_031_53 %>% 
  mutate(yr=year(Date)) %>% 
  mutate(mo=month(Date)) %>% 
  mutate(Value_d=Value*86400,
         Unit="m3/day") %>% 
  as_tibble()

# monthly dataset
Noce_031_53_mo <- Noce_031_53_day %>%
  group_by(yr, mo) %>%
  mutate(Value_mo=sum(Value_d, na.rm=T),
         Unit="m3/month") %>%
  distinct(., yr, mo, .keep_all = T) %>%
  dplyr::select(Date, yr, mo, Value_mo, Unit) %>% 
  as_tibble()

# changing name to monthly values
colnames(Noce_031_53_mo)[4]<-"GeoTransf_inflow_mo"

# plotting monthly inflow values (reported in Mm3/month)
ggplot(Noce_031_53_mo, aes(x=Date, y=GeoTransf_inflow_mo/1000000))+
  geom_point()+
  geom_line()


# confrontare questa portata con quella misurata --> è già stato calibrato e validato!!
# daily based --> uso dt: hot

setwd()
hot<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/Vulnerability/Daily_balance_sgiu")

gianni_day<-merge(Noce_031_53_day, hot, by="Date")

# confronto giornaliero GeoTransf e inflow reale
gianni_day %>% 
  gather(Variable, Value, c("Value", "Inflow")) %>%
  ggplot(., aes(x=Date, y=Value, color=Variable))+
  geom_point()+
  geom_line()


# monthly average --> flows (cioè hothot salvato come sgiu_recap)
setwd("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/DATI/DatiGDI_misuratori")

flows<-read.csv("sgiu_recap.csv")

# change format Date
flows$Date<-as.Date(flows$Date)

# removing montlhy duplicates
flows<-flows[!duplicated(flows[,c("yr", "mo")]),]

inflow<-flows %>% dplyr::select(Date, Inflow_mo)

inflow_mod<-Noce_031_53_mo %>%
  ungroup() %>% 
  dplyr::select(Date, GeoTransf_inflow_mo)

# saveRDS(inflow_mod, file="G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/RisultatiOrientGate/Time_series/Antropico/prova/AttualeClim/GeoTransf_ref.rds")

gianni<-merge(inflow_mod, inflow, by="Date")

saveRDS(gianni, file="gianni.rds")

# e anche il giornaliero
# saveRDS(gianni_day, file="gianni_day.rds")


ggianni<-gianni %>% gather(Variable, Value, c(2,3))

geot_q30<-quantile(gianni$Inflow_mo/10^6, 0.30)
geot_q80<-quantile(gianni$Inflow_mo/10^6, 0.80)


#### RCP45 2021-2050 ####
setwd("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/RisultatiOrientGate/Time_series/Antropico/prova/FutureRCP45_2020_2050/")

fil<-list.files()
Noce_031<-read.delim(fil[1], header=F, dec=".", sep = "", stringsAsFactors = F)

Noce_031<-Noce_031[-1,]
Noce_031_53<-data.frame(Value=Noce_031$V53, Date=seq.Date(as.Date('2020-01-01'), 
                                                          as.Date('2050-12-31'), by = 1))

Noce_031_53<-Noce_031_53[-c(1:366),]

# daily dataset
Noce_031_53_day <- Noce_031_53 %>% 
  mutate(yr=year(Date)) %>% 
  mutate(mo=month(Date)) %>% 
  mutate(Value_d=Value*86400,
         Unit="m3/day") %>% 
  as_tibble()

# saveRDS(Noce_031_53_day, "Q031_RCP45_2020_2050_day.rds")

# monthly dataset
Noce_031_53_mo <- Noce_031_53_day %>%
  group_by(yr, mo) %>%
  mutate(Value_mo=sum(Value_d, na.rm=T),
         Unit="m3/month") %>%
  distinct(., yr, mo, .keep_all = T) %>%
  dplyr::select(Date, yr, mo, Value_mo, Unit) %>% 
  as_tibble()

saveRDS(Noce_031_53_mo, file="~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP45_2020_2050.rds")

# changing name to monthly values
colnames(Noce_031_53_mo)[4]<-"GeoTransf_RCP45_2021_2050"


ggplot(Noce_031_53_mo, aes(x=Date, y=GeoTransf_RCP45_2021_2050/1000000))+
  geom_point()+
  geom_line()


dt_geot45_50<-Noce_031_53_mo %>%
  gather(Variable, Value, c(4)) %>%
  mutate(id=ifelse(yr<=2035, 0, 1)) %>%
  group_by(id) %>%
  mutate(Mean_id=mean(Value))


#### RCP45 2041-2070 ####
setwd("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/RisultatiOrientGate/Time_series/Antropico/prova/FutureRCP45_2040_2070/")

fil<-list.files()
Noce_031<-read.delim(fil[1], header=F, dec=".", sep = "", stringsAsFactors = F)

Noce_031<-Noce_031[-1,]
Noce_031_53<-data.frame(Value=Noce_031$V53, Date=seq.Date(as.Date('2040-01-01'), 
                                                          as.Date('2070-12-31'), by = 1))

Noce_031_53<-Noce_031_53[-c(1:365),]

# daily dataset
Noce_031_53_day <- Noce_031_53 %>% 
  mutate(yr=year(Date)) %>% 
  mutate(mo=month(Date)) %>% 
  mutate(Value_d=Value*86400,
         Unit="m3/day") %>% 
  as_tibble()

# monthly dataset
Noce_031_53_mo <- Noce_031_53_day %>%
  filter(yr>2040) %>% 
  group_by(yr, mo) %>%
  mutate(Value_mo=sum(Value_d, na.rm=T),
         Unit="m3/month") %>%
  distinct(., yr, mo, .keep_all = T) %>%
  dplyr::select(Date, yr, mo, Value_mo, Unit) %>% 
  as_tibble()

# save file
saveRDS(Noce_031_53_mo, file="~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP45_2040_2070.rds")

# changing name to monthly values
colnames(Noce_031_53_mo)[4]<-"GeoTransf_RCP45_2041_2070"

ggplot(Noce_031_53_mo, aes(x=Date, y=GeoTransf_RCP45_2041_2070/1000000))+
  geom_point()+
  geom_line()


dt_geot45_70<-Noce_031_53_mo %>%
  gather(Variable, Value, c(4)) %>%
  mutate(id=ifelse(yr<=2055, 0, 1)) %>%
  group_by(id) %>%
  mutate(Mean_id=mean(Value))


#### RCP85 2020-2050 ####
setwd("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/RisultatiOrientGate/Time_series/Antropico/prova/FutureRCP85_2020_2050/")

fil<-list.files()
Noce_031<-read.delim(fil[1], header=F, dec=".", sep = "", stringsAsFactors = F)

Noce_031<-Noce_031[-1,]
Noce_031_53<-data.frame(Value=Noce_031$V53, Date=seq.Date(as.Date('2020-01-01'), 
                                                          as.Date('2050-12-31'), by = 1))

Noce_031_53<-Noce_031_53[-c(1:365),]

# daily dataset
Noce_031_53_day <- Noce_031_53 %>% 
  mutate(yr=year(Date)) %>% 
  mutate(mo=month(Date)) %>% 
  mutate(Value_d=Value*86400,
         Unit="m3/day") %>% 
  as_tibble()


# saveRDS(Noce_031_53, "Q031_RCP85_2020_2050_day.rds")

# monthly dataset
Noce_031_53_mo <- Noce_031_53_day %>%
  filter(yr>2020) %>% 
  group_by(yr, mo) %>%
  mutate(Value_mo=sum(Value_d, na.rm=T),
         Unit="m3/month") %>%
  distinct(., yr, mo, .keep_all = T) %>%
  dplyr::select(Date, yr, mo, Value_mo, Unit) %>% 
  as_tibble()

# save dataset
saveRDS(Noce_031_53_mo, file="~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP85_2020_2050.rds")


# changing name to monthly values
colnames(Noce_031_53_mo)[4]<-"GeoTransf_RCP85_2021_2050"

ggplot(Noce_031_53_mo, aes(x=Date, y=GeoTransf_RCP85_2021_2050/1000000))+
  geom_point()+
  geom_line()

# saveRDS(Noce_031_53_mo, "Q031_RCP85_2020_2050.rds")

dt_geot85_50<-Noce_031_53_mo %>%
  gather(Variable, Value, c(4)) %>%
  mutate(id=ifelse(yr<=2035, 0, 1)) %>%
  group_by(id) %>%
  mutate(Mean_id=mean(Value))

#### RCP85 2040-2070 ####
setwd("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/RisultatiOrientGate/Time_series/Antropico/prova/FutureRCP85_2040_2070/")

fil<-list.files()
Noce_031<-read.delim(fil[1], header=F, dec=".", sep = "", stringsAsFactors = F)

Noce_031<-Noce_031[-1,]
Noce_031_53<-data.frame(Value=Noce_031$V53, Date=seq.Date(as.Date('2040-01-01'), 
                                                          as.Date('2070-12-31'), by = 1))

Noce_031_53<-Noce_031_53[-c(1:365),]

# daily dataset
Noce_031_53_day <- Noce_031_53 %>% 
  mutate(yr=year(Date)) %>% 
  mutate(mo=month(Date)) %>% 
  mutate(Value_d=Value*86400,
         Unit="m3/day") %>% 
  as_tibble()

# saveRDS(Noce_031_53, "Q031_RCP85_2040_2070_day.rds")


# monthly dataset
Noce_031_53_mo <- Noce_031_53_day %>%
  filter(yr>2040) %>% 
  group_by(yr, mo) %>%
  mutate(Value_mo=sum(Value_d, na.rm=T),
         Unit="m3/month") %>%
  distinct(., yr, mo, .keep_all = T) %>%
  dplyr::select(Date, yr, mo, Value_mo, Unit) %>% 
  as_tibble()


# save dataset
saveRDS(Noce_031_53_mo, file="~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/Q031_RCP85_2040_2070.rds")


# changing name to monthly values
colnames(Noce_031_53_mo)[4]<-"GeoTransf_RCP85_2041_2070"

ggplot(Noce_031_53_mo, aes(x=Date, y=GeoTransf_RCP85_2041_2070/1000000))+
  geom_point()+
  geom_line()


dt_geot85_70<-Noce_031_53_mo %>%
  gather(Variable, Value, c(4)) %>%
  mutate(id=ifelse(yr<=2055, 0, 1)) %>%
  group_by(id) %>%
  mutate(Mean_id=mean(Value))

# saveRDS(Noce_031_53, "Q031_RCP85_2040_2070.rds")

#### merging - plotting dt ####
geotransfone<-rbind(dt_geot45_70, dt_geot45_50, dt_geot85_50, dt_geot85_70)

ggplot(geotransfone, aes(x=Date, y=Value/1000000, color=Variable))+
  geom_point()+
  geom_line()+
  facet_wrap(~ Variable, scales ="free_x")+
  theme(legend.position = "none")+
  labs(x="Time [months]", 
       y="Mm3/month",
       title="Future water inflow for RCP4.5 and 8.5 ")+ 
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 30, hjust = 0.5),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        plot.caption = element_text(size=18),
        strip.text = element_text(size=20))+
  scale_x_date(breaks = pretty_breaks(13))

  
geotransfone<-geotransfone %>% arrange(-desc(Date))

# boxplot
ggplot(geotransfone, aes(x=Date, y=Value, color=Variable, group=id))+
  geom_boxplot()+
  facet_wrap(~ Variable, scales ="free_x")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 24),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 30, hjust = 0.5),
        axis.title=element_text(size = 24),
        legend.text=element_text(size=22),
        legend.title = element_text(size=24),
        plot.caption = element_text(size=14),
        strip.text = element_text(size=20))

# visualization
geotransfone



