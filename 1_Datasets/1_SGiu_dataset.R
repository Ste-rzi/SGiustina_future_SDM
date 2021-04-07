
#*******************************************************************************
# # This script to upload the dataset used in the regression modelling and 
# # check for variables correlations

#*******************************************************************************

#### Libraries ####
library("dplyr")
library("tibble")
library("gdata")
library("zoo")
library("tidyr")
library("lubridate")
library("corrplot")


# created dataset 
datasan<-read.csv("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/DATI/DatiGDI_misuratori/sgiu_recap.csv") %>%
  mutate_at("Date", as.Date) %>% 
  dplyr::select(Date, yr, mo, Inflow_mo, Outflow_turb_mo, Volume_mo, Inflow_sim_mo) %>% 
  mutate(Volume_lag=lag(Volume_mo)*10^6,
         Inflow_sim_lag=lag(Inflow_sim_mo),
         Volume_mo=Volume_mo*10^6,
         Date=as.Date(Date)) %>% 
    as_tibble()

# geotransf inflow data
ingeo<-readRDS("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/RisultatiOrientGate/Time_series/Antropico/prova/AttualeClim/GeoTransf_ref.rds")

sangeot<-inner_join(datasan, ingeo, by="Date", keep=T)

# correlation matrix
sub_sangeot<-sangeot %>% 
  dplyr::select(Outflow_turb_mo, Inflow_mo, Volume_mo, Volume_lag, GeoTransf_inflow_mo) %>% 
  rename("Outflow"= Outflow_turb_mo,
         "Inflow"=Inflow_mo,
         "Volume"=Volume_mo,
         "Volume_lagged"=Volume_lag,
         "GeoTransf_inflow"=GeoTransf_inflow_mo)

a<-cor(sub_sangeot, use="pairwise.complete.obs")

# plot correlation matrix
cp_sangeot<-corrplot(a[1:2,],
                     cl.pos='n',
                     method = 'circle',
                     type = "upper",
                     tl.col = "black",  
                     tl.srt = 50,
                     tl.cex=1,
                     is.corr = T,
                     pch.cex = 1,
                     cl.cex = 1,
                     addCoef.col="red", 
                     number.cex=1)

# selecting only geotransf inflow values
only_geot<-sangeot %>% 
  dplyr::select(Date, GeoTransf_inflow_mo)

# price dataset
pre<-read.csv("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/Prices/Energy_prices.csv") %>%
  spread(Variable, Value) %>%
  group_by(yr, mo) %>%
  distinct(avrg_mo, yr, mo, Unit, .keep_all=F) %>%
  rename(Price = avrg_mo) %>%
  mutate(Date=ymd(paste(yr, mo, "01", sep="-"))) %>%
  ungroup() %>%
  dplyr::select(Date, Price)

sanprice<-inner_join(datasan, pre, by="Date", keep=T)
 
# rainfall dataset
rain<-readRDS("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/R_scripts/Weather/Weatherdata.rds") %>%
  ungroup() %>%
  filter(Variable=="PIOGGIA" & Station=="T0236") %>%
  group_by(yr, mo) %>%
  distinct(avrg_mo, yr, mo, Station, Unit, .keep_all=F) %>%
  rename(Rainfall=avrg_mo) %>%
  mutate(Date=ymd(paste(yr, mo, "01", sep="-"))) %>%
  dplyr::select(Date, Station, Rainfall) %>%
  ungroup() %>%
  dplyr::select(Date, Rainfall)

sanprira<-inner_join(sanprice, rain, by="Date", keep=T)


#  temperature dataset
temp<-readRDS("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/R_scripts/Weather/Weatherdata.rds") %>%
  ungroup() %>%
  filter(Variable=="TEMPERATURA MEDIA" & Station=="T0236") %>%
  group_by(yr, mo) %>%
  distinct(avrg_mo, yr, mo, Station, Unit, .keep_all=F) %>%
  rename(Tempe=avrg_mo) %>%
  mutate(Date=ymd(paste(yr, mo, "01", sep="-"))) %>%
  dplyr::select(Date, Station, Tempe) %>%
  ungroup() %>%
  dplyr::select(Date, Tempe)

sanpritemp<-inner_join(sanprice, temp, by="Date", keep=T)

temp_rain<-inner_join(rain, temp, by="Date", keep=T) %>% 
  inner_join(., datasan, by="Date", keep=T)

# correlation matrix
corr_temp_rain<-temp_rain %>% 
  dplyr::select(Outflow_turb_mo, Volume_mo, Inflow_mo, Volume_lag, mo, Rainfall, Tempe) %>% 
  rename("Outflow"= Outflow_turb_mo,
         "Inflow"=Inflow_mo,
         "Volume"=Volume_mo,
         "Volume_lagged"=Volume_lag,
         "month"= mo,
         "Temperature"=Tempe) %>% 
  cor(., use="pairwise.complete.obs")

# plot correlation matrix
cp_temp_rain<-corrplot(corr_temp_rain,
                  cl.pos='r',
                  method = 'circle',
                  type = "upper",
                  tl.col = "black",  
                  tl.srt = 50,
                  tl.cex=1,
                  is.corr = T,
                  pch.cex = 1,
                  cl.cex = 1,
                  addCoef.col="red", 
                  number.cex=1)

# Careser outflows dataset
cares<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/Careser_Malga_Pian/Careser/02_Datasets/Careser.rds") %>%
  ungroup() %>%
  dplyr::select(Date, Outflow_turb_mo) %>%
  rename(Out_careser=Outflow_turb_mo) %>%
  arrange(-desc(Date))

sancares<-inner_join(datasan, cares, by="Date", keep=T) %>% 
  inner_join(., rain, by="Date", keep=T) 

# correlation matrix
corr_sancares<-sancares %>% 
  dplyr::select(Outflow_turb_mo, Volume_mo, Inflow_mo, Volume_lag, mo, Out_careser) %>% 
  rename("Outflow"= Outflow_turb_mo,
         "Inflow"=Inflow_mo,
         "Volume"=Volume_mo,
         "Volume_lagged"=Volume_lag,
         "month"= mo,
         "Outflow_careser"= Out_careser) %>% 
  cor(., use="pairwise.complete.obs")

# plot correlation matrix
cp_sancares<-corrplot(corr_sancares,
                       cl.pos='r',
                       method = 'circle',
                       type = "upper",
                       tl.col = "black",  
                       tl.srt = 50,
                       tl.cex=1,
                       is.corr = T,
                       pch.cex = 1,
                       cl.cex = 1,
                       addCoef.col="red", 
                       number.cex=1)



#### merging datasets
dt<-inner_join(sanpritemp, cares, by="Date", keep=T) %>%
  inner_join(., rain, by="Date", keep=T) %>% 
  inner_join(., only_geot, by="Date", keep=T) %>% 
  dplyr::select(Outflow_turb_mo, Volume_mo, Inflow_mo, Volume_lag, GeoTransf_inflow_mo, mo, Rainfall, Price, Tempe, Out_careser) %>% 
  rename("Outflow"= Outflow_turb_mo,
         "Inflow"=Inflow_mo,
         "Volume"=Volume_mo,
         "Volume_lagged"=Volume_lag,
         "GeoTransf_inflow"=GeoTransf_inflow_mo,
         "PUN"=Price,
         "Temperature"=Tempe,
         "Outflow_careser"=Out_careser)

a<-cor(dt, use="pairwise.complete.obs")

b<-cor.mtest(a)

# plot correlation matrix
cp_all<-corrplot(a,
                 cl.pos='r',
                 method = 'circle',
                 type = "upper",
                 tl.col = "black",  
                 tl.srt = 50,
                 tl.cex=1,
                 is.corr = T,
                 pch.cex = 1,
                 cl.cex = 1,
                 addCoef.col="red", 
                 number.cex=1)
