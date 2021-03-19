
#*******************************************************************************
# # This script to upload the dataset used in the regression modelling 

#*******************************************************************************


#### libraries ####
library("dplyr")
library("tibble")
library("gdata")
library("zoo")
library("tidyr")
library("lubridate")


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

# Careser outflows dataset
cares<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/Careser/Careser.rds") %>%
  ungroup() %>%
  dplyr::select(Date, Outflow_turb_mo) %>%
  rename(Out_careser=Outflow_turb_mo) %>%
  arrange(-desc(Date))

sancares<-inner_join(datasan, cares, by="Date", keep=T)

#### merging datasets
dt<-inner_join(sanprice, cares, by="Date", keep=T)
