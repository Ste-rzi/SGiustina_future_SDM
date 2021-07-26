
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

#### Daily time step ####

# available dataset
sgiudaily<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/Vulnerability/Daily_balance_sgiu") %>% 
  dplyr::select(Date, yr, mo, Outflow_HP, Outflow_MEF, Outflow_rel, Volume, Inflow_sim) %>% 
  mutate(Volume_lag=lag(Volume)*10^6,
         Inflow_sim=Inflow_sim*86400,
         Outflow_HP=Outflow_HP*86400,
         Outflow_MEF=Outflow_MEF*86400,
         Outflow_rel=Outflow_rel*86400,
         Inflow_sim_lag=lag(Inflow_sim),
         Volume=Volume*10^6,
         Date=as.Date(Date),
         WD=weekdays(Date)) %>% 
  as_tibble()

# price dataset
pre_day<-read.csv("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/Prices/Energy_prices.csv") %>%
  spread(Variable, Value) %>%
  rename(Price_day = Price) %>%
  mutate(Date=ymd(paste(yr, mo, "01", sep="-"))) %>%
  ungroup() %>%
  dplyr::select(Date, Price_day)

sanprice_day<-inner_join(sgiudaily, pre_day, by="Date", keep=T) 

# correlation matrix
corr_sanprice<-sanprice_day %>% 
  dplyr::select(Outflow_HP, Volume, Inflow_sim, Volume_lag, mo, Price_day) %>% 
  rename("Outflow"= Outflow_HP,
         "Inflow"=Inflow_sim,
         "Volume"=Volume,
         "Volume_lagged"=Volume_lag,
         "PUN"=Price_day) %>% 
  cor(., use="pairwise.complete.obs")

# plot correlation matrix
cp_price<-corrplot(corr_sanprice[1:2,],
             cl.pos='r',
             method = 'circle',
             type = "upper",
             tl.col = "black",  
             tl.srt = 50,
             tl.cex=1,
             is.corr = T,
             pch.cex = 1,
             cl.cex = 0.75,
             addCoef.col="red", 
             number.cex=1)

# rainfall dataset
rain_day<-readRDS("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/R_scripts/Weather/Weatherdata.rds") %>%
  ungroup() %>%
  filter(Variable=="PIOGGIA" & Station=="T0236") %>%
  rename(Rainfall=Value) %>%
  mutate(Date=ymd(paste(yr, mo, "01", sep="-"))) %>%
  dplyr::select(Date, Station, Rainfall) %>%
  ungroup() %>%
  dplyr::select(Date, Rainfall)

sanprira_day<-inner_join(sanprice_day, rain_day, by="Date", keep=T)

# correlation matrix
corr_sanprira_day<-sanprira_day%>% 
  dplyr::select(Outflow_HP, Volume, Inflow_sim, Volume_lag, mo, Rainfall, Price_day) %>% 
  rename("Outflow"= Outflow_HP,
         "Inflow"=Inflow_sim,
         "Volume"=Volume,
         "Volume_lagged"=Volume_lag,
         "PUN"=Price_day) %>% 
  cor(., use="pairwise.complete.obs")

# plot correlation matrix
cp_rain_day<-corrplot(corr_sanprira_day[1:2,],
             cl.pos='r',
             method = 'circle',
             type = "upper",
             tl.col = "black",  
             tl.srt = 50,
             tl.cex=1,
             is.corr = T,
             pch.cex = 1,
             cl.cex = 0.75,
             addCoef.col="red", 
             number.cex=1)

#  temperature dataset
temp_day<-readRDS("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/R_scripts/Weather/Weatherdata.rds") %>%
  ungroup() %>%
  filter(Variable=="TEMPERATURA MEDIA" & Station=="T0236") %>%
  rename(Tempe=Value) %>%
  dplyr::select(Date, Station, Tempe) %>%
  ungroup() %>%
  dplyr::select(Date, Tempe)

santemp_day<-inner_join(sgiudaily, temp_day, by="Date", keep=T) %>% 
  mutate(WD=wday(Date)) 
# %>% 
#   inner_join(., rain_day, by="Date", keep=T)

# correlation matrix
corr_santemp_day<-santemp_day %>% 
  dplyr::select(Outflow_HP, Volume, Inflow_sim, Volume_lag, mo, Tempe, WD) %>% 
  rename("Outflow"= Outflow_HP,
         "Inflow"=Inflow_sim,
         "Volume"=Volume,
         "Volume_lagged"=Volume_lag,
         "Temperature"=Tempe,
         "Weekday"=WD) %>%
  cor(., use="pairwise.complete.obs")

# plot correlation matrix
cp_temp_day<-corrplot(corr_santemp_day[1:2,],
             cl.pos='r',
             method = 'circle',
             type = "upper",
             tl.col = "black",  
             tl.srt = 50,
             tl.cex=1,
             is.corr = T,
             pch.cex = 1,
             cl.cex = 0.75,
             addCoef.col="red", 
             number.cex=1)


# Price, temperature and rain
sanpriratemp_day<-inner_join(sanprira_day, temp_day, by="Date", keep=T) %>% 
  mutate(WD=wday(Date)) 
# %>% 
#   inner_join(., rain_day, by="Date", keep=T)

# correlation matrix
corr_sanpriratemp_day<-sanpriratemp_day %>% 
  dplyr::select(Outflow_HP, Volume, Inflow_sim, Volume_lag, mo, Tempe, WD) %>% 
  rename("Outflow"= Outflow_HP,
         "Inflow"=Inflow_sim,
         "Volume"=Volume,
         "Volume_lagged"=Volume_lag,
         "Temperature"=Tempe,
         "Weekday"=WD) %>%
  cor(., use="pairwise.complete.obs")

# plot correlation matrix
cp_sanpriratemp_day<-corrplot(corr_sanpriratemp_day[1:2,],
                      cl.pos='r',
                      method = 'circle',
                      type = "upper",
                      tl.col = "black",  
                      tl.srt = 50,
                      tl.cex=1,
                      is.corr = T,
                      pch.cex = 1,
                      cl.cex = 0.75,
                      addCoef.col="red", 
                      number.cex=1)

# Careser outflows dataset
cares_day<-readRDS("C:/Users/STerzi/Documents/R/Noce/HydroNoce/Careser_Malga_Pian/Careser/Careser_daily.rds") %>%
  ungroup() %>%
  dplyr::select(Date, Outflow_turb_day) %>%
  rename(Out_careser=Outflow_turb_day) %>%
  arrange(-desc(Date))

sancares_day<-inner_join(sgiudaily, cares_day, by="Date", keep=T)

a<-sancares_day %>% 
  dplyr::select(Outflow_HP, Volume, Inflow_sim, Volume_lag, mo, Out_careser) %>% 
  rename("Outflow"= Outflow_HP,
         "Inflow"=Inflow_sim,
         "Volume"=Volume,
         "Volume_lagged"=Volume_lag,
         "Outflow_careser"=Out_careser) %>% 
  cor(., use="pairwise.complete.obs")

# plot correlation matrix
cp_careser_day<-corrplot(a[1:2,],
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

# #### merging datasets
dt_corr<-inner_join(sanpriratemp_day, cares_day, by="Date", keep=T) %>%
  dplyr::select(Outflow_HP, Volume, Inflow_sim, Volume_lag, Rainfall, Price_day, Tempe, Out_careser) %>% 
  rename("Outflow"= Outflow_HP,
         "Inflow"=Inflow_sim,
         "Volume"=Volume,
         "Volume_lagged"=Volume_lag,
         "PUN"=Price_day,
         "Temperature"=Tempe,
         "Outflow_careser"=Out_careser)

a<-cor(dt_corr, use="pairwise.complete.obs")

b<-cor.mtest(a)
  
# plot correlation matrix
cp_all_day<-corrplot(a,
             cl.pos='r',
             method = 'color',
             type = "upper",
             tl.col = "black",  
             tl.srt = 50,
             tl.cex=1,
             is.corr = T,
             pch.cex = 1,
             cl.cex = 1,
             addCoef.col="red", 
             number.cex=1)



#### Monthly time step ####
# available dataset
datasan<-read.csv("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/DATI/DatiGDI_misuratori/sgiu_recap.csv") %>%
  mutate_at("Date", as.Date) %>% 
  dplyr::select(Date, yr, mo, Inflow_mo, Outflow_turb_mo, Volume_mo, Inflow_sim_mo) %>% 
  mutate(Volume_lag=lag(Volume_mo)*10^6,
         Inflow_sim_lag=lag(Inflow_sim_mo),
         Volume_mo=Volume_mo*10^6,
         Date=as.Date(Date)) %>% 
  as_tibble()


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

# correlation matrix
corr_sanprice<-sanprice %>% 
  dplyr::select(Outflow_turb_mo, Volume_mo, Inflow_mo, Volume_lag, mo, Price) %>% 
  rename("Outflow"= Outflow_turb_mo,
         "Inflow"=Inflow_mo,
         "Volume"=Volume_mo,
         "Volume_lagged"=Volume_lag,
         "PUN"=Price) %>% 
  cor(., use="pairwise.complete.obs")

# plot correlation matrix
cp_price<-corrplot(corr_sanprice[1:2,],
                   cl.pos='r',
                   method = 'circle',
                   type = "upper",
                   tl.col = "black",  
                   tl.srt = 50,
                   tl.cex=1,
                   is.corr = T,
                   pch.cex = 1,
                   cl.cex = 0.75,
                   addCoef.col="red", 
                   number.cex=1)


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

# correlation matrix
corr_sanprira<-sanprira%>% 
  dplyr::select(Outflow_turb_mo, Volume_mo, Inflow_mo, Volume_lag, mo, Rainfall, Price) %>% 
  rename("Outflow"= Outflow_turb_mo,
         "Inflow"=Inflow_mo,
         "Volume"=Volume_mo,
         "Volume_lagged"=Volume_lag,
         "PUN"=Price) %>% 
  cor(., use="pairwise.complete.obs")

# plot correlation matrix
cp_rain<-corrplot(corr_sanprira[1:2,],
                  cl.pos='r',
                  method = 'circle',
                  type = "upper",
                  tl.col = "black",  
                  tl.srt = 50,
                  tl.cex=1,
                  is.corr = T,
                  pch.cex = 1,
                  cl.cex = 0.75,
                  addCoef.col="red", 
                  number.cex=1)

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

sanpritemp<-inner_join(sanprira, temp, by="Date", keep=T) %>% 
  inner_join(., rain, by="Date", keep=T) 

# correlation matrix
corr_sanpritemp<-sanpritemp %>% 
  dplyr::select(Outflow_turb_mo, Volume_mo, Inflow_mo, Volume_lag, mo, Tempe, Price, Rainfall) %>% 
  rename("Outflow"= Outflow_turb_mo,
         "Inflow"=Inflow_mo,
         "Volume"=Volume_mo,
         "Volume_lagged"=Volume_lag,
         "PUN"=Price,
         "Temperature"=Tempe) %>% 
  cor(., use="pairwise.complete.obs")

# plot correlation matrix
cp_temp<-corrplot(corr_sanpritemp[1:2,],
                  cl.pos='r',
                  method = 'circle',
                  type = "upper",
                  tl.col = "black",  
                  tl.srt = 50,
                  tl.cex=1,
                  is.corr = T,
                  pch.cex = 1,
                  cl.cex = 0.75,
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

a<-sancares %>% 
  dplyr::select(Outflow_turb_mo, Volume_mo, Inflow_mo, Volume_lag, mo, Out_careser, Rainfall) %>% 
  rename("Outflow"= Outflow_turb_mo,
         "Inflow"=Inflow_mo,
         "Volume"=Volume_mo,
         "Volume_lagged"=Volume_lag,
         "Outflow_careser"=Out_careser) %>% 
  cor(., use="pairwise.complete.obs")

# plot correlation matrix
cp_careser<-corrplot(a[1:2,],
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

# #### merging datasets
dt<-inner_join(sanpritemp, cares, by="Date", keep=T) %>%
  inner_join(., rain, by="Date", keep=T) %>% 
  # inner_join(., only_geot, by="Date", keep=T) %>% 
  dplyr::select(Outflow_turb_mo, Volume_mo, Inflow_mo, Volume_lag, mo, Rainfall, Price, Tempe, Out_careser) %>% 
  rename("Outflow"= Outflow_turb_mo,
         "Inflow"=Inflow_mo,
         "Volume"=Volume_mo,
         "Volume_lagged"=Volume_lag,
         "PUN"=Price,
         "Temperature"=Tempe,
         "Outflow_careser"=Out_careser)

a<-cor(dt, use="pairwise.complete.obs")

b<-cor.mtest(a)

# plot correlation matrix
cp_all<-corrplot(a,
                 cl.pos='r',
                 method = 'color',
                 type = "upper",
                 tl.col = "black",  
                 tl.srt = 50,
                 tl.cex=1,
                 is.corr = T,
                 pch.cex = 1,
                 cl.cex = 1,
                 addCoef.col="red", 
                 number.cex=1)
