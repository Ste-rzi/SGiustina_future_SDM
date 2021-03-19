
#*******************************************************************************
# # This script for S.Giustina past observations (volume, inflow, outflow and 
# # minimum ecological flow) cleaning and formatting 
#*******************************************************************************


#----------------------------------Libraries------------------------------------
library("stringr")
library("lubridate")
library("gdata")
library("purrr")
library("reshape2")
library("data.table")
library("tibble")
library("ggplot2")
library("tidyr")
library("proj4")
library("dplyr")
library("scales")
library("tools")
library("zoo")

#-------------------------------From S.Giustina---------------------------------

setwd("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/DATI/GDI_turbinate/Edison/From_SGiustina/")
outdir2<-("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/DATI/GDI_turbinate/Edison/From_SGiustina/")
M_files2<-list.files(pattern=".xls")
em<-list()


for (i in 1:length(M_files2)){
  mf2<-M_files2[i]
  sg<-read.xls(mf2, perl="C:/Strawberry/perl/bin/perl.exe", stringsAsFactors=FALSE)
  
  #first row has to be the names of the columns
  names(sg)<-sg[1,]
  sg<-sg[-1,]
  
  #converting dates
  #necessary command to use as.Date
  lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
  
  #same as previously
  out2<-gather(sg, Years, Value, c(2:length(sg))) %>% 
    mutate(., Date=as.Date(paste(.$`Giorno-mese`,.$Years), format="%d-%b %Y")) %>% 
    mutate(., Station=sub(".*[_]([^-]+)[-].*","\\1",file_path_sans_ext(M_files2[i]))) %>% 
    mutate(., Variable=sub(".*Noce([^_]+)_.*","\\1",file_path_sans_ext(M_files2[i]))) %>% 
    mutate(., Unit= "m3/s") %>% 
    dplyr::select(., Date, Station, Variable, Unit, Value)
  
  #saving the file in a list
  em[[i]]<-out2
  
  print(i)
}  

ou2<-do.call(rbind, em[])
ou2<-as_tibble(ou2)

#write.csv(ou2, file=paste0("SGiustina_Discharges", ".csv"), row.names = F)

ou2$yr<-year(as.Date(ou2$Date))
ou2$mo<-month(as.Date(ou2$Date))

#counting and filtering number of observation
ou3<-ou2 %>%
  drop_na(Value) %>% 
  group_by(Station, Variable, yr, mo) %>%
  mutate(Count_mo=n()) %>% 
  filter(Count_mo>=20) %>% 
  group_by(Station, Variable, yr) %>% 
  mutate(Count_yr=n()) %>% 
  filter(Count_yr>=200) 

ou3$Date<-as.Date(ou3$Date, format="%Y-%m-%d")

# computing monthly and yearly average
ou33<-ou3 %>%
  group_by(Station, Variable, yr, mo) %>% 
  mutate(avrg_mo=mean(Value, na.rm=T)) %>% 
  group_by(Station, Variable, yr) %>% 
  mutate(avrg_yr=mean(avrg_mo, na.rm=T)) %>% 
  dplyr::select(Date, Station, Variable, Unit, Value, yr, mo, Count_mo, Count_yr, avrg_mo, avrg_yr)

ou33<-as.data.frame(ou33)

ouuu<-ou33 %>% filter(Variable=="Turbinate")

ggplot(Taio, aes(x=Taio$Value))+
  geom_histogram(bins=20)+
  labs(x="Discharges", y="Number of observations", title="January discharges distribution")+
  theme(axis.text.x=element_text(size = 20), 
        axis.text.y=element_text(size = 20),
        legend.position="none",
        plot.title = element_text(size=28, face="bold"))


#-------------------------- Stored volume S.Giustina ------------------------
setwd("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/DATI/GDI_turbinate/Edison/S.Giustina/")

# volume curve (to convert the measured level water height into volume)
Giustina_volumecurve<-read.xls("curva S.Giustina.xls", perl="C:/Strawberry/perl/bin/perl.exe", sheet=1, stringsAsFactors=FALSE, na.strings=c("","NA")) [-1,]

giustina1<-as.tibble(apply(Giustina_volumecurve,2, function(x) as.numeric(as.character(x))))

# without considering MEF constraint
giustinaplot<-ggplot(giustina1, aes(x=giustina1$quota.livello.acqua.nel.bacino.s..Giustina, y=giustina1$volume.utile.progressivo))+
  geom_point()+
  stat_smooth()

logiu<-loess(giustina1$volume.utile.progressivo ~ giustina1$quota.livello.acqua.nel.bacino.s..Giustina, control = loess.control(surface = "direct"))


giustina_livelli<-read.xls("Livello invaso SGiustina 1998-2005.xls", perl="C:/Strawberry/perl/bin/perl.exe",stringsAsFactors=FALSE, na.strings = c("","NA"))[-1,]
giu_live<-giustina_livelli[,1:2]


header<-giu_live[1,1:2]
giu_live<-giu_live[-1,]
colnames(giu_live)<-header
rownames(giu_live)<-NULL

giu_live$`QUOTA BACINO`<-as.numeric(giu_live$`QUOTA BACINO`)
giu_live$Variable<-"Volume"
giu_live$Unit<-"Mmc"

giu_water<-giu_live %>% 
  mutate(Date=as.Date(as.POSIXct(giu_live$`DATA E ORA`, tz="Europe/Berlin"))) %>% 
  mutate(Value=`QUOTA BACINO`) %>% 
  dplyr::select(., Date, Variable, Unit, Value) %>% 
  mutate(day=day(Date), mo=month(Date), yr=year(Date)) %>% 
  group_by(Date) %>%
  mutate(avrg_day=mean(Value, na.rm=T)) %>% 
  group_by(yr, mo) %>% 
  mutate(avrg_mo=mean(avrg_day, na.rm=T)) %>% 
  group_by(yr) %>% 
  mutate(avrg_yr=mean(avrg_mo, na.rm=T))

giu_water$Value=predict(logiu, giu_water$Value)
giu_water$avrg_day=predict(logiu, giu_water$avrg_day)
giu_water$avrg_mo=predict(logiu, giu_water$avrg_mo)
giu_water$avrg_yr=predict(logiu, giu_water$avrg_yr)

predict(logiu, 445)

#selecting unique values per each day
giu_water1<-unique(giu_water[c("Date", "avrg_day")])
giu_water1<-giu_water1 %>% mutate(Variable="Volume", Unit="Mmc") %>% 
  dplyr::select(Date, Variable, Unit, avrg_day)

colnames(giu_water1)[4]<-"Value"


## volumes S.giustina 2008-2017
setwd("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/DATI/DatiGDI_misuratori/")

giust2<-read.csv("Gruppo2.csv", sep=";")

giust2$Valore_Medio<-sub(",",".",giust2$Valore_Medio)

giust2<-as.tibble(apply(giust2, 2, function(x) as.numeric(as.character(x))))

giust2_4<-giust2 %>% filter(Accorpamento==4)


giust2_4$Value<-predict(logiu, giust2_4$Valore_Medio)
giust2_4$Variable<-"Volume"
giust2_4$Unit<-"Mmc"

giust2_4$Date<-as.Date(paste(giust2_4$Anno, giust2_4$Mese, giust2_4$Giorno, sep="-"), format="%Y-%m-%d")

giust2_4<-giust2_4 %>% dplyr::select(Date, Variable, Unit, Value)

#filtro outliers erronei
giust2_4<-giust2_4 %>% filter(Value>8) 


# test plot 
ggplot(giust2_4, aes(x=Date, y=Value))+ 
  geom_point()+
  geom_line()+
  labs(x="Dates", y="Water volume [Mmc]",title="S.Giustina Water Volume")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size =10), legend.position="none")+
  scale_x_date(date_labels="%Y", date_breaks="1 years") 


## prova per vedere se nel file gruppo 2_2 ci sono valori del volume di santa giustina  <-<-<-<-<-<-<-<-<-<-???
giust2_2<-giust2 %>% filter(Accorpamento==2)

giust2_2$Value<-giust2_2$Valore_Medio
giust2_2$Variable<-"Volume"
giust2_2$Unit<-"Mmc"

giust2_2$Date<-as.Date(paste(giust2_2$Anno, giust2_2$Mese, giust2_2$Giorno, sep="-"), format="%Y-%m-%d")

giust2_2<-giust2_2 %>% dplyr::select(Date, Variable, Unit, Value)

#plotting daily values
ggplot(giust2_2, aes(x=Date, y=Value))+ 
  geom_point()+
  geom_line()+
  labs(x="Dates", y="Water volume [Mmc]",title="S.Giustina Water Volume")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size =10), legend.position="none")+
  scale_x_date(date_labels="%Y", date_breaks="1 years")


#extracting portate rilasciate da altro file
setwd("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/DATI/DatiGDI_misuratori")

giust2_1<-giust2 %>% filter(Accorpamento==1)

giust2_1$Station<-"Taio"
giust2_1$Variable<-"Scaricate"
giust2_1$Valore_Medio<-giust2_1$Valore_Medio/1000
giust2_1$Unit<-"m3/s"
colnames(giust2_1)[6]<-"Value"

giust2_1$Date<-as.Date(paste(giust2_1$Anno, giust2_1$Mese, giust2_1$Giorno, sep="-"), format="%Y-%m-%d")

giust2_1<-giust2_1 %>% dplyr::select(Date, Station, Variable, Unit, Value)

giusto2_1<-giust2_1 %>% 
  filter(Date>"2010-12-31") %>% 
  drop_na(Value) %>%  
  mutate(yr=year(Date), mo=month(Date)) %>% 
  group_by(Station, Variable, yr, mo) %>%
  mutate(Count_mo=n()) %>% 
  filter(Count_mo>=20) %>% 
  group_by(Station, Variable, yr) %>% 
  mutate(Count_yr=n()) %>% 
  filter(Count_yr>=200) %>% 
  group_by(yr, mo) %>% 
  mutate(avrg_mo=mean(Value, na.rm=T)) %>% 
  group_by(yr) %>% 
  mutate(avrg_yr=mean(avrg_mo, na.rm=T))

giusto2_1<-as.data.frame(giusto2_1)

#extracting portate turbinate da altro file
setwd("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/DATI/DatiGDI_misuratori")

giust2_3<-giust2 %>% filter(Accorpamento==3)

giust2_3$Station<-"Taio"
giust2_3$Variable<-"Turbinate"
giust2_3$Valore_Medio<-giust2_3$Valore_Medio/1000
giust2_3$Unit<-"m3/s"
colnames(giust2_3)[6]<-"Value"

giust2_3$Date<-as.Date(paste(giust2_3$Anno, giust2_3$Mese, giust2_3$Giorno, sep="-"), format="%Y-%m-%d")

giust2_3<-giust2_3 %>% dplyr::select(Date, Station, Variable, Unit, Value)


giusto<-giust2_3 %>% 
  filter(Date>"2010-12-31") %>% 
  drop_na(Value) %>%  
  mutate(yr=year(Date), mo=month(Date)) %>% 
  group_by(Station, Variable, yr, mo) %>%
  mutate(Count_mo=n()) %>% 
  filter(Count_mo>=20) %>% 
  group_by(Station, Variable, yr) %>% 
  mutate(Count_yr=n()) %>% 
  filter(Count_yr>=200) %>% 
  group_by(yr, mo) %>% 
  mutate(avrg_mo=mean(Value, na.rm=T)) %>% 
  group_by(yr) %>% 
  mutate(avrg_yr=mean(avrg_mo, na.rm=T))

giusto<-as.data.frame(giusto)

#Check-up on MEF ----
giust2_5<-giust2 %>% filter(Accorpamento==5)

giust2_5$Station<-"Taio"
giust2_5$Variable<-"MEF"
giust2_5$Valore_Medio<-giust2_5$Valore_Medio/1000
giust2_5$Unit<-"m3/s"
colnames(giust2_5)[6]<-"Value"

giust2_5$Date<-as.Date(paste(giust2_5$Anno, giust2_5$Mese, giust2_5$Giorno, sep="-"), format="%Y-%m-%d")

giust2_5<-giust2_5 %>%
  drop_na(Value) %>%  
  mutate(yr=year(Date), mo=month(Date)) %>% 
  group_by(yr, mo) %>%
  mutate(Count_mo=n()) %>% 
  filter(Count_mo>=20) %>% 
  group_by(yr) %>% 
  mutate(Count_yr=n()) %>% 
  filter(Count_yr>=200) %>% 
  group_by(yr, mo) %>% 
  mutate(avrg_mo=mean(Value, na.rm=T)) %>% 
  group_by(yr) %>% 
  mutate(avrg_yr=mean(avrg_mo, na.rm=T)) %>% 
  dplyr::select(Date, Station, Variable, Unit, Value, yr, mo, Count_mo, Count_yr, avrg_mo, avrg_yr)

giust2_5<-as.data.frame(giust2_5)


# Fluenti, Scaricate,turbinate e MEF dal 1981 al 2017
ou4<-base::rbind(ou33, giusto, giusto2_1, giust2_5)
ou4<-arrange(ou4, -desc(Date))

dodo<-ou4 %>% filter(Variable=="Scaricate")
ggplot(dodo, aes(x=Date, y=Value))+
  geom_point()+
  geom_line()

#write.csv(ou4, file=paste0("Sgiu", ".csv"))


#merging S.giustina volume datafra
giustinall<-base::rbind(giu_water1, giust2_2)

giustinall[c(3930, 3931, 3690, 5299, 5300),4]<-NA

giustinall$Value<-na.approx(giustinall$Value)


giustinall1<-giustinall  %>% 
  mutate(day=day(Date), mo=month(Date), yr=year(Date)) %>% 
  group_by(yr, mo) %>%
  mutate(Count_mo=n()) %>% 
  filter(Count_mo>=20) %>% 
  group_by(yr, mo) %>% 
  mutate(avrg_mo=mean(Value, na.rm=T)) %>% 
  group_by(yr) %>% 
  mutate(Count_yr=n()) %>% 
  filter(Count_yr>=305) %>% 
  mutate(avrg_yr=mean(avrg_mo, na.rm=T))

pplot<-ggplot(giustinall1, aes(x=Date, y=Value))+
  geom_point()+
  geom_line()

ggplotly(pplot)
# write.csv(giustinall1, file=paste0("sgiu_vol",".csv"))

# Inflow data retrieval  -------------------------------------------------------
#assessment first on daily values
ou5<-ou4 %>% 
  dplyr::select(Date, Station, Unit, Variable, yr, mo, Value) %>% 
  spread(Variable, Value)

ou5$Date<-as.Date(ou5$Date, format="%Y-%m-%d")

#setting volume dataframe
giustinall1$Station<-"Taio"

volvol<-giustinall1 %>% 
  dplyr::select(Date, Station, Unit, Variable, yr, mo, Value) %>% 
  spread(Variable, Value)

hot<-inner_join(ou5, volvol, by="Date")
hot$Volume_lag<-lag(hot$Volume)

hot$theoretical_mef<-ifelse(hot$mo.x %in% 
                              c(1,2,3,12), 2.625,
                            ifelse(hot$mo.x %in% c(4,5,6,7,10,11), 3.675, 
                                   3.150))

for (i in 1:nrow(hot)){
  hot$Inflow_sim[i]<-((hot$Volume[i+1]*10^6-hot$Volume[i]*10^6)/86400+(hot$Scaricate[i]+hot$Turbinate[i]+hot$MEF[i]))
  
  # inflow cannot be negative so I assume there's (at least!) the mef as inflow
  hot$Inflow_sim[i]<-ifelse(hot$Inflow_sim[i]<=2.625, 
                            hot$theoretical_mef[i], hot$Inflow_sim[i])
  
  hot$Diff[i]<-hot$Inflow_sim[i]-hot$Fluenti[i]
  
  #using sum function
  hot$Inflow_simsum[i]<-sum((hot$Volume[i+1]*10^6-hot$Volume[i]*10^6)/86400,hot$Scaricate[i],hot$Turbinate[i],hot$MEF[i], na.rm=T)
  # inflow cannot be negative so I assume there's (at least!) the mef as inflow
  hot$Inflow_simsum[i]<-ifelse(hot$Inflow_simsum[i]<=2.625, 
                               hot$theoretical_mef[i], hot$Inflow_simsum[i])
  
  hot$diffsum[i]<-hot$Fluenti[i]-hot$Inflow_simsum[i]
  
  #for the volumes
  hot$Volume_sim[i]<-(hot$Volume[i+1]*10^6/86400+(hot$Inflow_sim[i]-(hot$Turbinate[i]+hot$MEF[i]+hot$Scaricate[i])))*86400
  hot$VolDiff[i]<-hot$Volume[i]*10^6-hot$Volume_sim[i]
}


hot<-data.frame(Date=hot$Date, Station=hot$Station.x, yr=hot$yr.x, mo=hot$mo.x, 
                Outflow_HP=hot$Turbinate, Outflow_MEF=hot$MEF, Outflow_rel=hot$Scaricate,
                Volume=hot$Volume, Inflow=hot$Fluenti, Inflow_sim=hot$Inflow_simsum, Inflow_diff=hot$diffsum) %>% 
  as_tibble()


setwd("C:/Users/STerzi/Documents/R/Noce/HydroNoce/S.Giustina/Vulnerability/")
saveRDS(hot, "Daily_balance_sgiu")

#computing the monthly averages from daily inflows data (from water balance). Portate riportate in -->[m3/month]
hot1<-hot %>% 
  dplyr::group_by(yr, mo) %>%
  mutate(Inflow_mo=sum(Inflow*86400, na.rm=T)) %>% 
  mutate(Outflow_rels_mo=sum(Outflow_rel*86400, na.rm=T)) %>%
  mutate(Outflow_MEF_mo=sum(Outflow_MEF*86400, na.rm=T)) %>% 
  mutate(Outflow_turb_mo=sum(Outflow_HP*86400, na.rm=T)) %>% 
  mutate(Inflow_sim_mo=sum(Inflow_sim*86400, na.rm=T)) %>%
  mutate(Volume_mo=mean(Volume, na.rm=T)) %>% 
  dplyr::select(., Date, Station, yr, mo, Inflow_mo, Outflow_rels_mo, Outflow_MEF_mo, Outflow_turb_mo, Volume_mo, Inflow_sim_mo)


#cleaning dataframe without duplicates
hothot<-hot1[!duplicated(hot1[,c('mo','yr')]),]

hotplot<-hot1 %>% gather(Variable, Value, c(5,10)) %>% 
  ggplot(., aes(x=Date, y=Value/1000000, color=Variable))+
  geom_point()+
  geom_line()+
  ylab("Mm3/month")

setwd("G:/My Drive/01_PhD/06_Database globali/01_Database nazionali/01_dataset Trento/01_NOCE/HYDRO/APRIE/DATI/DatiGDI_misuratori")
write.csv(hothot, file=paste0("sgiu_recap",".csv"))
