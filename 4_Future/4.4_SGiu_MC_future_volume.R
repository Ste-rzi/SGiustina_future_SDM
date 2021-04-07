

#*******************************************************************************
# # This script to perform MonteCarlo analysis and counting the number of events
# # with volume greater and lower than the 30th and 80th quantiles from the baseline

#*******************************************************************************

#### Libraries  ####
library("lubridate")
library("tidyr")
library("ggplot2")
library("dplyr")
library("gridExtra")
library("caret")
library("doParallel")
library("foreach")
library("plotly")


#### Uploading dt ####
# baseline dataset
san_baseline_vol<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/4-5_Past/4_Past_volume/sangiu_volume_baseline.rds")

# dataset with future volume
dam1<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/san_future_vol_45_50.rds")

dam2<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/san_future_vol_45_70.rds")

dam3<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/san_future_vol_85_50.rds")

dam4<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/san_future_vol_85_70.rds")


# quantiles from baseline
# quantiles for past volume values 
Q_vol10<-quantile(san_baseline_vol$Volume_mo, 0.10, na.rm=T)
Q_vol90<-quantile(san_baseline_vol$Volume_mo, 0.90, na.rm=T)

Q_vol30<-quantile(san_baseline_vol$Volume_mo, 0.30, na.rm=T)
Q_vol80<-quantile(san_baseline_vol$Volume_mo, 0.80, na.rm=T)

# MonteCarlo sampling

# replicating future volumes based on confidence interval
# and sampling from it in order to use a montecarlo approach

#setting number of  sampling within the confidence interval
# extension of sampling form low to high confidence interval


#### Q30 and Q80 ####
strt<-Sys.time()
sizze=192*52
iterac<-1


lst_q30<-list()
lst_q80<-list()

N_cores<-detectCores()
registerDoParallel(cores = (N_cores-1))

for (s in iterac){
  
  #### * 2021-2050 ####
  # (both rcp4.5 and 8.5)
  
  sample_CI<-list()
  temp<-list()
  lista_dam50<-list(dam1, dam3)
  Date<-seq.Date(as.Date("2021-01-01"), as.Date("2050-12-01"), by="1 month")
  
  
  for (j in 1:length(lista_dam50)){
    l<-foreach (i = 1:nrow(dam1), .inorder = T) %dopar% {
      
      # creating a sequence of values within the range of lower and upper quantiles
      lala<-seq(as.numeric(lista_dam50[[j]][i,c("Lower_values")]), as.numeric(lista_dam50[[j]][i,c("Upper_values")]), by=1000000)
      
      #sampling from such sequence
      sample_CI[[i]]<-sample(lala, size=sizze, replace=T)
    }
    
    #binding dates and sampled values
    temp[[j]]<-cbind(as.data.frame(do.call(rbind, l)), Date)
  }
  # plotting many replicates all together with quantiles
    montecarlo_50<-lapply(temp, function(x){
    x %>% dplyr::mutate_at(.vars = c(1:(ncol(temp[[1]])-1)), funs(ifelse(. >  159299502, 159299502, .))) %>% 
      # dplyr::mutate(Date=seq.Date(as.Date("2021-01-01"), as.Date("2050-12-01"), by="1 month")) %>%
      gather(., Variable, Value, c(1:(ncol(temp[[1]])-1)))
  })
  
  plot_MC50<-lapply(montecarlo_50, function(x){
    x %>% ggplot()+
      geom_point(aes(x=Date, y=Value, color=Variable))+
      geom_line(aes(x=Date, y=Value, color=Variable))+
      geom_hline(yintercept=c(Q_vol10, Q_vol90), lty=2)+
      theme(legend.position = "none")+
      labs(x="\nYears", y="Mm3\n", 
           title="MonteCarlo analysis\n")
  })

#### * 2041-2070 #### 
#(both rcp4.5 and 8.5)
lista_dam70<-list(dam2, dam4)
sample_CI<-list()
temp2<-list()
Date<-seq.Date(as.Date("2041-01-01"), as.Date("2070-12-01"), by="1 month")

for (j in 1:length(lista_dam70)){
  l1<-foreach (i = 1:nrow(dam1), .inorder = T) %dopar% {
    
    lala<-seq(as.numeric(lista_dam70[[j]][i,c("Lower_values")]), as.numeric(lista_dam70[[j]][i,c("Upper_values")]), 1000000)
    
    sample_CI[[i]]<-sample(lala, size=sizze, replace=T)
  }
  temp2[[j]]<-cbind(as.data.frame(do.call(rbind, l1)), Date)
}

# plotting many replicates all together qith quantiles
montecarlo_70<-lapply(temp2, function(x){
  x %>% dplyr::mutate_at(.vars = c(1:(ncol(temp2[[1]])-1)), funs(ifelse(. >  159299502, 159299502, .))) %>% 
    # dplyr::mutate(Date=seq.Date(as.Date("2041-01-01"), as.Date("2070-12-01"), by="1 month")) %>%
    gather(., Variable, Value, c(1:(ncol(temp2[[1]])-1)))
}) 

plot_MC70<-lapply(montecarlo_70, function(x){
  x %>% ggplot()+
    geom_point(aes(x=Date, y=Value, color=Variable))+
    geom_line(aes(x=Date, y=Value, color=Variable))+
    geom_hline(yintercept=c(Q_vol10, Q_vol90), lty=2)+
    theme(legend.position = "none")+
    labs(x="\nYears", y="Mm3\n", 
         title="MonteCarlo analysis\n")
})

#### Baseline ####
# applying montecarlo approach to the baseline too
sample_baseline<-list()
date_base<-san_baseline_vol$Date

l_base<-foreach (i = 1:nrow(san_baseline_vol), 
                 .inorder = T, 
                 .packages=c("dplyr", "tidyr", "ggplot2", "caret", "lubridate")) %dopar% {
                   lala_base<-seq(san_baseline_vol$Lower_values[i], san_baseline_vol$Upper_values[i], 1000000)
                   
                   # sampling from upper and lower values of CI
                   sample_baseline[[i]]<- sample(lala_base, size=sizze, replace=T)
                 }
tm<-cbind(as.data.frame(do.call(rbind, l_base)), date_base)

montecarlo_baseline <- tm %>% 
  dplyr::mutate_at(.vars = c(1:(ncol(tm)-1)), funs(ifelse(. >  159299502, 159299502, .))) %>% 
  gather(., Variable, Value, c(1:(ncol(tm)-1)))


plot_MCbase<-montecarlo_baseline %>% 
  rename(.,  Date=date_base) %>% 
  ggplot()+
  geom_point(aes(x=Date, y=Value, color=Variable))+
  geom_line(aes(x=Date, y=Value, color=Variable))+
  theme(legend.position = "none")+
  labs(x="\nYears", y="Mm3\n", 
       title="MonteCarlo analysis for the baseline\n")


#### Number of events ####
lista_n <- list()
lista_n2 <- list()

lista_n80<-list()
lista_n80_2<-list()

V_unique<-as.data.frame(unique(montecarlo_50[[1]][2]))
temp_list<-list()
temp_list2<-list()
list_base<-list()

for (j in 1:length(montecarlo_50)){
  lista_j30<-list()
  lista_j30_2<-list()
  
  lista_j80<-list()
  lista_j80_2<-list()
  list_base2_30<-list()
  list_base2_80<-list()
  
  for (k in V_unique$Variable){
    posizione <- which(unique(montecarlo_50[[j]][2]) == k)

    temp_list[[posizione]]<-montecarlo_50[[j]][(which(montecarlo_50[[j]][2] == k)),]
    temp_list2[[posizione]]<-montecarlo_70[[j]][(which(montecarlo_70[[j]][2] == k)),]
    
    list_base[[posizione]]<-montecarlo_baseline[(which(montecarlo_baseline[2] == k)),]
    
    list_base2_30[[posizione]]<-length(which(list_base[[posizione]][3]<=Q_vol30))
    list_base2_80[[posizione]]<-length(which(list_base[[posizione]][,3]>=Q_vol80))
    
    for (i in 1:192) {
      # I consider series of 166 values so i can compare them with the baseline period
      interm<-createTimeSlices(1:nrow(temp_list[[posizione]]), 
                               initialWindow = 168, fixedWindow = TRUE)
      
      slice_dt<-temp_list[[posizione]][interm$train[[i]],3]
      
      interm2<-createTimeSlices(1:nrow(temp_list2[[posizione]]), 
                                initialWindow = 168, fixedWindow = TRUE)
      
      slice_dt2<-temp_list2[[posizione]][interm2$train[[i]],3]
      
      # counting values lower than the 30th quantile
      lista_j30[[i]]<-length(which(slice_dt<=(Q_vol30)))
      lista_j30_2[[i]]<-length(which(slice_dt2<=(Q_vol30)))
      
      
      # and greater than the 80th quantile
      lista_j80[[i]]<-length(which(slice_dt>=(Q_vol80)))
      lista_j80_2[[i]]<-length(which(slice_dt2>=(Q_vol80)))
    }
    
    lista_n[[j]]<-lista_j30
    lista_n2[[j]]<-lista_j30_2
    
    lista_n80[[j]]<-lista_j80
    lista_n80_2[[j]]<-lista_j80_2
  }
}

lst_q30[[s]]<-data.frame(Baseline=median(unlist(list_base2_30)),
                         "RCP4.5 2021-2050"=unlist(lista_n[[1]]),
                         "RCP8.5 2021-2050"=unlist(lista_n[[2]]),
                         "RCP4.5 2041-2070"=unlist(lista_n2[[1]]),
                         "RCP8.5 2041-2070"=unlist(lista_n2[[2]]), check.names = FALSE) %>% 
  as_tibble()

lst_q80[[s]]<-data.frame(Baseline=median(unlist(list_base2_80)),
                         "RCP4.5 2021-2050"=unlist(lista_n80[[1]]),
                         "RCP8.5 2021-2050"=unlist(lista_n80[[2]]),
                         "RCP4.5 2041-2070"=unlist(lista_n80_2[[1]]),
                         "RCP8.5 2041-2070"=unlist(lista_n80_2[[2]]), check.names = FALSE)%>% 
  as_tibble()

print(Sys.time()-strt)
print(s)
}

# Sys.time()

saveRDS(lst_q30, file="~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/MC_vol_events30_10k.rds")
saveRDS(lst_q80, file="~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/MC_vol_events80_10k.rds")

lst_q30<-readRDS(file="~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/MC_vol_events30_10k.rds")
lst_q80<-readRDS(file="~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/MC_vol_events80_10k.rds")

# setwd("C:/Users/STerzi/Documents/R/Noce/Hydro_analysis/S.Giustina/Vulnerability/")
lst_q30<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/MC_vol_events30_10k.rds") %>%
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  rename("RCP4.5 2021-2050"="RCP4.5_2021_2050",
         "RCP4.5 2041-2070"="RCP4.5_2041_2070",
         "RCP8.5 2021-2050"="RCP8.5_2021_2050",
         "RCP8.5 2041-2070"="RCP8.5_2041_2070")
  
lst_q80<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/MC_vol_events80_10k.rds") %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  rename("RCP4.5 2021-2050"="RCP4.5_2021_2050",
         "RCP4.5 2041-2070"="RCP4.5_2041_2070",
         "RCP8.5 2021-2050"="RCP8.5_2021_2050",
         "RCP8.5 2041-2070"="RCP8.5_2041_2070")


#### Boxplots ####
# case q10
plot_lst_q30<-lst_q30 %>% 
  mutate(ID=row.names(.)) %>% 
  gather(Variable, Value, c(1:5)) %>% 
  ggplot(., aes(x=Variable, y=Value, group=ID, color=Variable))+
  geom_point()+
  geom_line()

# dataframe for next boxplot
boxplot_lst_q30<-lst_q30 %>% 
  mutate(ID=row.names(.),
         Quant="Events < 30th quantile") %>% 
  gather(Variable, Value, c(1:5)) 

# creating boxplot for each scenario
box_30<-ggplot(boxplot_lst_q30, aes(x=Variable, y=Value, group=Variable, fill=Variable, stat="identity"))+
  geom_boxplot()+
  theme(legend.position = "none")+
  labs(x="Scenarios",
       y="Events #",
       title="Number of events below 30th quantile")

ggplotly(box_30)

# case q80
plot_lst_q80<-lst_q80 %>% 
  mutate(ID=row.names(.)) %>% 
  gather(Variable, Value, c(1:5)) %>% 
  ggplot(., aes(x=Variable, y=Value, group=ID, color=Variable))+
  geom_point()+
  geom_line()

boxplot_lst_q80<-lst_q80 %>% 
  mutate(ID=row.names(.),
         Quant="Events > 80th quantile") %>% 
  gather(Variable, Value, c(1:5)) 


box_80<-ggplot(boxplot_lst_q80, aes(x=Variable, y=Value, group=Variable, fill=Variable, stat="identity"))+
  geom_boxplot()+
  theme(legend.position = "none")+
  labs(x="Scenarios",
       y="Events #",
       title="Number of events above 80th quantile")

ggplotly(box_80)

boxplottone<-rbind(boxplot_lst_q30, boxplot_lst_q80) %>% 
  filter(Variable!="Baseline")

Baseline<-rbind(boxplot_lst_q30, boxplot_lst_q80) %>% 
  filter(Variable=="Baseline")

Base30<-Baseline %>% 
  filter(Quant=="Events < 30th quantile" & ID==1) %>% 
  pull(Value)

Base80<-Baseline %>% 
  filter(Quant=="Events > 80th quantile" & ID==1) %>% 
  pull(Value)
  
# setting colours for volume
col_vol<-c("#a6dba0", "#5aae61", "#c2a5cf", "#9970ab")

p_boxplottone<-ggplot(boxplottone, aes(x=Variable, y=Value, group=Variable, fill=Variable, stat="identity"))+
  geom_boxplot()+
  facet_wrap(~ Quant, scales="free_x")+
  labs(x="Scenarios",
       y="Events", 
       title="Comparison of past and future number of events with volume lower than the 30th or greater than the 80th quantile")+
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
  geom_hline(data = data.frame(xint=1, Quant = "Events < 30th quantile"), aes(yintercept = Base30), linetype = 3, size=1)+
  geom_hline(data = data.frame(xint=1, Quant = "Events > 80th quantile"), aes(yintercept = Base80), linetype = 3, size=1)+
  scale_fill_manual(values=c(col_vol))


ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/S.Giustina_vol_events_MonteCarlo.png", plot=p_boxplottone,
       width = 16,
       height = 8, 
       dpi=600)
