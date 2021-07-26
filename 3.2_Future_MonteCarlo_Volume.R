

#Libraries  --------------------------------------------------------------------
library("lubridate")
library("tidyr")
library("ggplot2")
library("dplyr")
library("gridExtra")
library("caret")
library("doParallel")
library("foreach")
library("data.table")
library("purrr")
library("plotly")
library("ggpubr")


#### Uploading datasets ####
# baseline dataset
san_baseline<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Baseline/Baseline_sgiustina_mo.rds") %>% 
  rename(Upper_values=Vol_Upr_mo,
         Lower_values=Vol_Lwr_mo)

san_baseline_indices<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Indices/Indices_volume_values_baseline.rds")


# Dataset with future volume
dam1<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Future_scenarios/Outflow&Volume_RCP45_2021_2050_month(no_maxminconditions).rds") %>% 
  rename(Upper_values=Vol_Upr_mo,
         Lower_values=Vol_Lwr_mo,
         RCP45_2021_2050=Volume_lagged_mo) %>% 
  gather(Variable, Value, c("RCP45_2021_2050")) %>% 
  dplyr::select(Date, yr, Upper_values, Lower_values, Variable, Value, id, Scenario, Time_period)

dam2<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Future_scenarios/Outflow&Volume_RCP45_2041_2070_month(no_maxminconditions).rds")%>% 
  rename(Upper_values=Vol_Upr_mo,
         Lower_values=Vol_Lwr_mo,
         RCP45_2041_2070=Volume_lagged_mo) %>% 
  gather(Variable, Value, c("RCP45_2041_2070")) %>% 
  dplyr::select(Date, yr, Upper_values, Lower_values, Variable, Value, id, Scenario, Time_period)

dam3<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Future_scenarios/Outflow&Volume_RCP85_2021_2050_month(no_maxminconditions).rds")%>% 
  rename(Upper_values=Vol_Upr_mo,
         Lower_values=Vol_Lwr_mo,
         RCP85_2021_2050=Volume_lagged_mo) %>% 
  gather(Variable, Value, c("RCP85_2021_2050")) %>% 
  dplyr::select(Date, yr, Upper_values, Lower_values, Variable, Value, id, Scenario, Time_period)

dam4<-readRDS("~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/New_scripts/01_Datasets/Future_scenarios/Outflow&Volume_RCP85_2041_2070_month(no_maxminconditions).rds")%>% 
  rename(Upper_values=Vol_Upr_mo,
         Lower_values=Vol_Lwr_mo,
         RCP85_2041_2070=Volume_lagged_mo) %>% 
  gather(Variable, Value, c("RCP85_2041_2070")) %>% 
  dplyr::select(Date, yr, Upper_values, Lower_values, Variable, Value, id, Scenario, Time_period)


# quantiles from baseline
# quantiles for past volume values
Q_vol10<-quantile(san_baseline$Volume_mo, 0.10, na.rm=T)
Q_vol20<-quantile(san_baseline$Volume_mo, 0.20, na.rm=T)

Q_vol80<-quantile(san_baseline$Volume_mo, 0.80, na.rm=T)
Q_vol90<-quantile(san_baseline$Volume_mo, 0.90, na.rm=T)


#### MonteCarlo sampling ####
strt<-Sys.time()
sizze=192*5

iterac<-1


# function for low percentiles
format_dt_q10_q20<-function(x, y, z, h, p){
  x %>%
    mutate(Number_of_months=length(which(x<=y)),
           Maxduration=with(rle(unlist(x)<= y), max(lengths[values], na.rm=TRUE)),
           Run=with(rle(unlist(x)<= y), rep(lengths, lengths), na.rm=TRUE),
           Cond=with(rle(unlist(x)<= y), rep(values, lengths), na.rm=TRUE),
           Event_id=cumsum(!duplicated(Run, Cond)),
           TotVol=sum(Value)) %>%
    filter(Cond == TRUE) %>%
    ungroup() %>%
    mutate(Number_of_events=length(unique(Event_id))) %>% 
    group_by(Event_id, Number_of_events) %>%
    mutate(Severity=sum(abs(y-Value))) %>%
    ungroup() %>% 
    distinct(Event_id, .keep_all = T) %>% 
    summarise(Threshold=z,
              Time_period=h,
              Scenario=p,
              Maxduration=mean(Maxduration),
              Number_of_months=mean(Number_of_months),
              Number_of_events=mean(Number_of_events),
              Mean_severity=mean(Severity),
              Sum_severity=sum(Severity),
              TotVol2=unique(TotVol),
              Janez_severity=Sum_severity/TotVol2) %>%
    ungroup() %>%
    distinct(Mean_severity, .keep_all = T)    
}

# function for high percentiles
format_dt_q80_q90<-function(x, y, z, h, p){
  tt<-x %>%
    mutate(Number_of_months=length(which(x>=y)),
           Maxduration=with(rle(unlist(x)>= y), max(lengths[values], na.rm=TRUE)),
           Run=with(rle(unlist(x)>= y), rep(lengths, lengths), na.rm=TRUE),
           Cond=with(rle(unlist(x)>= y), rep(values, lengths), na.rm=TRUE),
           Event_id=cumsum(!duplicated(Run, Cond)),
           TotVol=sum(Value)) %>% 
    dplyr::filter(Cond==TRUE) %>% 
    
    ungroup() %>%
    mutate(Number_of_events=length(unique(Event_id))) %>% 
    group_by(Event_id, Number_of_events) %>%
    mutate(Severity=sum(abs(y-Value))) %>%
    ungroup() %>% 
    distinct(Event_id, .keep_all = T) %>% 
    summarise(Threshold=z,
              Time_period=h,
              Scenario=p,
              Maxduration=mean(Maxduration),
              Number_of_months=mean(Number_of_months),
              Number_of_events=mean(Number_of_events),
              Mean_severity=mean(Severity),
              Sum_severity=sum(Severity),
              TotVol2=unique(TotVol),
              Janez_severity=Sum_severity/TotVol2) %>%
    ungroup() %>%
    distinct(Mean_severity, .keep_all = T)
}

low_strings<-c("q_10", "q_20")
high_string<-c("q_80", "q_90")

low_percentiles<-c(Q_vol10, Q_vol20)
high_percentiles<-c(Q_vol80, Q_vol90)

Time_period_basel="Historical"
Scenario_basel="Baseline"

Time_period_50<-"2021-2050"
Time_period_70<-"2041-2070"
Scenario<-c("RCP4.5", "RCP8.5")


N_cores<-detectCores()
registerDoParallel(cores = (N_cores-1))

a<-Sys.time()

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
  
  #creo una linea nel plot per ogni serie di campionamenti
  montecarlo_50<-lapply(temp, function(x){
    x %>% gather(., Variable, Value, c(1:(ncol(temp[[1]])-1)))
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
  # plotting many replicates all together with quantiles
  
  #creo una linea nel plot per ogni serie di campionamenti
  montecarlo_70<-lapply(temp2, function(x){
    x %>% gather(., Variable, Value, c(1:(ncol(temp2[[1]])-1)))
  }) 
  
  
  #### Baseline ####
  # applying montecarlo approach to the baseline too
  sample_baseline<-list()
  date_base<-san_baseline$Date
  
  l_base<-foreach (i = 1:nrow(san_baseline), 
                   .inorder = T, 
                   .packages=c("dplyr", "tidyr", "ggplot2", "caret", "lubridate")) %dopar% {
                     
                     lala_base<-seq(san_baseline$Lower_values[i], san_baseline$Upper_values[i], 1000000)
                     
                     # sampling from upper and lower values of CI
                     sample_baseline[[i]]<- sample(lala_base, size=sizze, replace=T)
                   }
  tm<-cbind(as.data.frame(do.call(rbind, l_base)), date_base)
  
  montecarlo_baseline <- tm %>% 
    gather(., Variable, Value, c(1:(ncol(tm)-1)))
  
  
  #### Indices ####
  
  V_unique<-as.data.frame(unique(montecarlo_50[[1]][2]))
  temp_list<-list()
  temp_list2<-list()
  list_base<-list()
  
  lista_results10<-list()
  
  for (j in 1:length(montecarlo_50)){
    
    lista_n10 <- list()
    
    list_base_all<-list()
    
    for (k in V_unique$Variable){
      
      posizione <- which(unique(montecarlo_50[[j]][2]) == k)
      
      temp_list[[posizione]]<-montecarlo_50[[j]][(which(montecarlo_50[[j]][2] == k)),]
      temp_list[[posizione]][which(temp_list[[posizione]][,3]<0),3] <- 0
      
      temp_list2[[posizione]]<-montecarlo_70[[j]][(which(montecarlo_70[[j]][2] == k)),]
      temp_list2[[posizione]][which(temp_list2[[posizione]][,3]<0),3] <- 0
      
      list_base[[posizione]]<-montecarlo_baseline[(which(montecarlo_baseline[2] == k)),]
      list_base[[posizione]][which(list_base[[posizione]][,3]<0),3] <- 0
      
      base_dt<-list(list_base[[posizione]][3], list_base[[posizione]][3])
      
      ll<-list(base_dt, low_percentiles, low_strings, Time_period_basel, Scenario_basel)
      ll2<-list(base_dt, high_percentiles, high_string, Time_period_basel, Scenario_basel)
      
      list_base_all[[posizione]]<-do.call(rbind, c(pmap(ll, format_dt_q10_q20), pmap(ll2, format_dt_q80_q90)))
      
      
      interm<-createTimeSlices(1:nrow(temp_list[[posizione]]), 
                               initialWindow = 168, fixedWindow = TRUE)
      
      interm2<-createTimeSlices(1:nrow(temp_list2[[posizione]]), 
                                initialWindow = 168, fixedWindow = TRUE)
      
      listai<-list()

      listai[[j]]<-foreach (i = 1:192,
                            .inorder = T, 
                            .packages=c("purrr", "dplyr")) %dopar% {
                              
                              # series of 168 values to commpare it with the time window from the baseline
                              
                              slice_dt<-temp_list[[posizione]][interm$train[[i]],3]
                              slice_dt2<-temp_list2[[posizione]][interm2$train[[i]],3]
                              
                              short_fut_dt<-list(data.frame(Value=slice_dt), data.frame(Value=slice_dt))
                              long_fut_dt<-list(data.frame(Value=slice_dt2), data.frame(Value=slice_dt2))
                              
                              indices_2050<-list(short_fut_dt, low_percentiles, low_strings, Time_period_50, Scenario[j])
                              indices_2050_2<-list(short_fut_dt, high_percentiles, high_string, Time_period_50, Scenario[j])
                              
                              indices_2070<-list(long_fut_dt, low_percentiles, low_strings, Time_period_70, Scenario[j])
                              indices_2070_2<-list(long_fut_dt, high_percentiles, high_string, Time_period_70, Scenario[j])
                              
                              lista_j10 <- do.call(rbind, c(pmap(indices_2050, format_dt_q10_q20),
                                                            pmap(indices_2050_2, format_dt_q80_q90), 
                                                            pmap(indices_2070, format_dt_q10_q20), 
                                                            pmap(indices_2070_2, format_dt_q80_q90)))
                            }

      # saving according to 2021-2050 or 2041-2070
      
      lista_n10[[posizione]]<-data.frame(do.call(rbind, map(listai, rbindlist))) %>% 
        as_tibble()
      
      print(paste0(Sys.time(), ",", j, ",", posizione))
    }
    
    # all 2021-2050
    lista_results10[[j]]<-do.call(rbind, lista_n10)
    
  }
  
  b<-Sys.time()
  b-a
  
  # storing final dt
  lst_q10<-data.frame(do.call(rbind, c(list_base_all, lista_results10))) %>% 
    as_tibble()
  
  
  print(Sys.time()-strt)
}

# saveRDS(lst_q10, file="~/R/Noce/HydroNoce/S.Giustina/1_Ordered_scripts/6_Future/Future_datasets/MC_vol_events10_10k_2attempt.rds")


#### Boxplots ####
# setting colors
col_vol<-c("gray50", "#a6dba0", "#5aae61", "#c2a5cf", "#9970ab")

# Number of months with volume < 10th percentile
N_months<-lst_q10 %>%
  mutate(Time_period=ifelse(Time_period=="Historical",  "1999-2004,2009-2016", Time_period),
         Scenario2=paste0(Scenario, " ", Time_period),
         Number_of_months_rel=Number_of_months) %>% 
  dplyr::select(-c("Sum_severity", "TotVol2", "Scenario", "Time_period", "Number_of_events", "Mean_severity")) %>% 
  gather(Variable, Value, c("Maxduration", "Number_of_months", "Janez_severity", "Number_of_months_rel")) %>% 
  mutate(Variable=sub(".*\\.", "", Variable),
         Value=case_when(
           Variable=="Number_of_months" ~ Value,
           Variable=="Number_of_months_rel" ~ Value,
           Variable=="Maxduration" ~ Value,
           Variable=="Janez_severity" ~ Value),
         Threshold=case_when(Threshold=="q_10" ~ "< 10th percentile",
                             Threshold=="q_20" ~ "< 20th percentile",
                             Threshold=="q_80" ~ "> 80th percentile",
                             Threshold=="q_90" ~ "> 90th percentile"),
         Variable=case_when(Variable=="Maxduration" ~ "3 Maximum duration",
                            Variable=="Number_of_months" ~ "1 Absolute frequency",
                            Variable=="Number_of_months_rel" ~ "2 Relative frequency",
                            Variable=="Janez_severity" ~ "4 Relative severity"))

N_months$Value<- ifelse(N_months$Variable=="2 Relative frequency", N_months$Value/14, N_months$Value)


# only q10 and q20

my_comparisons <- list( c("Baseline 1999-2004,2009-2016", "RCP4.5 2021-2050"), 
                        c("Baseline 1999-2004,2009-2016", "RCP4.5 2041-2070"), 
                        c("Baseline 1999-2004,2009-2016", "RCP8.5 2021-2050"),
                        c("Baseline 1999-2004,2009-2016", "RCP8.5 2041-2070"))

p_10_20<-N_months %>% 
  filter(Threshold %in% c("< 10th percentile", "< 20th percentile")) %>% 
  ggplot(., aes(x=Scenario2, y=Value, fill=Scenario2, stat="identity"))+
  facet_grid(Variable ~ Threshold, scales="free_y", switch="y", labeller = label_wrap_gen(width=18))+
  geom_boxplot()+
  stat_compare_means(method="wilcox.test", comparisons = my_comparisons, 
                     label = "p.signif", size = 8, vjust = 0.5)+  
  labs(title="Low volume")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 30, hjust=0.8, vjust=0.95, size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(size = 22),
        axis.title=element_blank(),
        legend.position = "none",
        panel.spacing = unit(0.75, "lines"),
        strip.text = element_text(size=20))+
  scale_y_continuous(position="right", expand = expansion(mult = c(.1, .1)))+
  scale_x_discrete(labels = c('Baseline\n1999-2004,2009-2016', 'RCP4.5\n2021-2050', 'RCP4.5\n2041-2070', 
                                'RCP8.5\n2021-2050', 'RCP8.5\n2041-2070'),
                   expand = c(0.1, 0.1))+
  scale_fill_manual(values=c(col_vol))


p_10_20


ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/Water_balance/S.Giustina_MonteCarlo_indices_test_lowV_960.png", plot=p_10_20,
       width = 20,
       height = 13,
       dpi=600)


# only q80 and q90
p_80_90<-N_months %>% 
  filter(Threshold %in% c("> 80th percentile", "> 90th percentile")) %>% 
  ggplot(., aes(x=Scenario2, y=Value, fill=Scenario2, stat="identity"))+
  facet_grid(Variable ~ Threshold, scales="free_y", switch="y", labeller = label_wrap_gen(width=18))+
  geom_boxplot(coef = 10)+
  stat_compare_means(method="wilcox.test", comparisons = my_comparisons, 
                     label = "p.signif", size = 8, vjust = 0.5)+    
  labs(title="High volume")+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 30, hjust=0.8, vjust=0.95, size = 18),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(size = 22),
        axis.title=element_blank(),
        legend.position = "none",
        panel.spacing = unit(1.5, "lines"),
        strip.text = element_text(size=20))+
  scale_y_continuous(position="right", expand = expansion(mult = c(.1, .1)))+
  scale_x_discrete(labels = c('Baseline\n1999-2004,2009-2016', 'RCP4.5\n2021-2050', 'RCP4.5\n2041-2070', 
                              'RCP8.5\n2021-2050', 'RCP8.5\n2041-2070'))+
  scale_fill_manual(values=c(col_vol))

p_80_90

ggsave("~/R/Noce/HydroNoce/S.Giustina/02_images/Water_balance/S.Giustina_MonteCarlo_indices_test_highV_960.png", plot=p_80_90,
       width = 20,
       height = 13,
       dpi=600)
