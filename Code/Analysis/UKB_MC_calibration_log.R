### Load libraries ####

library(tidyverse)
library(survival)
library(Hmisc)

### SET PATHS ####

path_1<-"K:/CKFB UKB metab/UKB_oct_22/Data dictionaries/"
path_2<-"K:/CKFB UKB metab/UKB_oct_22/Data coding files/"
path_3<-"K:/CKFB UKB metab/UKB_oct_22/Participant withdrawals/"
path_4<-"K:/CKFB UKB metab/UKB_oct_22/Data apr24/"
path_5<-"K:/ckb_data/Staff_Folders/jonathan_clarke/Cancer_risk_prediction/Project_data/EC/"
path_6<-"K:/kadoorie/Staff_Folders/JonathanC/Projects/Cancer_risk_prediction/Data/Study_data/EC/"
path_7<-"K:/kadoorie/Staff_Folders/JonathanC/Projects/Cancer_risk_prediction/Tables_Figures/EC/Tables/"

### Import study dataset ####

data<-readRDS(paste0(path_5,"ukb_study_data_ec_analysis_wd_2_05_2024_selected.rds")) 

### Import meta data (risk factor data) ####

tab<-read.csv(paste0(path_6,"EC_risk_factors.csv"))
tab<-tab[tab$varnames %in% grep(str_c(tab$varnames[tab$ukb_variable=="Yes"],collapse="|"),tab$varnames,value=TRUE),]

### Analysis function ####
   
    fun<-function(study_id){
      
      ### Generic exclusions ####     
      
      ds<-data[which(data$sex==0 & data[[paste0(outcome,"_prev")]]==0), ]
        
      ### Study specific exclusions ####     
      
      if (study_id=="pfr_13")                              {ds<-ds[which(ds$bilateral_oophorectomy_sr==0 & ds$hysterectomy_prev==0),]} 
      else if (study_id=="hcc_15")                         {ds<-ds} 
      else if (study_id=="hus_16")                         {ds<-ds[which(ds$all_cancer_prev==0 & ds$hysterectomy_prev==0),]} 
      else if (study_id=="shi_23")                         {ds<-ds[which(ds$menopause_cat2==1 & ds$age >=45 & ds$ethnicity_cat3==0 & ds$hysterectomy_prev==0),]} 
      else if (study_id=="bea_23")                         {ds<-ds[which(!is.na(ds$bmi)),]}   
      
      ### Selects model risk factors ####   
      
      adj_for<-tab$varnames[tab[[paste0(study_id)]]=="Yes" & tab$ukb_variable=="Yes"]
      adjust_for<-paste(adj_for,collapse = " + ")
      
      ds<-ds %>% select(all_of(c("eid.",paste0(outcome,"_inc"),adj_for))) 
      ds <- ds[complete.cases(ds),]    
      
      ### Logistic model ####
      
      model<-glm(as.formula(paste0(outcome,"_inc ~",adjust_for)),data=ds,family=binomial())
      
      ds$predicted_risk <- predict(model,type='response')
     
      ds$predicted_risk<-ds$predicted_risk + runif(length(ds$predicted_risk), -0.0001, 0.0001)  # Jitter function (required for models with only discrete risk factors for 10 groups) 
      
      ### Group observed and predicted probabilities ####
      
      group_cut <- quantile(ds$predicted_risk, c(seq(0, 1, 0.1)))
      group <- cut(ds$predicted_risk, group_cut) 
      predicted_risk <- tapply(ds$predicted_risk, group, mean)
      observed_risk <- tapply(model$y, group, mean)
      
      results <- data.frame(cbind(predicted_risk, observed_risk, study_id))

      print(results)
    
    }

### Select studies ####

study_id<-names(tab)[c(8:18)]

# Run analysis function with missing as NA

outcome<-"endometrial_cancer"                                                          # Main outcome definition
fig_data<-lapply(study_id,fun)
fig_data<-as.data.frame(do.call(rbind, fig_data))      
write.csv(fig_data,paste0(path_7,"fig_data_log_cal_main_na_obstt.csv"))                # Not required

outcome<-"endometrial_cancer_x"                                                        # Broad outcome definition
fig_data<-lapply(study_id,fun)
fig_data<-as.data.frame(do.call(rbind, fig_data))     
write.csv(fig_data,paste0(path_7,"fig_data_log_cal_broad_na_obs.csv"))                 # Not required  

### Replace missing with 0 for all variables (except date variables) ####

is_date <- sapply(data, function(col) inherits(col, "Date"))
non_date_variables <- names(data)[is_date==FALSE]
for(ii in non_date_variables) {data[[ii]][is.na(data[[ii]])] <- 0}

# Run analysis function with missing as 0

outcome<-"endometrial_cancer"                                                          # Main outcome definition
fig_data<-lapply(study_id,fun)
fig_data<-as.data.frame(do.call(rbind, fig_data))      
write.csv(fig_data,paste0(path_7,"fig_data_log_cal_main_0_obs.csv"))                   # Not required

outcome<-"endometrial_cancer_x"                                                        # Broad outcome definition
fig_data<-lapply(study_id,fun)
fig_data<-as.data.frame(do.call(rbind, fig_data))      
write.csv(fig_data,paste0(path_7,"fig_data_log_cal_broad_0_obs.csv"))                  # Not required 

#############################################################################################################################################
