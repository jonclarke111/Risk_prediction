### Load libraries ####

library(tidyverse)
library(survival)
library(fastDummies)
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

data<-readRDS(paste0(path_5,"ukb_study_data_ec_analysis_wd_20_05_2024_selected.rds")) 

### Import meta data (risk factor data) ####

tab<-read.csv(paste0(path_6,"EC_risk_factors.csv"))

### Linear predictor function ####
   
    fun<-function(study_id){
      
      ### Create dummy variables for all (binary and) categorical risk factors ####
      
      risk_factor<-tab$varnames[tab$variable_type %in% c("binary","categorical") & tab$ukb_variable=="Yes"]   # Select risk factors (must be coded 0, 1, 2, NA etc.)
      for(ii in risk_factor){data[[ii]]<-as.factor(data[[ii]])}                                               # Convert risk factors values to character format
      ds<-dummy_cols(data,select_columns=risk_factor,ignore_na=TRUE)                                          # Create dummy variables
      
      ### Generic exclusions ####     
      
      ds<-ds[which(ds$sex==0 & ds[[paste0(outcome,"_prev")]]==0), ]
        
      ### Study specific exclusions ####     
     
      if (study_id=="pfr_13")                              {ds<-ds[which(ds$bilateral_oophorectomy_sr==0 & ds$hysterectomy_prev==0),]} 
      else if (study_id=="hcc_15")                         {ds<-ds} 
      else if (study_id=="hus_16")                         {ds<-ds[which(ds$all_cancer_prev==0 & ds$hysterectomy_prev==0),]} 
      else if (study_id=="shi_23")                         {ds<-ds[which(ds$menopause_cat2==1 & ds$age >=45 & ds$ethnicity_cat3==0 & ds$hysterectomy_prev==0),]} 
      else if (study_id=="bea_23")                         {ds<-ds[which(!is.na(ds$bmi)),]}   
      
      risk_factor_all<- tab$varnames[tab$risk_factor=="Yes"]  
       
      ### Create risk factor * log(Weight) columns ####
        
      for(ii in risk_factor_all){
      ds[[ii]][!is.na(ds[[ii]])]<-ds[[ii]][!is.na(ds[[ii]])] * log(as.numeric(tab[tab$varnames==ii & tab$risk_factor=="Yes",paste0(study_id)]))
      }
    
      ### Create log odds predictor ####
        
      adj_for_cat<-tab$varnames[tab[[study_id]]=="Yes" & tab$ukb_variable=="Yes"]
      adj_for_cat<-str_c("^",adj_for_cat,"_")
      adj_for_cont<-tab$varnames[tab[[study_id]]=="Yes" & tab$ukb_variable=="Yes" & tab$variable_type=="continuous"]
      adj_for<-c(adj_for_cat,adj_for_cont)
      adj_for<-grep(str_c(adj_for,collapse="|"),names(ds),value=TRUE)
      adjust_for<-paste(adj_for,collapse = " + ")
      
      ds<-ds %>% select(all_of(c("eid.",paste0(outcome,"_inc"),adj_for))) 
      ds<-ds %>% rowwise() %>% mutate(log_odds_pred = sum(c_across(all_of(adj_for)),na.rm=TRUE))
      
      ds <- ds[complete.cases(ds),]    
     
      ### Logistic model #### 
      
      model<-glm(as.formula(paste0(outcome,"_inc ~",adjust_for)),data=ds,family=binomial())
      
      ds$predicted_risk <- predict(model,type='response')
      
      ### C-index calculation ####  
      
      data<-rcorr.cens(ds$predicted_risk, ds[[paste0(outcome,"_inc")]], outx=FALSE)
      
      est<-data[1]
      stderr<-data[3]
      
      ### Briers score ####
      
      brsc <- mean((ds$predicted_risk-ds[[paste0(outcome,"_inc")]])^2)
      
      ### Number of cases/controls ####
       
      cc<-as.data.frame(table(ds[[paste0(outcome,"_inc")]]))
     
      cases<-cc$Freq[2]
      non_cases<-cc$Freq[1]
      
      results<-rbind(est,
                     stderr,
                     cases,
                     non_cases,
                     brsc,
                     study_id,
                     str_c(adj_for,collapse=" + "))
      
      print(results)
    }

### Select studies ####

study_id<-names(tab)[c(7:11)]

# Run analysis function with missing as NA

outcome<-"endometrial_cancer"                                                          # Main outcome definition
fig_data<-lapply(study_id,fun)
fig_data<-as.data.frame(t(do.call(cbind, fig_data)))     
write.csv(fig_data,paste0(path_7,"fig_data_log_dis_lp_main_na_obs.csv"))               # Not required

outcome<-"endometrial_cancer_x"                                                        # Broad outcome definition
fig_data<-lapply(study_id,fun)
fig_data<-as.data.frame(t(do.call(cbind, fig_data)))     
write.csv(fig_data,paste0(path_7,"fig_data_log_dis_lp_broad_na_obs.csv"))              # Not required

### Replace missing with 0 for all variables (except date variables) ####

is_date <- sapply(data, function(col) inherits(col, "Date"))
non_date_variables <- names(data)[is_date==FALSE]
for(ii in non_date_variables) {data[[ii]][is.na(data[[ii]])] <- 0}

# Run analysis function with missing as 0

outcome<-"endometrial_cancer"                                                          # Main outcome definition
fig_data<-lapply(study_id,fun)
fig_data<-as.data.frame(t(do.call(cbind, fig_data)))     
write.csv(fig_data,paste0(path_7,"fig_data_log_dis_lp_main_0_obs.csv"))                # eFigure 8

outcome<-"endometrial_cancer_x"                                                        # Broad outcome definition
fig_data<-lapply(study_id,fun)
fig_data<-as.data.frame(t(do.call(cbind, fig_data)))     
write.csv(fig_data,paste0(path_7,"fig_data_log_dis_lp_broad_0_obs.csv"))               # Not required

################################################################################################





