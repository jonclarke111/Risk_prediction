
rm(list=ls())

### Required packages - install if missing and load (all) ###

required_packages <- c("tidyverse",
                       "stringr",
                       "survival",
                       "fastDummies",
                       "Hmisc",
                       "dplyr")

for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

### External data sources ####

path_0<-"K:/ckb_data/Staff_Folders/jonathan_clarke/UKB_data_meta/Data meta/"
path_1<-"K:/ckb_data/Staff_Folders/jonathan_clarke/UKB_data_meta/Data dictionaries/"
path_2<-"K:/ckb_data/Staff_Folders/jonathan_clarke/UKB_data_meta/Data coding files/"
path_3<-"K:/ckb_data/Staff_Folders/jonathan_clarke/UKB_data_meta/Text files/"
path_4<-"K:/ckb_data/Staff_Folders/jonathan_clarke/UKB_data_meta/Participant withdrawals/"
path_9<-"K:/kadoorie/Staff_Folders/JonathanC/Projects/Risk prediction MC/Results/"
path_11<-"K:/kadoorie/Staff_Folders/JonathanC/Projects/Risk prediction MC/Tables_figures/"

### Internal data sources ####

path_5  <- "Data/Raw_data_ukb/"
path_6  <- "Data/Raw_data_ckb/"
path_7  <- "Data/Project_data_all/"
path_8  <- "Results/"
path_10 <- "Tables_figures/"

### Import study dataset ####

data_all <- readRDS(paste0(path_7,"ukb_study_data_ec_analysis_wd_20_05_2024_selected.rds"))                      # Study dataset
tab      <- read.csv(paste0(path_7,"EC_risk_factors.csv"))                                                       # Study risk factors and weights

### Linear predictor function ####
   
fun<-function(study_id){
      
      data<-data_all
  
      outcome_prev <- paste0(outcome,"_prev")             # Initialize prevalent outcome
      outcome_inc  <- paste0(outcome,"_inc")              # Initialize incident outcome
  
      ### Create dummy variables for all (binary and) categorical risk factors ####
      
      risk_factor<-tab$varnames[tab$variable_type %in% c("binary","categorical") & tab$ukb_variable=="Yes"]   # Select risk factors (must be coded 0, 1, 2, NA etc.)
      for(ii in risk_factor){data[[ii]]<-as.factor(data[[ii]])}                                               # Convert risk factors values to character format
      data<-dummy_cols(data,select_columns=risk_factor,ignore_na=TRUE)                                        # Create dummy variables
      
      
      data<-data[which(data$sex==0 & data[[outcome_prev]]==0), ]  # Generic exclusions  
        
      ### Study specific exclusions ####     
     
      if (study_id=="pfr_13")                              {data<-data[which(data$bilateral_oophorectomy_op==0 & data$hysterectomy_prev==0),]} 
      else if (study_id=="hcc_15")                         {data<-data} 
      else if (study_id=="hus_16")                         {data<-data[which(data$all_cancer_prev==0 & data$hysterectomy_prev==0),]} 
      else if (study_id=="shi_23")                         {data<-data[which(data$menopause_cat2==1 & data$age >=45 & data$ethnicity_cat3==0 & data$hysterectomy_prev==0),]} 
      else if (study_id=="bea_23")                         {data<-data[which(!is.na(data$bmi)),]}   
      
      risk_factor_all<- tab$varnames[tab$risk_factor=="Yes"]  
       
      ### Create risk factor * log(Weight) columns ####
        
      for(ii in risk_factor_all){
      data[[ii]][!is.na(data[[ii]])]<-data[[ii]][!is.na(data[[ii]])] * log(as.numeric(tab[tab$varnames==ii & tab$risk_factor=="Yes",paste0(study_id)]))
      }
    
      ### Select model specific variables (must be correctly labelled in meta data) ####
        
      risk_factors_cat  <- tab$varnames[tab[[study_id]]=="Yes" & tab$ukb_variable=="Yes"]            
      risk_factors_cat  <- str_c("^",risk_factors_cat,"_")
      risk_factors_cont <- tab$varnames[tab[[study_id]]=="Yes" & tab$ukb_variable=="Yes" & tab$variable_type=="continuous"]
      risk_factors      <- c(risk_factors_cat,risk_factors_cont)
      risk_factors      <- grep(str_c(risk_factors,collapse="|"),names(data),value=TRUE)
   
      data <- data %>% select(all_of(c("eid.",outcome_inc,risk_factors))) 
      data <- data %>% rowwise() %>% mutate(log_odds_pred = sum(c_across(all_of(risk_factors)),na.rm=TRUE))   # Create log odds predictor
      data <- data %>% drop_na()  
      
      model <-glm(reformulate(risk_factors,response=outcome_inc),data=data, family=binomial())                # Logistic model
      
      data$predicted_risk <- predict(model,type='response')
      c_index           <- rcorr.cens(data$predicted_risk, data[[outcome_inc]], outx=FALSE)                   # C-index statistics 
      
      results<-rbind(est          <- c_index[1],                                                              # C-index estimate
                     stderr       <- c_index[3],                                                              # C-index stderr
                     cases        <- as.data.frame(table(data[[outcome_inc]]))$Freq[2],                       # Number of cases
                     non_cases    <- as.data.frame(table(data[[outcome_inc]]))$Freq[1],                       # Number of controls
                     brsc         <- mean((data$predicted_risk-data[[outcome_inc]])^2),                       # Briers score
                     study        <- study_id,                                                                # Study
                     risk_factors <- str_c(risk_factors,collapse=","),                                        # Risk factors
                     outcome      <- outcome)                            
      
      print(results)
      return(results)

    }

### Select studies ####

study_id<-names(tab)[c(7:9,11)]

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





