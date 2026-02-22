
### LIBRARIES ####

library(stringr)
library(tidyverse)

### SET PATHS ####

path_1<-"K:/CKFB UKB metab/UKB_oct_22/Data dictionaries/"
path_2<-"K:/CKFB UKB metab/UKB_oct_22/Data coding files/"
path_3<-"K:/CKFB UKB metab/UKB_oct_22/Participant withdrawals/"
path_4<-"K:/CKFB UKB metab/UKB_oct_22/Data apr24/"
path_5<-"K:/ckb_data/Staff_Folders/jonathan_clarke/Cancer_risk_prediction/Project_data/EC/"
path_6<-"K:/kadoorie/Staff_Folders/JonathanC/Projects/Cancer_risk_prediction/Data/Study_data/EC/"
path_7<-"K:/kadoorie/Staff_Folders/JonathanC/Projects/Cancer_risk_prediction/Tables_Figures/EC/Tables/"

### IMPORT MAIN DATASET ####

data_all<-readRDS(paste0(path_4,"ukb_8may24.rds"))

### Select censoring variables ####

data<-data_all %>% 
  select(grep(str_c(c("eid.", 
                      "index_of_multiple_deprivation_england.0.0",         # 26410
                      "index_of_multiple_deprivation_scotland.0.0",        # 26427
                      "index_of_multiple_deprivation_wales.0.0",           # 26426
                      "uk_biobank_assessment_centre.0.0",                  # 54
                      "reason_lost_to_follow_up.0.0",                      # 190
                      "date_lost_to_follow_up.0.0",                        # 191
                      "date_of_attending_assessment_centre.0.0",           # 53
                      "date_of_death.0.0",                                 # 40000
                      "underlying_primary_cause_of_death_icd10.0.0"),      # 40001
                    collapse="|"),names(data_all)))

data<-data %>% 
  rename(index_of_multiple_deprivation_england               = index_of_multiple_deprivation_england.0.0,
         index_of_multiple_deprivation_scotland              = index_of_multiple_deprivation_scotland.0.0,
         index_of_multiple_deprivation_wales                 = index_of_multiple_deprivation_wales.0.0,
         study_date                                          = date_of_attending_assessment_centre.0.0, 
         assessment_centre                                   = uk_biobank_assessment_centre.0.0,
         reason_lost_to_follow_up                            = reason_lost_to_follow_up.0.0,
         date_lost_to_follow_up                              = date_lost_to_follow_up.0.0,
         date_of_death                                       = date_of_death.0.0,
         cause_of_death                                      = underlying_primary_cause_of_death_icd10.0.0)

### Death endpoint ####

data$death_ep<-ifelse(is.na(data$date_of_death),0,1)

### Censoring date ####

# Date lost to follow-up (priority 1)

data$end_date<-data$date_lost_to_follow_up

# Death date (priority 2)

data$end_date[is.na(data$date_lost_to_follow_up)]<-data$date_of_death[is.na(data$date_lost_to_follow_up)]

# Study end date based on location using index of multiple deprivation (priority 3) 

# End dates set depending on data sources used (amend if using Cancer Registry only)

data$end_date[is.na(data$end_date) & !is.na(data$index_of_multiple_deprivation_wales)]   <-"2022-05-31"    # (cancer registry only 2016-12-31)
data$end_date[is.na(data$end_date) & !is.na(data$index_of_multiple_deprivation_scotland)]<-"2022-08-31"    # (cancer registry only 2021-11-31)
data$end_date[is.na(data$end_date) & !is.na(data$index_of_multiple_deprivation_england)] <-"2022-10-31"    # (cancer registry only 2020-12-31)

# Study end date based on location using uk assessment centre (priority 4) 

centre_code<-read.table(file = paste0(path_2, 'coding10.tsv'), sep = '\t', header = TRUE)
table(data$assessment_centre)
summary(data$end_date)

data$end_date[is.na(data$end_date) & data$assessment_centre %in% c(11003,11022,11023)]<-"2022-05-31"   # Wales centres    (cancer registry only 2016-12-31)
data$end_date[is.na(data$end_date) & data$assessment_centre %in% c(11004,11005)]<-"2022-08-31"         # Scotland centres (cancer registry only 2021-11-31)
data$end_date[is.na(data$end_date)]<-"2022-10-31"                                                      # England centres  (cancer registry only 2020-12-31)

# Date of death set as censoring date, if no date of death

summary(data$date_of_death)

data$date_of_death[is.na(data$date_of_death)]<-data$end_date[is.na(data$date_of_death)]

summary(data$date_of_death)

data_censoring<-data 

### HEALTH RELATED OUTCOMES - CANCER REGISTRY - selected cancer registry variables ####

# 40006 ICD-10: Diagnoses           (MULTI-CATEGORICAL)
# 40013 ICD-9:  Diagnoses           (MULTI-CATEGORICAL)
# 40005 ICD-10: Date of diagnoses   (MULTI-CATEGORICAL)
# 41270 ICD-10: Diagnoses           (MULTI-CATEGORICAL)
# 41271 ICD-9:  Diagnoses           (MULTI-CATEGORICAL)
# 41280 ICD-10: Date of diagnoses   (MULTI-CATEGORICAL)
# 41281 ICD-9:  Date of diagnoses   (MULTI-CATEGORICAL)

data_cr<-data_all[,grep("eid|type_of_cancer_icd10|type_of_cancer_icd9|date_of_cancer_diagnosis",names(data_all),value=TRUE)]

data_cr<-merge(data_censoring,data_cr,all.x=TRUE)

var<-names(data_cr)[grep("date_of_cancer_diagnosis",names(data_cr))]

for (i in var){
  
  data_cr[[i]][is.na(data_cr[[i]])]<-data_cr$end_date[is.na(data_cr[[i]])]
  
}

### HEALTH RELATED OUTCOMES - HOSPITAL INPATIENT - selected hospital in-patient variables ####

data_hes<- data_all %>%
  select(grep(str_c(c("eid.",                                          
                      "diagnoses_icd10.0",                                                           # 41270
                      "diagnoses_icd9.0",                                                            # 41271
                      "date_of_first_in_patient_diagnosis_icd10.0",                                  # 41280
                      "date_of_first_in_patient_diagnosis_icd9.0"),                                  # 41281
                    collapse="|"),names(data_all)))

data_hes<-merge(data_censoring,data_hes)

var<-names(data_hes)[grep("date_of_first_in_patient_diagnosis_icd10.0.|date_of_first_in_patient_diagnosis_icd9.0.",names(data_hes))]

for (i in var){
  
  data_hes[[i]][is.na(data_hes[[i]])]<-data_hes$end_date[is.na(data_hes[[i]])]
  
}

### Select baseline variables ####

data<- data_all %>%
  select(grep(str_c(c("eid.",
                      "sex.0.0",                                                                      # 31
                      "ethnic_background.0.0",                                                        # 21000
                      "age_when_attended_assessment_centre.0.0",                                      # 21003
                      "qualifications.0",                                                             # 6138
                      "age_when_completed_full_time_education.0",                                     # 845
                      "smoking_status.0.0",                                                           # 20116
                      "number_of_cigarettes_currently_smoked_daily_current_cigarette_smokers.0.0",    # 3456
                      "diabetes_diagnosed_by_doctor.0.0",                                             # 2443
                      "cancer_diagnosed_by_doctor.0.0",                                               # 2453
                      "age_high_blood_pressure_diagnosed.0.0",                                        # 2966
                      "age_started_oral_contraceptive_pill.0.0",                                      # 2794
                      "age_when_last_used_oral_contraceptive_pill.0.0",                               # 2804
                      "age_started_hormone_replacement_therapy_hrt.0.0",                              # 3536
                      "age_last_used_hormone_replacement_therapy_hrt.0.0",                            # 3546
                      "age_at_hysterectomy.0.0",                                                      # 2824
                      "ever_had_hysterectomy_womb_removed.0.0",                                       # 3591
                      "ever_taken_oral_contraceptive_pill.0.0",                                       # 2784
                      "age_at_menopause_last_menstrual_period.0.0",                                   # 3581
                      "age_when_periods_started_menarche.0.0",                                        # 2714
                      "ever_used_hormone_replacement_therapy_hrt.0.0",                                # 3546
                      "had_menopause.0.0",                                                            # 2724
                      "ever_had_stillbirth_spontaneous_miscarriage_or_termination.0.0",               # 2774
                      "age_at_first_live_birth.0.0",                                                  # 2754
                      "age_at_last_live_birth.0.0",                                                   # 2764
                      "number_of_live_births.0.0",                                                    # 2734
                      "number_of_stillbirths.0.0",                                                    # 3829
                      "number_of_pregnancy_terminations.0.0",                                         # 3849
                      "number_of_spontaneous_miscarriages.0.0",                                       # 3839
                      "body_mass_index_bmi.0.0",                                                      # 21001
                      "operation_code.0",                                                             # 20004 MULTI-CATEGORICAL (coding 5)
                      "cancer_code_self_reported.0",                                                  # 20001 MULTI-CATEGORICAL (coding 3)
                      "non_cancer_illness_code_self_reported.0",                                      # 20002 MULTI-CATEGORICAL (coding 6)
                      "treatment_medication_code.0"),                                                # 20003 MULTI-CATEGORICAL (coding 4)
                    collapse="|"),names(data_all)))


# Rename single (first occurrence) variables 

data<-data %>% 
  rename(sex                                                 = sex.0.0,
         ethnicity                                           = ethnic_background.0.0,
         age                                                 = age_when_attended_assessment_centre.0.0,
         smoking_status                                      = smoking_status.0.0,
         current_number_of_cigs_daily                        = number_of_cigarettes_currently_smoked_daily_current_cigarette_smokers.0.0,
         diabetes_diagnosed_by_doctor                        = diabetes_diagnosed_by_doctor.0.0,
         cancer_diagnosed_by_doctor                          = cancer_diagnosed_by_doctor.0.0,
         age_high_blood_pressure_diagnosed                   = age_high_blood_pressure_diagnosed.0.0,
         age_started_oral_contraceptive_pill                 = age_started_oral_contraceptive_pill.0.0,
         age_when_last_used_oral_contraceptive_pill          = age_when_last_used_oral_contraceptive_pill.0.0,
         ever_taken_oral_contraceptive_pill                  = ever_taken_oral_contraceptive_pill.0.0,
         age_started_hormone_replacement_therapy_hrt         = age_started_hormone_replacement_therapy_hrt.0.0,
         age_last_used_hormone_replacement_therapy_hrt       = age_last_used_hormone_replacement_therapy_hrt.0.0,
         age_at_menopause                                    = age_at_menopause_last_menstrual_period.0.0,
         age_at_hysterectomy                                 = age_at_hysterectomy.0.0,
         ever_had_hysterectomy_womb_removed                  = ever_had_hysterectomy_womb_removed.0.0,
         age_at_menarche                                     = age_when_periods_started_menarche.0.0,
         ever_used_hormone_replacement_therapy_hrt           = ever_used_hormone_replacement_therapy_hrt.0.0,
         had_menopause                                       = had_menopause.0.0,
         ever_had_stillbirth_spo_misc_or_termination         = ever_had_stillbirth_spontaneous_miscarriage_or_termination.0.0,
         number_of_spontaneous_miscarriages                  = number_of_spontaneous_miscarriages.0.0,
         age_at_first_live_birth                             = age_at_first_live_birth.0.0,
         age_at_last_live_birth                              = age_at_last_live_birth.0.0,
         number_of_pregnancy_terminations                    = number_of_pregnancy_terminations.0.0,
         parity                                              = number_of_live_births.0.0,                     
         number_of_still_births                              = number_of_stillbirths.0.0,
         number_of_pregnancy_terminations                    = number_of_pregnancy_terminations.0.0,
         number_of_spontaneous_miscarriages                  = number_of_spontaneous_miscarriages.0.0,
         bmi                                                 = body_mass_index_bmi.0.0)

rm(data_all)

### CREATE HEALTH OUTCOMEs (PREVALENT & INCIDENT) DATASET #### 

d1<-  c(disease="all_cancer",                  icd10="^C" ,                            icd9="^14|^15|^16|^17|^18|^19|^20|^21|^22|^23", cr="y", hes="y",sr="y",inc="n",prev="y")  
d2<-  c(disease="all_cancer_x_melanoma",       icd10="^C0|^C1|^C2|^C3|^C40|^C41|^C42|^C44|^C45|^C46|^C47|^C48|^C49|^C5|^C6|^C7|^C8|^C9",   
                                               icd9="^14|^15|^16|^170|^171|^173|^174|^175|^176|^177|^178|^179|^18|^19|^20|^21|^22|^23",
                                               cr="y", hes="y",sr="y",inc="n",prev="y") 
d3<-  c(disease="endometrial_cancer",          icd10="^C541" ,                         icd9="^1820",                       cr="y", hes="y",sr="y",inc="y",prev="y")       
d4<-  c(disease="corpus_uteri_cancer",         icd10="^C54" ,                          icd9="^182",                        cr="y", hes="y",sr="y",inc="y",prev="y")       
d5<-  c(disease="uterine_cancer",              icd10="^C55" ,                          icd9="^179",                        cr="y", hes="y",sr="y",inc="y",prev="y")       
d6<-  c(disease="bowel_cancer",                icd10="^C189" ,                         icd9="^153",                        cr="y", hes="y",sr="y",inc="n",prev="y")
d7<-  c(disease="breast_cancer",               icd10="^C50" ,                          icd9="^174",                        cr="y", hes="y",sr="y",inc="n",prev="y")
d8<-  c(disease="diabetes",                    icd10="^E08|^E09|^E10|^E11|^E12|^E13",  icd9="^250",                        cr="n", hes="y",sr="y",inc="n",prev="y")
d9<-  c(disease="pcos",                        icd10="^E282",                          icd9="^2564",                       cr="n", hes="y",sr="y",inc="n",prev="y")
d10<- c(disease="endometrial_hyperplasia",     icd10="^N850",                          icd9="^6213",                       cr="n", hes="y",sr="y",inc="n",prev="y")   
d11<- c(disease="schizophrenia",               icd10="^F209",                          icd9="^2959",                       cr="n", hes="y",sr="y",inc="n",prev="y")
d12<- c(disease="mania_depression",            icd10="^F309",                          icd9="^296",                        cr="n", hes="y",sr="y",inc="n",prev="y") 
d13<- c(disease="uterine_polyp",               icd10="^N840",                          icd9="^6210",                       cr="n", hes="y",sr="y",inc="n",prev="y") 
d14<- c(disease="hypertension",                icd10="^I10",                           icd9="^401",                        cr="n", hes="y",sr="y",inc="n",prev="y") 
d15<- c(disease="hysterectomy",                icd10="^Z907",                          icd9="^689",                        cr="n", hes="y",sr="y",inc="n",prev="y") 
d16<- c(disease="endometriosis"  ,             icd10="^N80" ,                          icd9="^617",                        cr="n", hes="y",sr="y",inc="n",prev="y" ) 
d17<- c(disease="pcos_2"  ,                    icd10="^N970" ,                         icd9="^6280",                       cr="n", hes="y",sr="y",inc="n",prev="y" ) 

data_disease<-as.data.frame(rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17))

### UNDERLYING CAUSE OF DEATH ####

dd<-data_disease %>% filter(cr=="y",inc=="y")

for(i in 1:length(dd$disease)){
  
  data_censoring[[paste0(dd$disease[i],"_du")]]<-0
  data_censoring[[paste0(dd$disease[i],"_du")]][data_censoring$cause_of_death %in% unique(grep(dd$icd10[i],data_censoring$cause_of_death,value=TRUE))]<-1
  print(table(data_censoring[[paste0(dd$disease[i],"_du")]]))
}  


### HEALTH RELATED OUTCOMES - CANCER REGISTRY (INCIDENT) ####

dd<-data_disease %>% filter(cr=="y",inc=="y")

for(i in 1:length(dd$disease)){
  
  data_cr[[paste0(dd$disease[i],"_inc_cr")]]<-0
  data_cr[[paste0(dd$disease[i],"_inc_date_cr")]]<-data_cr$end_date
  
  # ICD-10 diagnosis (incident)
  
  for(ii in 0:(length(grep("type_of_cancer_icd10",names(data_cr),value=TRUE))-1)){
    
    data_cr[[paste0(dd$disease[i],"_inc_cr")]][(data_cr[[paste0("type_of_cancer_icd10.",ii,".0")]] %in% unique(grep(dd$icd10[i],data_cr[[paste0("type_of_cancer_icd10.",ii,".0")]],value=TRUE))) &
                                                 data_cr[[paste0("date_of_cancer_diagnosis.",ii,".0")]] > data_cr$study_date]<-1
    
    data_cr[[paste0(dd$disease[i],"_inc_date_cr")]][(data_cr[[paste0("type_of_cancer_icd10.",ii,".0")]] %in% unique(grep(dd$icd10[i],data_cr[[paste0("type_of_cancer_icd10.",ii,".0")]],value=TRUE))) &
                                                      data_cr[[paste0("date_of_cancer_diagnosis.",ii,".0")]] > data_cr$study_date &
                                                      data_cr[[paste0("date_of_cancer_diagnosis.",ii,".0")]] < data_cr[[paste0(dd$disease[i],"_inc_date_cr")]]]<-data_cr[[paste0("date_of_cancer_diagnosis.",ii,".0")]][(data_cr[[paste0("type_of_cancer_icd10.",ii,".0")]] %in% unique(grep(dd$icd10[i],data_cr[[paste0("type_of_cancer_icd10.",ii,".0")]],value=TRUE))) &
                                                                                                                                                                                                                          data_cr[[paste0("date_of_cancer_diagnosis.",ii,".0")]] > data_cr$study_date &
                                                                                                                                                                                                                          data_cr[[paste0("date_of_cancer_diagnosis.",ii,".0")]] < data_cr[[paste0(dd$disease[i],"_inc_date_cr")]]]
    
    
    print(table(data_cr[[paste0(dd$disease[i],"_inc_cr")]],useNA="ifany"))
    
  }
}

### HEALTH RELATED OUTCOMES - CANCER REGISTRY (PREVALENT) ####

dd<-data_disease %>% filter(cr=="y",prev=="y")

for(i in 1:length(dd$disease)){
  
  
  data_cr[[paste0(dd$disease[i],"_prev_cr")]]<-0
  
  for(ii in 0:(length(grep("type_of_cancer_icd10",names(data_cr),value=TRUE))-1)){
    
    data_cr[[paste0(dd$disease[i],"_prev_cr")]][(data_cr[[paste0("type_of_cancer_icd10.",ii,".0")]] %in% unique(grep(dd$icd10[i],data_cr[[paste0("type_of_cancer_icd10.",ii,".0")]],value=TRUE))) &
                                                  data_cr[[paste0("date_of_cancer_diagnosis.",ii,".0")]] < data_cr$study_date]<-1
    
    
    print(table(data_cr[[paste0(dd$disease[i],"_prev_cr")]],useNA="ifany"))
    
  }
  
  
  for(ii in 0:(length(grep("type_of_cancer_icd9",names(data_cr),value=TRUE))-1)){
    
    data_cr[[paste0(dd$disease[i],"_prev_cr")]][(data_cr[[paste0("type_of_cancer_icd9.",ii,".0")]] %in% unique(grep(dd$icd9[i],data_cr[[paste0("type_of_cancer_icd9.",ii,".0")]],value=TRUE)))]<-1
    
    
    print(table(data_cr[[paste0(dd$disease[i],"_prev_cr")]],useNA="ifany"))
    
  }
}

data_cr<-data_cr %>% select(grep(str_c(c("eid.","inc_cr","inc_date_cr","prev_cr"),collapse="|"),names(data_cr)))

### HEALTH RELATED OUTCOMES - HOSPITAL INPATIENT  ####

dd<-data_disease %>% filter(hes=="y",inc=="y")

for(i in 1:length(dd$disease)){
  
  data_hes[[paste0(dd$disease[i],"_inc_hes")]]<-0
  data_hes[[paste0(dd$disease[i],"_inc_date_hes")]]<-data_hes$end_date
  
  # ICD-10 diagnosis (incident)
  
  for(ii in 0:(length(grep("diagnoses_icd10.0.",names(data_hes),value=TRUE))-1)){
    
    data_hes[[paste0(dd$disease[i],"_inc_hes")]][(data_hes[[paste0("diagnoses_icd10.0.",ii)]] %in% unique(grep(dd$icd10[i],data_hes[[paste0("diagnoses_icd10.0.",ii)]],value=TRUE))) &
                                                   data_hes[[paste0("date_of_first_in_patient_diagnosis_icd10.0.",ii)]] > data_hes$study_date]<-1
    
    data_hes[[paste0(dd$disease[i],"_inc_date_hes")]][(data_hes[[paste0("diagnoses_icd10.0.",ii)]] %in% unique(grep(dd$icd10[i],data_hes[[paste0("diagnoses_icd10.0.",ii)]],value=TRUE))) &
                                                        data_hes[[paste0("date_of_first_in_patient_diagnosis_icd10.0.",ii)]] > data_hes$study_date & 
                                                        data_hes[[paste0("date_of_first_in_patient_diagnosis_icd10.0.",ii)]] < data_hes[[paste0(dd$disease[i],"_inc_date_hes")]]]<-data_hes[[paste0("date_of_first_in_patient_diagnosis_icd10.0.",ii)]][(data_hes[[paste0("diagnoses_icd10.0.",ii)]] %in% unique(grep(dd$icd10[i],data_hes[[paste0("diagnoses_icd10.0.",ii)]],value=TRUE))) &
                                                                                                                                                                                                                                                          data_hes[[paste0("date_of_first_in_patient_diagnosis_icd10.0.",ii)]] > data_hes$study_date & 
                                                                                                                                                                                                                                                          data_hes[[paste0("date_of_first_in_patient_diagnosis_icd10.0.",ii)]] < data_hes[[paste0(dd$disease[i],"_inc_date_hes")]]]
    
    print(table(data_hes[[paste0(dd$disease[i],"_inc_hes")]],useNA="ifany"))
    
  }
  
  # ICD-9 diagnosis (incident)  
  
  for(ii in 0:(length(grep("diagnoses_icd9.0.",names(data_hes),value=TRUE))-1)){
    
    data_hes[[paste0(dd$disease[i],"_inc_hes")]][(data_hes[[paste0("diagnoses_icd9.0.",ii)]] %in% unique(grep(dd$icd9[i],data_hes[[paste0("diagnoses_icd9.0.",ii)]],value=TRUE))) &
                                                   data_hes[[paste0("date_of_first_in_patient_diagnosis_icd9.0.",ii)]] > data_hes$study_date]<-1
    
    data_hes[[paste0(dd$disease[i],"_inc_date_hes")]][(data_hes[[paste0("diagnoses_icd9.0.",ii)]] %in% unique(grep(dd$icd9[i],data_hes[[paste0("diagnoses_icd9.0.",ii)]],value=TRUE))) &
                                                        data_hes[[paste0("date_of_first_in_patient_diagnosis_icd9.0.",ii)]] > data_hes$study_date & 
                                                        data_hes[[paste0("date_of_first_in_patient_diagnosis_icd9.0.",ii)]] < data_hes[[paste0(dd$disease[i],"_inc_date_hes")]]]<-data_hes[[paste0("date_of_first_in_patient_diagnosis_icd9.0.",ii)]][(data_hes[[paste0("diagnoses_icd9.0.",ii)]] %in% unique(grep(dd$icd9[i],data_hes[[paste0("diagnoses_icd9.0.",ii)]],value=TRUE))) &
                                                                                                                                                                                                                                                        data_hes[[paste0("date_of_first_in_patient_diagnosis_icd9.0.",ii)]] > data_hes$study_date & 
                                                                                                                                                                                                                                                        data_hes[[paste0("date_of_first_in_patient_diagnosis_icd9.0.",ii)]] < data_hes[[paste0(dd$disease[i],"_inc_date_hes")]]]
    
    
    print(table(data_hes[[paste0(dd$disease[i],"_inc_hes")]],useNA="ifany"))
    
  }
}

### Hospital in-patient - Prevalent disease across ICD9 and ICD10 #### 

dd<-data_disease %>% filter(hes=="y",prev=="y")

for(i in 1:length(dd$disease)){
  
  data_hes[[paste0(dd$disease[i],"_prev_hes")]]<-0
  
  # ICD-10 diagnosis (prevalent)
  
  for(ii in 0:(length(grep("diagnoses_icd10.0.",names(data_hes),value=TRUE))-1)){
    
    data_hes[[paste0(dd$disease[i],"_prev_hes")]][(data_hes[[paste0("diagnoses_icd10.0.",ii)]] %in% unique(grep(dd$icd10[i],data_hes[[paste0("diagnoses_icd10.0.",ii)]],value=TRUE))) &
                                                    data_hes[[paste0("date_of_first_in_patient_diagnosis_icd10.0.",ii)]] <= data_hes$study_date]<-1
    
    print(table(data_hes[[paste0(dd$disease[i],"_prev_hes")]],useNA="ifany"))
    
  }
  
  # ICD-9 diagnosis (prevalent)  
  
  for(ii in 0:(length(grep("diagnoses_icd9.0.",names(data_hes),value=TRUE))-1)){
    
    data_hes[[paste0(dd$disease[i],"_prev_hes")]][(data_hes[[paste0("diagnoses_icd9.0.",ii)]] %in% unique(grep(dd$icd9[i],data_hes[[paste0("diagnoses_icd9.0.",ii)]],value=TRUE))) &
                                                    data_hes[[paste0("date_of_first_in_patient_diagnosis_icd9.0.",ii)]] <= data_hes$study_date]<-1
    
    print(table(data_hes[[paste0(dd$disease[i],"_prev_hes")]],useNA="ifany"))
    
  }
}

data_hes<-data_hes %>% select(grep(str_c(c("eid.","inc_hes","inc_date_hes","prev_hes"),collapse="|"),names(data_hes)))

### SELF REPORTED ILLNESSES ####

### Self-reported - cancer ####

cancer_code<-read.table(file = paste0(path_2, 'coding3.tsv'), sep = '\t', header = TRUE)

table(data$cancer_code_self_reported.0.0)
grep("cancer_code_self_reported",names(data),value=TRUE)

disease<-c("all_cancer","endo_uter_cancer","bowel_cancer","breast_cancer","melanoma_cancer")

for(i in 1:length(disease)){
  
  code <- switch(disease[i],
                 all_cancer               = c(1001:1088),  
                 endo_uter_cancer         = c(1040),  
                 bowel_cancer             = c(1019,1020), 
                 breast_cancer            = c(1002), 
                 melanoma_cancer          = c(1059))            
  
  data[[paste0(disease[i],"_prev_sr")]]<-0
  for(ii in 0:(length(grep("cancer_code_self_reported",names(data)))-1)) {
    data[[paste0(disease[i],"_prev_sr")]][data[[paste0("cancer_code_self_reported.0.",ii)]] %in% c(code)]<-1
    data[[paste0(disease[i],"_prev_sr")]]<-as.factor(data[[paste0(disease[i],"_sr")]])
    print(table(data[[paste0(disease[i],"_prev_sr")]],useNA="ifany"))
  }
  
}

### Self-reported - Non-cancer illness ####

non_cancer_code<-read.table(file = paste0(path_2, 'coding6.tsv'), sep = '\t', header = TRUE)

table(data$non_cancer_illness_code_self_reported.0.0)
grep("non_cancer_illness_code_self_reported",names(data),value=TRUE)

disease<-c("diabetes","pcos","hypertension","hypertension_essential","uterine_polyp","schizophrenia","mania_depression","menopause_symp")

for(i in 1:length(disease)){
  
  code <- switch(disease[i],
                 diabetes               = c(1220),  
                 pcos                   = c(1137),  
                 hypertension           = c(1065), 
                 hypertension_essential = c(1072), 
                 uterine_polyp          = c(1457),
                 schizophrenia          = c(1289),     
                 mania_depression       = c(1291),   
                 menopause_symp         = c(1665))            
  
  data[[paste0(disease[i],"_prev_sr")]]<-0
  for(ii in 0:(length(grep("non_cancer_illness_code_self_reported",names(data)))-1)) {
    data[[paste0(disease[i],"_prev_sr")]][data[[paste0("non_cancer_illness_code_self_reported.0.",ii)]] %in% c(code)]<-1
    data[[paste0(disease[i],"_prev_sr")]]<-as.factor(data[[paste0(disease[i],"_sr")]])
    print(table(data[[paste0(disease[i],"_prev_sr")]],useNA="ifany"))
  }
  
}

### Self-reported - Operation ####

operation_code<-read.table(file = paste0(path_2, 'coding5.tsv'), sep = '\t', header = TRUE)
grep("operation_code",names(data),value=TRUE)

disease<-c("hysterectomy","bilateral_oophorectomy","unilateral_oophorectomy")

for(i in 1:length(disease)){
  
  code <- switch(disease[i],
                 bilateral_oophorectomy      = c(1355),
                 unilateral_oophorectomy     = c(1356),
                 hysterectomy                = c(1357),)            
  
  data[[paste0(disease[i],"_prev_op")]]<-0
  for(ii in 0:(length(grep("operation_code",names(data)))-1)) {
    data[[paste0(disease[i],"_prev_op")]][data[[paste0("operation_code.0.",ii)]] %in% c(code)]<-1
    data[[paste0(disease[i],"_prev_op")]]<-as.factor(data[[paste0(disease[i],"_op")]])
    print(table(data[[paste0(disease[i],"_prev_op")]],useNA="ifany"))
  }
  
}


med_all<-read.table(file = paste0(path_2, 'coding4.tsv'), sep = '\t', header = TRUE)
med_selected<-read.csv(paste0(path_6,"EC and OC semi-combined2.csv"))

### Multiple medications ####

medication<-c("progesterone_only_pill",
              "estrogen",
              "hormone_replacement_therapy",
              "tamoxifen")

for(ii in medication){
  
  med_list<-med_selected[med_selected$medication_group==ii & med_selected$present_in_ukb=="Y","ukb_name"]
  med_list<-str_c(med_list,collapse="|")
  
  med<-med_all %>% 
    filter(meaning %in% (grep(str_c(med_list,collapse="|"),meaning,value=TRUE)))
  med_code<-med[,1]
  
  treatment_medication_code<-grep("treatment_medication_code.0.",names(data),value=TRUE)
  
  data[[paste0("taking_",ii)]]<-0
  
  for(i in treatment_medication_code) {
    data[[paste0("taking_",ii)]][data[[paste0(i)]] %in% c(med_code)]<-1
    data[[paste0("taking_",ii)]]<-factor(data[[paste0("taking_",ii)]])
  }
  
  print(table(data[[paste0("taking_",ii)]],useNA="ifany"))
  
}

### Merge data ####

data_list <- list(data_censoring,
                  data_cr,
                  data_hes,
                  data)      

data<- data_list %>% reduce(inner_join, by='eid.')

rm(data_censoring)
rm(data_hes)
rm(data_cr)

saveRDS(data,paste0(path_5,"ukb_study_data_ec_analysis_base.rds"))

data<-readRDS(paste0(path_5,"ukb_study_data_ec_analysis_base.rds"))

###  Derived incident variables ####

data$endometrial_cancer_inc       <-ifelse(data$endometrial_cancer_inc_cr==1|data$endometrial_cancer_inc_hes==1|data$endometrial_cancer_du==1,1,0)
data$endometrial_cancer_x_inc     <-ifelse(data$corpus_uteri_cancer_inc_cr==1|data$corpus_uteri_cancer_inc_hes==1|data$corpus_uteri_cancer_du==1|data$uterine_cancer_inc_cr==1|data$uterine_cancer_inc_hes==1|data$uterine_cancer_du==1,1,0)
data$endometrial_cancer_inc_date  <-pmin(data$endometrial_cancer_inc_date_hes,data$endometrial_cancer_inc_date_cr)
data$endometrial_cancer_x_inc_date<-pmin(data$corpus_uteri_cancer_inc_date_cr,data$corpus_uteri_cancer_inc_date_hes,data$uterine_cancer_inc_date_cr,data$uterine_cancer_inc_date_hes)

### Derived prevalent variables ####

data$endometrial_cancer_prev      <-ifelse(data$endometrial_cancer_prev_hes==1|data$endometrial_cancer_prev_cr==1|data$endo_uter_cancer_sr==1,1,0)
data$endometrial_cancer_x_prev    <-ifelse(data$corpus_uteri_cancer_prev_hes==1|data$corpus_uteri_cancer_prev_cr==1|data$uterine_cancer_prev_hes==1|data$uterine_cancer_prev_cr==1|data$endo_uter_cancer_sr==1,1,0)
data$bowel_cancer_prev            <-ifelse(data$bowel_cancer_prev_hes==1|data$bowel_cancer_sr==1|data$bowel_cancer_prev_cr==1,1,0)
data$breast_cancer_prev           <-ifelse(data$breast_cancer_prev_hes==1|data$breast_cancer_sr==1|data$breast_cancer_prev_cr==1,1,0)
data$all_cancer_prev_doc          <-ifelse(data$cancer_diagnosed_by_doctor== "No",0,ifelse(data$cancer_diagnosed_by_doctor== "Yes - you will be asked about this later by an interviewer",1,NA))
data$all_cancer_prev              <-ifelse(data$all_cancer_prev_hes==1|data$all_cancer_sr==1|data$all_cancer_prev_doc==1|data$all_cancer_prev_cr==1,1,0)
data$age_high_blood_pressure_prev <-ifelse(is.na(data$age_high_blood_pressure_diagnosed),0,ifelse(data$age_high_blood_pressure_diag %in% c(-3,-1),NA,1))
data$hypertension_prev            <-ifelse(data$hypertension_prev_hes==1|data$age_high_blood_pressure_prev==1|data$hypertension_sr==1|data$hypertension_essential_sr==1,1,0)
data$diabetes_prev_doc            <-ifelse(data$diabetes_diagnosed_by_doctor== "No",0,ifelse(data$diabetes_diagnosed_by_doctor== "Yes",1,NA))
data$diabetes_prev                <-ifelse(data$diabetes_prev_hes==1|data$diabetes_prev_doc==1|data$diabetes_sr==1,1,0)
data$pcos_prev                    <-ifelse(data$pcos_sr==1|data$pcos_prev_hes==1,1,0)
data$man_sch_prev                 <-ifelse(data$schizophrenia_prev_hes==1 | data$mania_depression_prev_hes==1|data$schizophrenia_sr==1|data$mania_depression_sr==1,1,0)
data$endo_hyper_uter_polyp_prev    <-ifelse(data$endometrial_hyperplasia_prev_hes==1|data$uterine_polyp_prev_hes==1|data$uterine_polyp_sr==1,1,0)
data$endometrial_hyperplasia_prev <-data$endometrial_hyperplasia_prev_hes==1
data$uterine_polyp_prev           <-ifelse(data$uterine_polyp_prev_hes==1|data$uterine_polyp_sr==1,1,0)
data$age_at_hysterectomy_prev     <-ifelse(is.na(data$age_at_hysterectomy),0,ifelse(data$age_at_hysterectomy %in% c(-3,-1),NA,1))
data$had_hysterectomy             <-ifelse(data$ever_had_hysterectomy_womb_removed=="Yes",1,ifelse(data$ever_had_hysterectomy_womb_removed=="No",0,ifelse(data$ever_had_hysterectomy_womb_removed %in% c("Not sure","Prefer not to answer"),NA,data$ever_had_hysterectomy_womb_removed)))
data$hysterectomy_prev            <-ifelse(data$age_at_hysterectomy_prev==1|data$hysterectomy_prev_hes==1|data$hysterectomy_op==1|data$had_hysterectomy==1,1,0)
data$hysterectomy_prev_x          <-ifelse(data$hysterectomy_prev==1|data$bilateral_oophorectomy_op==1|data$unilateral_oophorectomy_op==1,1,0)
data$bilateral_oophorectomy_prev  <-data$bilateral_oophorectomy_op 
data$endometriosis_prev           <-data$endometriosis_prev_hes 
data$mania_depression_prev        <-ifelse(data$mania_depression_prev_hes==1|data$mania_depression_sr==1,1,0)
data$schizophrenia_prev           <-ifelse(data$schizophrenia_prev_hes==1|data$schizophrenia_sr==1,1,0)
data$sex                          <-ifelse(data$sex=="Female",0,1)
data$age_median_centred           <-data$age-median(data$age)
data$age_cat2                     <-ifelse(data$age<40,0,1)
data$age_cat4                     <-ifelse(data$age<45,0,ifelse(data$age<55,1,ifelse(data$age<65,2,3)))
data$ethnicity_cat3               <-ifelse(data$ethnicity %in% c("Prefer not to answer","Do not know","Other ethnic group","Mixed","Any other mixed background"),NA,ifelse(data$ethnicity %in% c( "White","British","Irish","Any other white background"),0,ifelse(data$ethnicity %in% c( "Black or British","White and Black Caribbean","White and Black African","African","Any other Black background","Caribbean"),1,2)))
data$smoking_cat3a                <-ifelse(data$smoking_status == "Never",0,ifelse(data$smoking_status == "Previous",1,ifelse(data$smoking_status == "Current",2,NA)))
data$smoking_cat3b                <-ifelse(data$smoking_status == "Current",0,ifelse(data$smoking_status == "Previous",1,ifelse(data$smoking_status == "Never",2,NA)))
data$current_number_of_cigs_daily <-ifelse(data$current_number_of_cigs_daily %in% c(-3,-1),NA,ifelse(data$current_number_of_cigs_daily %in% c(-10),0,data$current_number_of_cigs_daily))
data$smoking_cat5                 <-data$smoking_cat3a
data$smoking_cat5[data$smoking_cat3a==2 & !is.na(data$smoking_cat3a) & data$current_number_of_cigs_daily > 0 & data$current_number_of_cigs_daily < 10  ]<-2
data$smoking_cat5[data$current_number_of_cigs_daily > 9 & data$current_number_of_cigs_daily < 20 ]<-3
data$smoking_cat5[!is.na(data$smoking_cat3a) & data$current_number_of_cigs_daily > 19]<-4
data$bmi_mean_centred             <-data$bmi-mean(data$bmi,na.rm=TRUE)
data$bmi_cat4                     <-ifelse(data$bmi<25,0,ifelse(data$bmi<30,1,ifelse(data$bmi<35,2,3)))
data$bmi_cat5a                    <-ifelse(data$bmi <25,0,ifelse(data$bmi<30,1,ifelse(data$bmi<40,2,ifelse(data$bmi<50,3,4))))
data$bmi_cat5b                    <-ifelse(data$bmi>=18.5 & data$bmi<25,0,ifelse(data$bmi<18.5,1,ifelse(data$bmi<30,2,ifelse(data$bmi<35,3,4))))
data$parity_cat3                  <-ifelse(data$parity == -3,NA,ifelse(data$parity %in% c(1,2),1,ifelse(data$parity>=3,0,2)))
data$parity_cat4                  <-ifelse(data$parity == -3,NA,ifelse(data$parity>=3,3,data$parity))
data$parity_cat5                  <-ifelse(data$parity == -3,NA,ifelse(data$parity>=4,4,data$parity))
data$age_at_menarche<-ifelse(data$age_at_menarche %in% c(-3,-1),NA,data$age_at_menarche)
data$age_at_menarche_median_centred[!is.na(data$age_at_menarche)]<-data$age_at_menarche[!is.na(data$age_at_menarche)] - median(data$age_at_menarche,na.rm=TRUE)
data$age_at_menarche_cat5         <-ifelse(data$age_at_menarche<10,0,ifelse(data$age_at_menarche %in% c(10,11),1,ifelse(data$age_at_menarche %in% c(12,13),2,ifelse(data$age_at_menarche %in% c(14,15),3,4))))
data$ever_used_oc                 <-ifelse(data$ever_taken_oral_contraceptive_pill == "No",0,ifelse(data$ever_taken_oral_contraceptive_pill == "Yes",1,NA))
data$age_started_oral_contraceptive_pill       <-ifelse(data$age_started_oral_contraceptive_pill %in% c(-1,-3),NA,data$age_started_oral_contraceptive_pill)
data$age_when_last_used_oral_contraceptive_pill<-ifelse(data$age_when_last_used_oral_contraceptive_pill %in% c(-1,-3),NA,data$age_when_last_used_oral_contraceptive_pill)
data$age_when_last_used_oral_contraceptive_pill[data$age_when_last_used_oral_contraceptive_pill==-11 & !is.na(data$age_when_last_used_oral_contraceptive_pill)]<-data$age[data$age_when_last_used_oral_contraceptive_pill==-11 & !is.na(data$age_when_last_used_oral_contraceptive_pill)]
data$oc_use_yrs                   <-data$age_when_last_used_oral_contraceptive_pill - data$age_started_oral_contraceptive_pill
data$oc_use_cat2                  <-ifelse(data$oc_use_yrs >= 1,0,ifelse(data$oc_use_yrs < 1,1,NA))
data$oc_use_cat4                  <-ifelse(data$oc_use_yrs < 1,0,ifelse(data$oc_use_yrs < 6,1,ifelse(data$oc_use_yrs < 11,2,ifelse(data$oc_use_yrs >= 11,3,NA))))
data$oc_bmi_cat4                  <-ifelse(data$ever_used_oc==0,0,ifelse(data$ever_used_oc==1 & data$bmi<25,1,ifelse(data$ever_used_oc==1 & data$bmi<30,2,ifelse(data$ever_used_oc==1 & data$bmi>=30,3,NA))))
data$ever_used_hrt                <-ifelse(data$ever_used_hormone_replacement_therapy_hrt== "Yes"|data$taking_hormone_replacement_therapy==1,1,ifelse(data$ever_used_hormone_replacement_therapy_hrt== "No",0,NA))
data$ever_used_hrt_e              <-data$ever_used_hrt   # TBC
data$ever_used_hrt_ep             <-data$ever_used_hrt  # TBC
data$age_started_hormone_replacement_therapy_hrt<-ifelse(data$age_started_hormone_replacement_therapy_hrt %in% c(-1,-3),NA,data$age_started_hormone_replacement_therapy_hrt)
data$age_last_used_hormone_replacement_therapy_hrt<-ifelse(data$age_last_used_hormone_replacement_therapy_hrt %in% c(-1,-3),NA,data$age_last_used_hormone_replacement_therapy_hrt)
data$age_last_used_hormone_replacement_therapy_hrt[!is.na(data$age_last_used_hormone_replacement_therapy_hrt) & data$age_last_used_hormone_replacement_therapy_hrt==-11]<-data$age[!is.na(data$age_last_used_hormone_replacement_therapy_hrt) & data$age_last_used_hormone_replacement_therapy_hrt==-11] 
data$hrt_use_yrs                  <-data$age_last_used_hormone_replacement_therapy_hrt- data$age_started_hormone_replacement_therapy_hrt
data$hrt_use_cat3                 <-ifelse(data$hrt_use_yrs==0,0,ifelse(data$hrt_use_yrs < 11,1,ifelse(data$hrt_use_yrs >= 11,2,NA)))
data$hrt_use_cat4                 <-ifelse(data$hrt_use_yrs==0,0,ifelse(data$hrt_use_yrs < 6,1,ifelse(data$hrt_use_yrs < 10,2,ifelse(data$hrt_use_yrs >= 10,3,NA))))   # TBC
data$hrt_bmi_cat2                 <-ifelse(data$ever_used_hrt==0 | data$bmi >= 25,0,ifelse(data$ever_used_hrt==1 & data$bmi < 25,1,NA))
data$hrt_bmi_cat4                 <-ifelse(data$ever_used_hrt==0|data$bmi<25 ,0,ifelse(data$ever_used_hrt==1 & data$bmi<30,1,ifelse(data$ever_used_hrt==1 & data$bmi<35,2,ifelse(data$ever_used_hrt==1 & data$bmi>=35,3,NA))))
data$age_at_first_live_birth_1    <-ifelse(data$age_at_first_live_birth %in% c(-4,-3),NA,data$age_at_first_live_birth)
data$age_at_first_live_birth_median_centred[!is.na(data$age_at_first_live_birth_1)]<-data$age_at_first_live_birth_1[!is.na(data$age_at_first_live_birth_1)] - median(data$age_at_first_live_birth_1,na.rm=TRUE)
data$age_at_first_live_birth_2    <-ifelse(is.na(data$age_at_first_live_birth),1000,ifelse(data$age_at_first_live_birth %in% c(-4,-3),NA,data$age_at_first_live_birth))
data$age_at_first_live_birth_cat6 <-ifelse(data$age_at_first_live_birth_2<20,0,ifelse(data$age_at_first_live_birth_2<25,1,ifelse(data$age_at_first_live_birth_2<30,2,ifelse(data$age_at_first_live_birth_2<35,3,ifelse(data$age_at_first_live_birth_2<1000,4,5)))))
data$age_at_menopause_prev        <-ifelse(is.na(data$age_at_menopause),0,ifelse(data$age_at_menopause>0,1,data$age_at_menopause)) 
data$age_at_menopause             <-ifelse(data$age_at_menopause %in% c(-3,-1),NA,data$age_at_menopause) 
data$age_at_menopause_median_centred[!is.na(data$age_at_menopause)]<-data$age_at_menopause[!is.na(data$age_at_menopause)] - median(data$age_at_menopause,na.rm=TRUE)
data$age_at_menopause[is.na(data$age_at_menopause)]<-1000
data$age_at_menopause_cat4        <-ifelse(data$age_at_menopause<50,0,ifelse(data$age_at_menopause<55,1,ifelse(data$age_at_menopause<1000,2,3)))
data$had_menopause_prev           <-ifelse(data$had_menopause %in% c("No"),0, ifelse(data$had_menopause %in% c("Not sure - had a hysterectomy"),1,ifelse(data$had_menopause %in% c("Yes"),2,NA)))
data$menopause_cat2               <-ifelse(data$had_menopause_prev %in% c(0,1),0,ifelse(data$had_menopause_prev %in% c(2),1,NA))
data$menopause_cat2_x             <-ifelse(data$had_menopause_prev %in% c(0),0,ifelse(data$had_menopause_prev %in% c(1,2),1,NA))
data$age_cat3                     <-ifelse(data$age < 46,0,ifelse((data$age >=46 & data$age<56),1,2))
data$menopause_cat21              <-ifelse(is.na(data$menopause_cat2),2,data$menopause_cat2)
data$menopause_cat21_x            <-ifelse(is.na(data$menopause_cat2_x),2,data$menopause_cat2_x)
data$menopause_cat3               <-ifelse(((data$menopause_cat21==0 & data$age_cat3 %in% c(0,1)) | data$age_cat3==0),0,
                                           ifelse(((data$menopause_cat21==1 & data$age_cat3 %in% c(1,2)) | data$age_cat3==2 | data$bilateral_oophorectomy_op==1),2,
                                                  ifelse((data$age_cat3 %in% c(1) & data$menopause_cat21==2),1,NA)))
data$menopause_cat3_x             <-ifelse(((data$menopause_cat21_x==0 & data$age_cat3 %in% c(0,1)) | data$age_cat3==0),0,
                                           ifelse(((data$menopause_cat21_x==1 & data$age_cat3 %in% c(1,2)) | data$age_cat3==2 | data$bilateral_oophorectomy_op==1),2,
                                                  ifelse((data$age_cat3 %in% c(1) & data$menopause_cat21_x==2),1,NA)))
data$smoking_cat3<-ifelse(is.na(data$smoking_cat3a),3,data$smoking_cat3a)

data$menopause_smoke_cat10<-ifelse(data$smoking_cat3==0,0,
                                   ifelse(data$menopause_cat3==0 & data$smoking_cat3==1,1,
                                          ifelse(data$menopause_cat3==0 & data$smoking_cat3==2,2, 
                                                 ifelse(data$menopause_cat3==0 & data$smoking_cat3==3,3,
                                                        ifelse(data$menopause_cat3==1 & data$smoking_cat3==1,4,
                                                               ifelse(data$menopause_cat3==1 & data$smoking_cat3==2,5,
                                                                      ifelse(data$menopause_cat3==1 & data$smoking_cat3==3,6,
                                                                             ifelse(data$menopause_cat3==2 & data$smoking_cat3==1,7,
                                                                                    ifelse(data$menopause_cat3==2 & data$smoking_cat3==2,8,
                                                                                           ifelse(data$menopause_cat3==2 & data$smoking_cat3==3,9,NA))))))))))
data$menopause_smoke_cat10_x<-ifelse(data$smoking_cat3==0,0,
                                     ifelse(data$menopause_cat3_x==0 & data$smoking_cat3==1,1,
                                            ifelse(data$menopause_cat3_x==0 & data$smoking_cat3==2,2, 
                                                   ifelse(data$menopause_cat3_x==0 & data$smoking_cat3==3,3,
                                                          ifelse(data$menopause_cat3_x==1 & data$smoking_cat3==1,4,
                                                                 ifelse(data$menopause_cat3_x==1 & data$smoking_cat3==2,5,
                                                                        ifelse(data$menopause_cat3_x==1 & data$smoking_cat3==3,6,
                                                                               ifelse(data$menopause_cat3_x==2 & data$smoking_cat3==1,7,
                                                                                      ifelse(data$menopause_cat3_x==2 & data$smoking_cat3==2,8,
                                                                                             ifelse(data$menopause_cat3_x==2 & data$smoking_cat3==3,9,NA))))))))))

data$education_cat3 <- ifelse(data$qualifications.0.0 %in% c("O levels/GCSEs or equivalent","CSEs or equivalent"),"Compulsory schooling",
                                         ifelse(data$qualifications.0.0 %in% c("A levels/AS levels or equivalent", "NVQ or HND or HNC or equivalent"),"College or further education",
                                         ifelse(data$qualifications.0.0 %in% c("College or University degree","Other professional qualifications eg: nursing, teaching"),"University or professional",NA)))



### Save dataset without removing withdrawals ####

saveRDS(data,paste0(path_5,"ukb_study_data_ec_analysis.rds"))

data<-readRDS(paste0(path_5,"ukb_study_data_ec_analysis.rds"))

# Exclude individuals who have withdrawn consent

withdrawn <- read.table(paste0(path_3,"withdraw90686_246_20240520.txt"))

data <- data[-which(as.character(data$eid.) %in% as.character(withdrawn$V1)),]

### Save dataset removing withdrawals ####

saveRDS(data,paste0(path_5,"ukb_study_data_ec_analysis_wd_20_05_2024.rds"))

data<-readRDS(paste0(path_5,"ukb_study_data_ec_analysis_wd_20_05_2024.rds"))

### Import meta data (risk factor data) ####

tab<-read.csv(paste0(path_6,"EC_risk_factors.csv"))

# Select risk factors in ukb dataset

tab<-tab[tab$varnames %in% grep(str_c(tab$varnames[tab$ukb_variable=="Yes"],collapse="|"),tab$varnames,value=TRUE),]

risk_factors<-tab$varnames[tab$ukb_variable=="Yes"] 

### Select all censoring, exclusions, outcomes, risk factors ####

data<-data %>%
  select(eid.,
         sex,
         age,
         endometrial_cancer_prev, 
         endometrial_cancer_x_prev,
         all_cancer_prev,
         bilateral_oophorectomy_op,
         hysterectomy_prev,
         hysterectomy_prev_x,
         menopause_cat2,
         bmi,
         taking_tamoxifen,
         endometrial_cancer_inc,
         endometrial_cancer_x_inc,
         study_date,
         endometrial_cancer_inc_date,
         endometrial_cancer_x_inc_date,
         all_of(risk_factors))     

saveRDS(data,paste0(path_5,"ukb_study_data_ec_analysis_wd_20_05_2024_selected.rds"))

#table(data$endometrial_cancer_inc_cr,useNA="ifany")
#table(data$endometrial_cancer_inc_hes,useNA="ifany")
#table(data$endometrial_cancer_du,useNA="ifany")
#table(data$corpus_uteri_cancer_inc_cr,useNA="ifany")
#table(data$corpus_uteri_cancer_inc_hes,useNA="ifany")
#table(data$corpus_uteri_cancer_du,useNA="ifany")
#table(data$uterine_cancer_inc_cr,useNA="ifany")
#table(data$uterine_cancer_inc_hes,useNA="ifany")
#table(data$uterine_cancer_du,useNA="ifany")
#table(data$endometrial_cancer_inc,useNA="ifany")
#table(data$endometrial_cancer_x_inc,useNA="ifany")
#table(data$endometrial_cancer_prev_hes,useNA="ifany")
#table(data$endometrial_cancer_prev_cr,useNA="ifany")
#table(data$endo_uter_cancer_sr,useNA="ifany")
#table(data$endometrial_cancer_prev,useNA="ifany")
#table(data$corpus_uteri_cancer_prev_hes,useNA="ifany")
#table(data$corpus_uteri_cancer_prev_cr,useNA="ifany")
#table(data$uterine_cancer_prev_hes,useNA="ifany")
#table(data$uterine_cancer_prev_cr,useNA="ifany")
#table(data$endo_uter_cancer_sr,useNA="ifany")
#table(data$endometrial_cancer_x_prev,useNA="ifany")
#table(data$bowel_cancer_prev_hes,useNA="ifany")
#table(data$bowel_cancer_prev_cr,useNA="ifany")
#table(data$bowel_cancer_sr,useNA="ifany")
#table(data$breast_cancer_prev_hes,useNA="ifany")
#table(data$breast_cancer_prev_cr,useNA="ifany")
#table(data$breast_cancer_sr,useNA="ifany")
#table(data$breast_cancer_prev,useNA="ifany")
#table(data$cancer_diagnosed_by_doctor,useNA="ifany")
#table(data$all_cancer_prev_doc,useNA="ifany")
#table(data$all_cancer_prev_hes,useNA="ifany")
#table(data$all_cancer_prev_cr,useNA="ifany")
#table(data$all_cancer_sr,useNA="ifany")
#table(data$all_cancer_prev,useNA="ifany") 
#table(data$age_high_blood_pressure_diagnosed,useNA="ifany")
#table(data$hypertension_prev_hes,useNA="ifany")
#table(data$hypertension_sr,useNA="ifany")
#table(data$hypertension_essential_sr,useNA="ifany")
#table(data$hypertension_prev,useNA="ifany")
#table(data$high_blood_pressure_prev,useNA="ifany")
#table(data$diabetes_prev_hes,useNA="ifany")
#table(data$diabetes_sr,useNA="ifany")
#table(data$diabetes_diagnosed_by_doctor,useNA="ifany")
#table(data$diabetes_prev,useNA="ifany")
#table(data$pcos_sr,useNA="ifany")
#table(data$pcos_prev_hes,useNA="ifany")
#table(data$pcos_prev,useNA="ifany")
#table(data$schizophrenia_prev_hes,useNA="ifany")
#table(data$manic_depression_prev_hes,useNA="ifany")
#table(data$schizophrenia_sr,useNA="ifany")
#table(data$mania_depression_sr,useNA="ifany")
#table(data$man_sch_prev,useNA="ifany")
#table(data$endometrial_hyperplasia_prev_hes,useNA="ifany")
#table(data$uterine_polyp_prev_hes,useNA="ifany")
#table(data$uterine_polyp_sr,useNA="ifany")
#table(data$endo_hyper_prev,useNA="ifany")
#table(data$age_at_hysterectomy,useNA="ifany")
#table(data$age_at_hysterectomy_prev,useNA="ifany")
#table(data$hysterectomy_prev_hes,useNA="ifany")
#table(data$hysterectomy_sr,useNA="ifany")
#table(data$ever_had_hysterectomy_womb_removed,useNA="ifany")
#table(data$had_hysterectomy,useNA="ifany")
#table(data$hysterectomy_prev,useNA="ifany")
#table(data$bilateral_oophorectomy_sr,useNA="ifany")
#table(data$unilateral_oophorectomy_sr,useNA="ifany")
#table(data$hysterectomy_prev_x,useNA="ifany") 
#table(data$sex,useNA="ifany")
#summary(data$age) 
#summary(data$age_median_centred)
#table(data$ethnicity,useNA="ifany")
#table(data$ethnicity_cat3,useNA="ifany")
#table(data$age_cat2,useNA="ifany")
#table(data$age_cat5,useNA ="ifany")
#table(data$education,useNA="ifany")
#table(data$smoking_status,useNA="ifany")
#table(data$smoking_cat3a,useNA="ifany")
#table(data$smoking_cat3b,useNA="ifany")
#table(data$current_number_of_cigs_daily,useNA="ifany")
#table(data$current_number_of_cigs_daily,useNA="ifany")
#table(data$current_number_of_cigs_daily,data$smoking_cat3a,useNA="ifany")
#table(data$smoking_cat5,useNA="ifany")
#summary(data$bmi,useNA="ifany")
#summary(data$bmi_centred)
#table(data$bmi_cat4,useNA="ifany")
#table(data$bmi_cat5a,useNA="ifany")
#table(data$bmi_cat5b,useNA="ifany")
#table(data$parity,useNA="ifany")
#table(data$parity_cat3, useNA="ifany")
#table(data$parity_cat4, useNA="ifany")
#table(data$parity_cat5, useNA="ifany")                        
#table(data$age_at_menarche, useNA = "ifany")
#table(data$age_at_menarche, useNA = "ifany")
#table(data$age_at_menarche_median_centred)
#table(data$age_at_menarche_cat5,useNA="ifany")
#table(data$ever_taken_oral_contraceptive_pill,useNA="ifany") 
#table(data$ever_used_oc,useNA="ifany")
#table(data$age_started_oral_contraceptive_pill,useNA = "ifany")
#table(data$age_started_oral_contraceptive_pill,useNA = "ifany")
#table(data$age_when_last_used_oral_contraceptive_pill,useNA= "ifany")
#table(data$age_when_last_used_oral_contraceptive_pill,useNA= "ifany")
#table(data$oc_use_yrs,useNA="ifany")
#table(data$oc_use_cat2,useNA = "ifany")
#table(data$oc_use_cat4, useNA = "ifany")
#table(data$oc_bmi_cat4,useNA="ifany")
#table(data$ever_used_hormone_replacement_therapy_hrt, useNA = "ifany")
#table(data$ever_used_hrt, useNA="ifany")
#table(data$age_started_hormone_replacement_therapy_hrt, useNA = "ifany")
#table(data$age_started_hormone_replacement_therapy_hrt, useNA = "ifany")
#table(data$age_last_used_hormone_replacement_therapy_hrt, useNA = "ifany")
#table(data$age_last_used_hormone_replacement_therapy_hrt, useNA = "ifany")
#table(data$age_last_used_hormone_replacement_therapy_hrt, useNA = "ifany")
#table(data$hrt_use_yrs, useNA="ifany")
#table(data$hrt_use_cat3, useNA = "ifany")
#table(data$hrt_use_cat4, useNA = "ifany")
#table(data$hrt_bmi_cat2,useNA="ifany")
#table(data$hrt_bmi_cat4,useNA="ifany")
#table(data$age_at_menopause,useNA="ifany")
#table(data$age_at_menopause,useNA="ifany")
#table(data$age_at_menopause_median_centred,useNA="ifany")
#table(data$age_at_menopause,useNA="ifany")
#table(data$age_at_menopause_cat4,useNA="ifany")
#table(data$age_at_first_live_birth, useNA = "ifany")
#table(data$age_at_first_live_birth_1, useNA = "ifany")
#table(data$first_live_birth_age_median_centred)
#table(data$age_at_first_live_birth_2, useNA = "ifany")
#table(data$age_at_first_live_birth_cat6,useNA="ifany")
#table(data$menopause_cat2,useNA="ifany")
#table(data$menopause_cat2_x,useNA="ifany")
#table(data$menopause_cat21,useNA="ifany")
#table(data$menopause_cat21_x,useNA="ifany")
#table(data$age_cat3,useNA="ifany")
#table(data$age_cat3,data$menopause_cat2,useNA="ifany")
#table(data$age_cat3,data$menopause_cat21,useNA="ifany")
#table(data$age_cat3,data$menopause_cat21_x,useNA="ifany")
#table(data$had_menopause,useNA="ifany")
#table(data$age_at_menopause_prev,useNA="ifany")
#table(data$had_menopause,data$age_at_menopause_prev,useNA="ifany")
#table(data$had_menopause_prev,useNA="ifany")
#table(data$menopause_cat2,useNA="ifany")
#table(data$menopause_cat2_x,useNA="ifany")
#table(data$menopause_cat3,useNA="ifany")
#table(data$menopause_cat3_x,useNA="ifany")
#table(data$smoking_cat3a,useNA="ifany")
#table(data$smoking_cat3,useNA="ifany")
#table(data$menopause_cat2,data$smoking_cat3,useNA="ifany")
#table(data$menopause_cat2_x,data$smoking_cat3,useNA="ifany")
#table(data$menopause_cat3,data$smoking_cat3,useNA="ifany")
#table(data$menopause_cat3_x,data$smoking_cat3,useNA="ifany")
#table(data$menopause_smoke_cat10,useNA="ifany")
#table(data$menopause_smoke_cat10_x,useNA="ifany")



