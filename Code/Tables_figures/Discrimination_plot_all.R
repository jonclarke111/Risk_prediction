
### Load libraries ####

library(tidyverse)
library(survival)
library(lattice)
library(ggplot2)
library(Hmisc)
library(ckbplotr)
library(SurvMetrics)
library(readxl)

### SET PATHS ####

path_to_data<-"K:/kadoorie/Staff_Folders/JonathanC/Projects/Cancer_risk_prediction/Tables_Figures/EC/Tables/"
path_to_figure<-"K:/kadoorie/Staff_Folders/JonathanC/Projects/Cancer_risk_prediction/Tables_Figures/EC/Figures/"

#data_obs<-"fig_data_cox_dis_lp_main_0_obs"
#data_gen<-"fig_data_cox_lp_dis_main_0_gen"   

############################################################################################################################
############################################### MAIN FIGURE (RIsK SCORE) ###################################################
############################################################################################################################

process_data<-function(data_obs,data_gen,figure){
  
  fig_data_obs<-read.csv(paste0(path_to_data,data_obs,".csv"))
  fig_data_gen<-read_excel(paste0(path_to_data,"fig_data_dis_all_gen.xlsx"),sheet=paste0(data_gen))
  
  fig_data_gen<-as.data.frame(t(fig_data_gen))
  names(fig_data_gen)<-fig_data_gen[1,] 
  fig_data_gen<-fig_data_gen[2:13,]
  fig_data_gen$study_id<-row.names(fig_data_gen)
  fig_data_gen$snps<-format(round(as.numeric(fig_data_gen$snps),0),big.mark=",",scientific = FALSE)
  fig_data_gen$snps[11:12]<-""
  

  fig_data_obs$X<-NA
  fig_data_obs<-fig_data_obs[,1:7]
  names(fig_data_obs)<-c("snps","est","stderr","cases","non_cases","brsc","study_id")
  
  fig_data<-rbind(fig_data_obs,fig_data_gen)
  
  ### Format data ####
  
  fig_data$cases<-round(as.numeric(fig_data$cases),0)    
  fig_data$all<-fig_data$cases + as.numeric(fig_data$non_cases)   
  fig_data$all<-format(fig_data$all,big.mark=",",scientific = FALSE)
  fig_data$cases<-format(fig_data$cases,big.mark=",",scientific = FALSE)
  fig_data$controls<-format(fig_data$non_cases,big.mark=",",scientific = FALSE)
  fig_data$cases_all<-paste0(fig_data$cases,"/",fig_data$all)
  fig_data$brsc<-format(round(as.numeric(fig_data$brsc),4),digits=4,nsmall=1,scientific=FALSE)
  fig_data$est<-as.numeric(fig_data$est)
  fig_data$stderr<-as.numeric(fig_data$stderr)
  
  fig_data$author<-c("Kim et al.,",          
                     "Li (full) et al.,",         
                     "Li (selected) et al.,",          
                     "Funston et al.,",        
                     "Rosner et al.,",          
                     "Hippisley-Cox et al.,",  
                     "Hippisley-Cox et al.,",   
                     "Pfeiffer et al.,",          
                     "Urban et al.,",
                     "Pieta et al.,",
                     "Yang et al.,",  
                     "Kuchenbaecker et al.,",  
                     "Barnes et al.,",  
                     "\"          \"", 
                     "\"          \"", 
                     "\"          \"", 
                     "Dareng et al.,", 
                     "\"          \"", 
                     "\"          \"", 
                     "\"          \"", 
                     "Clyde et al.,",
                     "Dite et al.,")
  
  fig_data$date<-c("2004","2015","2015","2021","2005","2011","2015","2013","2015","2018","2017","2020","2020","","","","2024","","","","2016","2023")
  
  fig_data$author_date<-paste0(fig_data$author," ",fig_data$date)
  fig_data$data<-rep(c("Questionnaire-based data","Genetic data"),c(10,12))
  fig_data$population<-rep(c("General population","High-risk population","General population","High-risk population"),c(11,3,6,2))
  fig_data$all_order<-c(1,7,6,10,2,3,4,5,8,9,11,20,18,19,14,15,12,17,13,16,21,22)
  fig_data$cases_all[c(2,14,15:16,18:20)]<-"\"  \""
  fig_data$population <- glue::glue("<span style='color:transparent;'>XX</span>{fig_data$population}")
  fig_data$author_date <- glue::glue("<span style='color:transparent;'>XXXX</span>{fig_data$author_date}")
  fig_data$model_name<-c("HCR"," \"\"     \"\"","Li et. al.",NA,NA,"QCancer"," \"\"   \"\" ",NA,NA,"RAFOCC","PRS          15","PRS          17","PRS          22","PRS          30","PRS     1,403","PRS  10,797","PRS          22","PRS  27,240","PRS          36","PRS  18,007",NA,NA)
  
  ### levels and order ####
  
  fig_data<-fig_data %>% arrange(desc(data),population,all_order)

  # Drop selected studies
    
  fig_data<-subset(fig_data, !(study_id %in% c("barnes_b_20","liF_15","dareng_s1","dareng_s2","dareng_s4","dareng_s5","dareng_s6")))
  
  ### Create forest plot ####
  
  fp_plot<-forest_plot(fig_data,
                       col.estimate        = "est",
                       col.stderr          = "stderr",
                       col.key             = "study_id",
                       exponentiate        = FALSE,
                       row.labels          = fig_data,
                       row.labels.levels   = c("data","population","author_date"),
                       row.labels.heading  = c("Author, year"),
                       col.left            = c("cases_all","model_name"),
                       col.left.heading    = c("Cases/All","Model \n(if applicable)"),
                       left.space          = unit(48, "mm"),
                       col.left.hjust      = c(1,0),
                       col.left.pos        = unit(c(0,45), "mm"),
                       col.right           =  "brsc",
                       col.right.hjust     = 1,
                       nullval             = 0.5,
                       scalepoints         = TRUE,
                       row.labels.space    = c(0,0,0,0),
                       xlim                = c(0.44, 0.77),
                       xticks              = c(0.50,0.55,0.6,0.65,0.70),  
                       panel.width         = unit(100,"mm"),
                       col.right.heading   = c("C-index (95% CI)","Brier \nscore"),
                       col.heading.space   = 0.5,
                       xlab                = "C-index (95% CI)",
                       plot.margin         = margin(5,0,0,0,"mm"))
  
  ggsave(paste0(path_to_figure,figure),
         plot=fp_plot$plot,
         dpi = 450,
         width = 26,
         height = 15,
         bg  = "white",
         units = "cm")
  
}

process_data("fig_data_cox_dis_lp_main_0_obs","fig_data_cox_lp_dis_main_0_gen","Figure_2_cox_dis_lp_main_0t.png")    

###################################################################################################################################
############################################ SUPPLEMENTARY FIGUREs (RISK SCORE) ###################################################
###################################################################################################################################

#data_obs<-"fig_data_cox_dis_lp_main_na_obs"
#data_gen<-"fig_data_cox_lp_dis_main_na_gen"   

process_data<-function(data_obs,data_gen,figure){
  
  fig_data_obs<-read.csv(paste0(path_to_data,data_obs,".csv"))
  fig_data_gen<-read_excel(paste0(path_to_data,"fig_data_dis_all_gen.xlsx"),sheet=paste0(data_gen))
  
  fig_data_gen<-as.data.frame(t(fig_data_gen))
  names(fig_data_gen)<-fig_data_gen[1,] 
  fig_data_gen<-fig_data_gen[2:13,]
  fig_data_gen$study_id<-row.names(fig_data_gen)
  fig_data_gen$snps<-format(round(as.numeric(fig_data_gen$snps),0),big.mark=",",scientific = FALSE)
  fig_data_gen$snps[11:12]<-""
  
  fig_data_obs$X<-NA
  fig_data_obs<-fig_data_obs[,1:7]
  names(fig_data_obs)<-c("snps","est","stderr","cases","non_cases","brsc","study_id")
  
  fig_data<-rbind(fig_data_obs,fig_data_gen)
  
  ### Format data ####
  
  fig_data$cases<-round(as.numeric(fig_data$cases),0)    
  fig_data$all<-fig_data$cases + as.numeric(fig_data$non_cases)   
  fig_data$all<-format(fig_data$all,big.mark=",",scientific = FALSE)
  fig_data$cases<-format(fig_data$cases,big.mark=",",scientific = FALSE)
  fig_data$controls<-format(fig_data$non_cases,big.mark=",",scientific = FALSE)
  fig_data$cases_all<-paste0(fig_data$cases,"/",fig_data$all)
  fig_data$brsc<-format(round(as.numeric(fig_data$brsc),4),digits=4,nsmall=1,scientific=FALSE)
  fig_data$est<-as.numeric(fig_data$est)
  fig_data$stderr<-as.numeric(fig_data$stderr)
  
  fig_data$author<-c("Kim et al.,",          
                     "Li (full) et al.,",         
                     "Li (selected) et al.,",          
                     "Funston et al.,",        
                     "Rosner et al.,",          
                     "Hippisley-Cox et al.,",  
                     "Hippisley-Cox et al.,",   
                     "Pfeiffer et al.,",            
                     "Urban et al.,",
                     "Pieta et al.,",
                     "Yang et al.,",  
                     "Kuchenbaecker et al.,",  
                     "Barnes et al.,",  
                     "\"          \"", 
                     "\"          \"", 
                     "\"          \"", 
                     "Dareng et al.,", 
                     "\"          \"", 
                     "\"          \"", 
                     "\"          \"", 
                     "Clyde et al.,",
                     "Dite et al.,")
  
  fig_data$date<-c("2004","2015","2015","2021","2005","2011","2015","2013","2015","2018","2017","2020","2020","","","","2024","","","","2016","2023")
  
  fig_data$author_date<-paste0(fig_data$author," ",fig_data$date)
  fig_data$data<-rep(c("Questionnaire-based data","Genetic data"),c(10,12))
  fig_data$population<-rep(c("General population","High-risk population","General population","High-risk population"),c(11,3,6,2))
  fig_data$all_order<-c(1,7,6,10,2,3,4,5,8,9,11,20,18,19,14,15,12,17,13,16,21,22)
  fig_data$cases_all[c(14,15:16,18:20)]<-"\"  \""                                               # Missing as NA
#  fig_data$cases_all[c(2,14,15:16,18:20)]<-"\"  \""                                            # Missing as 0
  fig_data$population <- glue::glue("<span style='color:transparent;'>XX</span>{fig_data$population}")
  fig_data$author_date <- glue::glue("<span style='color:transparent;'>XXXX</span>{fig_data$author_date}")
  
  fig_data$model_name<-c("HCR"," \"\"     \"\"","Li et. al.",NA,NA,"QCancer"," \"\"   \"\" ",NA,NA,"RAFOCC","PRS          15","PRS          17","PRS          22","PRS          30","PRS     1,403","PRS  10,797","PRS          22","PRS  27,240","PRS          36","PRS  18,007",NA,NA)
  
  ### levels and order ####
  
  fig_data<-fig_data %>% arrange(desc(data),population,all_order)
  
  ### Create forest plot ####
  
  fp_plot<-forest_plot(fig_data,
                       col.estimate        = "est",
                       col.stderr          = "stderr",
                       col.key             = "study_id",
                       exponentiate        = FALSE,
                       row.labels          = fig_data,
                       row.labels.levels   = c("data","population","author_date"),
                       row.labels.heading  = c("Author, year"),
                       col.left            = c("cases_all","model_name"),
                       col.left.heading    = c("Cases/All","Model \n(if applicable)"),
                       left.space          = unit(48, "mm"),
                       col.left.hjust      = c(1,0),
                       col.left.pos        = unit(c(0,45), "mm"),
                       col.right           =  "brsc",
                       col.right.hjust     = 1,
                       nullval             = 0.5,
                       scalepoints         = TRUE,
                       row.labels.space    = c(0,0,0,0),
                       xlim                = c(0.44, 0.77),
                       xticks              = c(0.50,0.55,0.6,0.65,0.70),  
                       panel.width         = unit(100,"mm"),
                       col.right.heading   = c("C-index (95% CI)","Brier \nscore"),
                       col.heading.space   = 0.5,
                       xlab                = "C-index (95% CI)",
                       plot.margin         = margin(5,0,0,0,"mm"))
  
  
  ggsave(paste0(path_to_figure,figure),
         plot=fp_plot$plot,
         dpi = 450,
         width = 26,
         height = 22,
         bg  = "white",
         units = "cm")
  
}

process_data("fig_data_cox_dis_lp_main_na_obs","fig_data_cox_lp_dis_main_na_gen","eFigure_2_cox_dis_lp_main_nat.png")    
process_data("fig_data_cox_dis_lp_broad_0_obs","fig_data_cox_lp_dis_broad_0_gen","eFigure_6_cox_dis_lp_broad_0t.png")    
process_data("fig_data_cox_dis_lp_broad_na_obs","fig_data_cox_lp_dis_broad_na_ge","eFigure_4_cox_dis_lp_broad_nat.png")    
process_data("fig_data_log_dis_lp_main_0_obs","fig_data_log_lp_dis_main_0_gen","eFigure_8_log_dis_lp_main_0t.png")   

#####################################################################################################################################
################################################# SUPPLEMENTARY FIGURES (RECALIBRATION) #############################################
#####################################################################################################################################

#data_obs<-"fig_data_cox_dis_main_0_obs"

process_data<-function(data_obs,figure){
  
  fig_data<-read.csv(paste0(path_to_data,data_obs,".csv"))

  fig_data$X<-NA  
  fig_data<-fig_data[,1:7]
  names(fig_data)<-c("snps","est","stderr","cases","non_cases","brsc","study_id")
  
  ### Format data ####
  
  fig_data$cases<-round(as.numeric(fig_data$cases),0)    
  fig_data$all<-fig_data$cases + as.numeric(fig_data$non_cases)   
  fig_data$all<-format(fig_data$all,big.mark=",",scientific = FALSE)
  fig_data$cases<-format(fig_data$cases,big.mark=",",scientific = FALSE)
  fig_data$controls<-format(fig_data$non_cases,big.mark=",",scientific = FALSE)
  fig_data$cases_all<-paste0(fig_data$cases,"/",fig_data$all)
  fig_data$brsc<-format(round(as.numeric(fig_data$brsc),4),digits=4,nsmall=1,scientific=FALSE)
  fig_data$est<-as.numeric(fig_data$est)
  fig_data$stderr<-as.numeric(fig_data$stderr)
  
  fig_data$author<-c("Kim et al.,",          
                     "Li (full) et al.,",         
                     "Li (selected) et al.,",          
                     "Funston et al.,",        
                     "Rosner et al.,",          
                     "Hippisley-Cox et al.,",  
                     "Hippisley-Cox et al.,",   
                     "Pfeiffer et al.,",         
                     "Urban et al.,",
                     "Hibler et al.,",
                     "Pieta et al.,")
  
  fig_data$date<-c("2004","2015","2015","2021","2005","2011","2015","2013","2015","2022","2018")
  
  fig_data$author_date<-paste0(fig_data$author," ",fig_data$date)
  fig_data$data<-rep(c("Questionnaire-based data"),c(11))
  fig_data$population<-rep(c("General population"),c(11))
  fig_data$all_order<-c(1,7,6,10,2,3,4,5,8,11,9)
 # fig_data$cases_all[c(2)]<-"\"  \""                                                # Run line for missing as 0
  fig_data$population <- glue::glue("<span style='color:transparent;'>XX</span>{fig_data$population}")
  fig_data$author_date <- glue::glue("<span style='color:transparent;'>XXXX</span>{fig_data$author_date}")
  
  fig_data$model_name<-c("HCR"," \"\"     \"\"","Li et. al.",NA,NA,"QCancer"," \"\"   \"\" ",NA,NA,"BAYRT","RAFOCC")
  
  ### levels and order ####

  fig_data<-fig_data %>% arrange(desc(data),population,all_order)
  
  ### Create forest plot ####
  
  fp_plot<-forest_plot(fig_data,
                       col.estimate        = "est",
                       col.stderr          = "stderr",
                       col.key             = "study_id",
                       exponentiate        = FALSE,
                       row.labels          = fig_data,
                       row.labels.levels   = c("data","population","author_date"),
                       row.labels.heading  = c("Author, year"),
                       col.left            = c("cases_all","model_name"),
                       col.left.heading    = c("Cases/All","Model \n(if applicable)"),
                       left.space          = unit(48, "mm"),
                       col.left.hjust      = c(1,0),
                       col.left.pos        = unit(c(0,45), "mm"),
                       col.right           =  "brsc",
                       col.right.hjust     = 1,
                       nullval             = 0.5,
                       scalepoints         = TRUE,
                       row.labels.space    = c(0,0,0,0),
                       xlim                = c(0.44, 0.77),
                       xticks              = c(0.50,0.55,0.6,0.65,0.70),  
                       panel.width         = unit(100,"mm"),
                       col.right.heading   = c("C-index (95% CI)","Brier \nscore"),
                       col.heading.space   = 0.5,
                       xlab                = "C-index (95% CI)",
                       plot.margin         = margin(5,0,0,0,"mm"))
  
  ggsave(paste0(path_to_figure,figure),
         plot=fp_plot$plot,
         dpi = 450,
         width = 24,
         height = 10,
         bg  = "white",
         units = "cm")
  
}

process_data("fig_data_cox_dis_main_0_obs","eFigure_13_cox_dis_main_0t.png")   
process_data("fig_data_cox_dis_main_na_obs","eFigure_12_cox_dis_main_nat.png")   
process_data("fig_data_cox_dis_broad_0_obs","eFigure_10_cox_dis_broad_0t.png")   
process_data("fig_data_cox_dis_broad_na_obs","eFigure_14_cox_dis_broad_nat.png")   
process_data("fig_data_log_dis_main_0_obs","eFigure_11_log_dis_main_0t.png")   

##########################################################################################################################################
############################################################ END #########################################################################
##########################################################################################################################################
