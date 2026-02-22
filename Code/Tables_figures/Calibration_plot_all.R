
### Load libraries ####

library(tidyverse)
library(survival)
library(lattice)
library(ggplot2)
library(Hmisc)
library(ckbplotr)
library(gridExtra)
library(ggplotify)
library(Epi)
library(ggplot2)
library(rms)
library(PredictABEL)
library(grid)
library(ggpubr)
library(readxl)

#data_obs<-"fig_data_log_cal_lp_main_0_obs"
#data_gen<-"fig_data_log_lp_cal_main_0_gen"  
#cal_plots_obs<-"Figure_3_cox_cal_lp_main_0_obs"
#cal_plots_gen<-"Figure_3_cox_cal_lp_main_0_gen" 

################################################################################################################################################
################################################################ MAIN FIGURE (RISK SCORE) ######################################################
################################################################################################################################################

cal_plots<-function(data_obs,data_gen,cal_plots_obs,cal_plots_gen){
  
  fig_data_obs<-read.csv(paste0("K:/kadoorie/Staff_Folders/JonathanC/Projects/Cancer_risk_prediction/Tables_Figures/EC/Tables/",data_obs,".csv"))
  fig_data_gen<-read_excel(paste0("K:/kadoorie/Staff_Folders/JonathanC/Projects/Cancer_risk_prediction/Tables_Figures/EC/Tables/fig_data_cal_all_gen.xlsx"),sheet=paste0(data_gen))
  
  fig_data_obs<-fig_data_obs[,c("predicted_risk_group","predicted_risk","observed_risk","study")] # Drop number in predicted risk group 
  
  fig_data_obs$data<-"Questionnaire-based data"
  fig_data_gen$data<-"Genetic data"
  
  fig_data<-rbind(fig_data_obs,fig_data_gen)
  
  ### Assign description ####    
  
  study_a<-c("kim_04",         "Kim et al.,",          "2004", "General population","")    
  study_b<-c("ros_05",         "Rosner et al.,",       "2005", "General population","") 
  study_c<-c("hcc_11",         "Hippisley-Cox et al.,","2011", "General population","") 
  study_d<-c("hcc_15",         "Hippisley-Cox et al.,","2015", "General population","") 
  study_e<-c("pfr_13",         "Pfeiffer et al.,",     "2013", "General population","")   
  study_f<-c("liS_15",         "Li (selected) et al.,","2015", "General population","")       
  study_g<-c("liF_15",         "Li (full) et al.,",    "2015", "General population","")      
  study_h<-c("urb_15",         "Urban et al.,",        "2015", "General population","") 
  study_i<-c("pie_18",         "Pieta et al.,",        "2018", "General population","") 
  study_j<-c("fun_21",         "Funston et al.,",      "2021", "General population","")  
  study_m<-c("yang_18",        "Yang et al.,",         "2017", "General population","PRS 15") 
  study_n<-c("dareng_22_S1",   "Dareng et al.,",       "2024", "General population","PRS 22")
  study_o<-c("dareng_22_S2",   "Dareng et al.,",       "2024", "General population","PRS 36")
  study_p<-c("dareng_22_S3",   "Dareng et al.,",       "2024", "General population","PRS 1,403")
  study_q<-c("dareng_22_S4",   "Dareng et al.,",       "2024", "General population","PRS 10,797")
  study_r<-c("dareng_22_S5",   "Dareng et al.,",       "2024", "General population","PRS 18,007")
  study_s<-c("dareng_22_S6",   "Dareng et al.,",       "2024", "General population","PRS 27,240")
  study_t<-c("Barnes_20_PRS22","Barnes et al.,",       "2020", "High-risk population","PRS 22")
  study_u<-c("Barnes_20_PRS30","Barnes et al.,",       "2020", "High-risk population","PRS 30")
  study_v<-c("kucken_17",      "Kuchenbaecker et al.,","2020", "High-risk population","PRS 17")
  study_w<-c("cly_16",         "Clyde et al.,",        "2016", "High-risk population","")
  study_x<-c("dite_23",        "Dite et al.,",         "2023", "High-risk population","")
  
  tab<-as.data.frame(rbind(study_a,study_b,study_c,study_d,study_e,study_f,study_g,study_h,study_i,study_j,
                           study_m,study_n,study_t,study_v,study_w,study_x))
  names(tab)<-c("study","author","date","population","model")
  tab$author_date<-paste0(tab$author," ",tab$date)
  tab$order<-c(1:16)
  
  fig_data<-merge(tab,fig_data,by="study")
  
  fig_data$predicted_risk<-as.numeric(fig_data$predicted_risk)
  fig_data$observed_risk<-as.numeric(fig_data$observed_risk)
  
  ### Study reporting order ####
  
  fig_data<-fig_data %>% arrange(desc(data),population,order)
  
  ### Calibration plot ####
  
  cal_plot<-list()
  
  for (ii in unique(fig_data$order)){
    
    cal_data<-fig_data[fig_data$order==ii,]
   
    # Max value to use for axes
    
    max_pred <- max(c(0.005,cal_data$predicted_risk,cal_data$observed_risk))
    
    plot <- ggplot(cal_data,aes(x = predicted_risk,y = observed_risk )) +
      scale_x_continuous("Predicted Risk") +
      scale_y_continuous("Observed Risk") +
      ggtitle(paste0(cal_data$author_date[1],"\n ",cal_data$model[1])) +
      annotate("segment", x = 0, y = 0, xend = max_pred, yend = max_pred, colour = "grey60", linetype = "dashed") +
      geom_smooth(method='lm', formula = y~x, se = FALSE) +
      geom_point(size = 2, shape = 19) +
      ckb_style(xlims = c(0, max_pred),
                ylims = c(0, max_pred),
                ratio = 1) +
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8),
           axis.title.x = element_text(size = 9),
           axis.title.y = element_text(size = 9))
    
    print(plot) 
    
    cal_plot[[ii]]<-plot
    
  }
  
  ### Panel of plots - obs ####
  
  make_a_plot   <- function (ii){gridExtra::grid.arrange(fix_panel(cal_plot[[ii]], height = unit(50,"mm")),nrow = 1)} 
  list_of_plots <- lapply(c(1:10), make_a_plot)
  grid_of_plots_obs <- arrangeGrob(grobs = list_of_plots, nrow = 3)
  
  plot_obs<- grid.arrange(grid_of_plots_obs,nrow = 1,heights=c(40))
  
  ggsave(paste0("K:/kadoorie/Staff_Folders/JonathanC/Projects/Cancer_risk_prediction/Tables_Figures/OC/Figures/",cal_plots_obs,".png"),
         plot_obs,
         dpi = 450,
         width = 36,
         height = 24,
         bg  = "white",
         units = "cm")     
  
  ### Panel of plots - gen ####
  
  make_a_plot   <- function (ii){gridExtra::grid.arrange(fix_panel(cal_plot[[ii]], height = unit(50,"mm")),nrow = 1)} 
  list_of_plots <- lapply(c(11:16), make_a_plot)
  grid_of_plots_gen <- arrangeGrob(grobs = list_of_plots, nrow = 2)
  
  plot_gen <- grid.arrange(grid_of_plots_gen, nrow = 1,heights=c(40))
  
  ggsave(paste0("K:/kadoorie/Staff_Folders/JonathanC/Projects/Cancer_risk_prediction/Tables_Figures/OC/Figures/",cal_plots_gen,".png"),
         plot_gen,
         dpi = 450,
         width = 26,
         height = 16,
         bg  = "white",
         units = "cm")     
  
} 


cal_plots("fig_data_cox_cal_lp_main_0_obs","fig_data_cox_lp_cal_main_0_gen","Figure_3_cox_cal_lp_main_0_obst","Figure_3_cox_cal_lp_main_0_gent")    

#########################################################################################################################################
####################################################### SUPPLEMENTARY FIGUREs (RISK SCORE) ##############################################
#########################################################################################################################################


cal_plots<-function(data_obs,data_gen,cal_plots_obs,cal_plots_gen){

fig_data_obs<-read.csv(paste0("K:/kadoorie/Staff_Folders/JonathanC/Projects/Cancer_risk_prediction/Tables_Figures/OC/Tables/",data_obs,".csv"))
fig_data_gen<-read_excel(paste0("K:/kadoorie/Staff_Folders/JonathanC/Projects/Cancer_risk_prediction/Tables_Figures/OC/Tables/fig_data_cal_all_gen.xlsx"),sheet=paste0(data_gen))

fig_data_obs<-fig_data_obs[,c("predicted_risk_group","predicted_risk","observed_risk","study")] # Drop number in predicted risk group 

fig_data_obs$data<-"Questionnaire-based data"
fig_data_gen$data<-"Genetic data"

fig_data<-rbind(fig_data_obs,fig_data_gen)

### Assign description ####    

study_a<-c("kim_04",         "Kim et al.,",          "2004", "General population","")    
study_b<-c("ros_05",         "Rosner et al.,",       "2005", "General population","") 
study_c<-c("hcc_11",         "Hippisley-Cox et al.,","2011", "General population","") 
study_d<-c("hcc_15",         "Hippisley-Cox et al.,","2015", "General population","") 
study_e<-c("pfr_13",         "Pfeiffer et al.,",     "2013", "General population","")   
study_f<-c("liS_15",         "Li (selected) et al.,","2015", "General population","")       
study_g<-c("liF_15",         "Li (full) et al.,",    "2015", "General population","")      
study_h<-c("urb_15",         "Urban et al.,",        "2015", "General population","") 
study_i<-c("pie_18",         "Pieta et al.,",        "2018", "General population","") 
study_j<-c("fun_21",         "Funston et al.,",      "2021", "General population","")  
study_m<-c("yang_18",        "Yang et al.,",         "2017", "General population","PRS 15") 
study_n<-c("dareng_22_S1",   "Dareng et al.,",       "2024", "General population","PRS 22")
study_o<-c("dareng_22_S2",   "Dareng et al.,",       "2024", "General population","PRS 36")
study_p<-c("dareng_22_S3",   "Dareng et al.,",       "2024", "General population","PRS 1,403")
study_q<-c("dareng_22_S4",   "Dareng et al.,",       "2024", "General population","PRS 10,797")
study_r<-c("dareng_22_S5",   "Dareng et al.,",       "2024", "General population","PRS 18,007")
study_s<-c("dareng_22_S6",   "Dareng et al.,",       "2024", "General population","PRS 27,240")
study_t<-c("Barnes_20_PRS22","Barnes et al.,",       "2020", "High-risk population","PRS 22")
study_u<-c("Barnes_20_PRS30","Barnes et al.,",       "2020", "High-risk population","PRS 30")
study_v<-c("kucken_17",      "Kuchenbaecker et al.,","2020", "High-risk population","PRS 17")
study_w<-c("cly_16",         "Clyde et al.,",        "2016", "High-risk population","")
study_x<-c("dite_23",        "Dite et al.,",         "2023", "High-risk population","")

tab<-as.data.frame(rbind(study_a,study_b,study_c,study_d,study_e,study_f,study_g,study_h,study_i,study_j,
                         study_m,study_n,study_o,study_p,study_q,study_r,study_s,study_t,study_u,study_v,study_w,study_x))
names(tab)<-c("study","author","date","population","model")
tab$author_date<-paste0(tab$author," ",tab$date)
tab$order<-c(1:22)

fig_data<-merge(tab,fig_data,by="study")

fig_data$predicted_risk<-as.numeric(fig_data$predicted_risk)
fig_data$observed_risk<-as.numeric(fig_data$observed_risk)

### Study reporting order ####

fig_data<-fig_data %>% arrange(desc(data),population,order)

### Calibration plot ####

cal_plot<-list()

for (ii in unique(fig_data$order)){
  
  cal_data<-fig_data[fig_data$order==ii,]

# Max value to use for axes

max_pred <- max(c(0.005,cal_data$predicted_risk,cal_data$observed_risk))

plot <- ggplot(cal_data,aes(x = predicted_risk,y = observed_risk )) +
  scale_x_continuous("Predicted Risk") +
  scale_y_continuous("Observed Risk") +
  ggtitle(paste0(cal_data$author_date[1],"\n ",cal_data$model[1])) +
  annotate("segment", x = 0, y = 0, xend = max_pred, yend = max_pred, colour = "grey60", linetype = "dashed") +
  geom_smooth(method='lm', formula = y~x, se = FALSE) +
  geom_point(size = 2, shape = 19) +
  ckb_style(xlims = c(0, max_pred),
            ylims = c(0, max_pred),
            ratio = 1) + 
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))
  

print(plot) 

cal_plot[[ii]]<-plot

}

### Panel of plots - obs ####
   
   make_a_plot   <- function (ii){gridExtra::grid.arrange(fix_panel(cal_plot[[ii]], height = unit(50,"mm")),nrow = 1)} 
   list_of_plots <- lapply(c(1:10), make_a_plot)
   grid_of_plots_obs <- arrangeGrob(grobs = list_of_plots, nrow = 3)
  
   plot_obs<- grid.arrange(grid_of_plots_obs,nrow = 1,heights=c(40))
   
   ggsave(paste0("K:/kadoorie/Staff_Folders/JonathanC/Projects/Cancer_risk_prediction/Tables_Figures/OC/Figures/",cal_plots_obs,".png"),
          plot_obs,
          dpi = 450,
          width = 36,
          height = 24,
          bg  = "white",
          units = "cm")     
 
### Panel of plots - gen ####
   
   make_a_plot   <- function (ii){gridExtra::grid.arrange(fix_panel(cal_plot[[ii]], height = unit(50,"mm")),nrow = 1)} 
   list_of_plots <- lapply(c(11:22), make_a_plot)
   grid_of_plots_gen <- arrangeGrob(grobs = list_of_plots, nrow = 3)
 
   plot_gen <- grid.arrange(grid_of_plots_gen, nrow = 1,heights=c(40))

   ggsave(paste0("K:/kadoorie/Staff_Folders/JonathanC/Projects/Cancer_risk_prediction/Tables_Figures/OC/Figures/",cal_plots_gen,".png"),
          plot_gen,
          dpi = 450,
          width = 36,
          height = 24,
          bg  = "white",
          units = "cm")     
 
} 

cal_plots("fig_data_cox_cal_lp_main_na_obs","fig_data_cox_lp_cal_main_na_gen","eFigure_3_cox_cal_lp_main_na_obst","eFigure_3_cox_cal_lp_main_na_gent") 
cal_plots("fig_data_cox_cal_lp_broad_na_obs","fig_data_cox_lp_cal_broad_na_ge","eFigure_5_cox_cal_lp_broad_na_obst","eFigure_5_cox_cal_lp_broad_na_gent")    
cal_plots("fig_data_cox_cal_lp_broad_0_obs","fig_data_cox_lp_cal_broad_0_gen","eFigure_7_cox_cal_lp_broad_0_obst","eFigure_7_cox_cal_lp_broad_0_gent")    
cal_plots("fig_data_log_cal_lp_main_0_obs","fig_data_log_lp_cal_main_0_gen","eFigure_9_log_cal_lp_main_0_obst","eFigure_9_log_cal_lp_main_0_gent")    

################################################################################################################################
##################################################### ALL FIGURES (RECALIBRATION) ##############################################
################################################################################################################################

#data_obs<-"fig_data_log_cal_main_0_obs"
#cal_plots_obs<-"cal_plot_cox_re_main_0_obs"  

cal_plots<-function(data_obs,cal_plots_obs){
  
  fig_data<-read.csv(paste0("K:/kadoorie/Staff_Folders/JonathanC/Projects/Cancer_risk_prediction/Tables_Figures/OC/Tables/",data_obs,".csv"))
  fig_data<-fig_data[,2:5]  
  fig_data<-subset(fig_data, !(study %in% c("hib_22")))
  
  ### Assign description ####    
  
  study_a<-c("kim_04",         "Kim et al.,",          "2004", "General population")    
  study_b<-c("ros_05",         "Rosner et al.,",       "2005", "General population") 
  study_c<-c("hcc_11",         "Hippisley-Cox et al.,","2011", "General population") 
  study_d<-c("hcc_15",         "Hippisley-Cox et al.,","2015", "General population") 
  study_e<-c("pfr_13",         "Pfeiffer et al.,",     "2013", "General population")   
  study_f<-c("liS_15",         "Li (selected) et al.,","2015", "General population")       
  study_g<-c("liF_15",         "Li (full) et al.,",    "2015", "General population")      
  study_h<-c("urb_15",         "Urban et al.,",        "2015", "General population") 
  study_i<-c("pie_18",         "Pieta et al.,",        "2018", "General population") 
  study_j<-c("fun_21",         "Funston et al.,",      "2021", "General population")  

  
  tab<-as.data.frame(rbind(study_a,study_b,study_c,study_d,study_e,study_f,study_g,study_h,study_i,study_j))
  names(tab)<-c("study","author","date","population")
  tab$author_date<-paste0(tab$author," ",tab$date)
  tab$order<-c(1:10)
  
  fig_data<-merge(tab,fig_data,by="study")
  
  fig_data$predicted_risk<-as.numeric(fig_data$predicted_risk)
  fig_data$observed_risk<-as.numeric(fig_data$observed_risk)
  
  ### Study reporting order ####
  
  fig_data<-fig_data %>% arrange(order)
  
  ### Calibration plot ####
  
  cal_plot<-list()
  
  for (ii in unique(fig_data$order)){
    
    cal_data<-fig_data[fig_data$order==ii,]

    # Max value to use for axes
    
    max_pred <- max(c(0.005,cal_data$predicted_risk,cal_data$observed_risk))
    
    plot <- ggplot(cal_data,aes(x = predicted_risk,y = observed_risk )) +
      scale_x_continuous("Predicted Risk") +
      scale_y_continuous("Observed Risk") +
      ggtitle(cal_data$author_date[1]) +
      annotate("segment", x = 0, y = 0, xend = max_pred, yend = max_pred, colour = "grey60", linetype = "dashed") +
      geom_smooth(method='lm', formula = y~x, se = FALSE) +
      geom_point(size = 2, shape = 19) +
      ckb_style(xlims = c(0, max_pred),
                ylims = c(0, max_pred),
                ratio = 1) +
      theme(axis.text.x = element_text(size = 9),
            axis.text.y = element_text(size = 9),
            axis.title.x = element_text(size = 10),
            axis.title.y = element_text(size = 10))
    
    print(plot) 
    
    cal_plot[[ii]]<-plot
    
  }
  
  ### Panel of plots - obs ####
  
  make_a_plot   <- function (ii){gridExtra::grid.arrange(fix_panel(cal_plot[[ii]], height = unit(50,"mm")),nrow = 1)} 
  list_of_plots <- lapply(c(1:12), make_a_plot)
  grid_of_plots_obs <- arrangeGrob(grobs = list_of_plots, nrow = 3)
  
  plot_obs<- grid.arrange(grid_of_plots_obs,nrow = 1,heights=c(40))
  
  ggsave(paste0("K:/kadoorie/Staff_Folders/JonathanC/Projects/Cancer_risk_prediction/Tables_Figures/OC/Figures/",cal_plots_obs,".png"),
         plot_obs,
         dpi = 450,
         width = 36,
         height = 24,
         bg  = "white",
         units = "cm")     

} 

#cal_plots("fig_data_cox_cal_main_0_obs","cal_plot_cox_re_main_0_obs")    
cal_plots("fig_data_cox_cal_main_na_obs","cal_plot_cox_re_main_na_obs.obs") 
#cal_plots("fig_data_cox_cal_broad_0_obs","cal_plot_cox_re_broad_0_obs")
cal_plots("fig_data_cox_cal_broad_na_obs","cal_plot_cox_re_broad_na_obs")    
#cal_plots("fig_data_log_cal_main_0_obs","cal_plot_log_re_main_0_obs")  


  
 