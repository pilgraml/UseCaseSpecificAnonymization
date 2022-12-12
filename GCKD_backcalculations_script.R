# Packages 
pacman::p_load(tidyr, stringr, dplyr, openxlsx, naniar, emmeans, multcomp, 
               plyr, finalfit, ggplot2, tibble, lmtest, sandwich,
               tidyverse, tidyselect, summarytools, scales, gridExtra, 
               lubridate, eeptools, gtsummary, flextable, boot, mosaic, patchwork, rms, coxed, DescTools, PropCIs)

# Dataset
path_results = "C:/Users/User/OneDrive/Documents/PRIVAT/Charite/Forschung/Projekt Computerbasierte Anonymisierung/Titzeetal/Ergebnisse"
#path_data = "Z:/GCKD/"
path_data = "D:/BackUp GCKD"
setwd(path_data)
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_generic_k11_all.xlsx", sep = ";"))
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_generic_k11k2_all.xlsx", sep = ";"))
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_specific_k11_all.xlsx", sep = ";"))
GCKD_df1 <- as_tibble(read.xlsx("GCKD_specific_k11k2_all.xlsx", sep = ";"))
dim(GCKD_df1) ## ku_height not included 

# Data Cleansing
## Marking NA
GCKD_df1 <- GCKD_df1 %>% na_if("*")
GCKD_df1 <- GCKD_df1 %>% na_if("NULL")
GCKD_df1 <- GCKD_df1 %>% na_if("null")
## Data Type
GCKD_df1$ckd_lead <- factor(GCKD_df1$ckd_lead, levels = c("ckd_diab", "ckd_vask", "ckd_syst", "ckd_glom_prim", 
                                                          "ckd_interst", "ckd_aki", "ckd_single", "ckd_heredit", 
                                                          "ckd_obstr", "ckd_oth", "ckd_lead_uk"))
col_cat <- c("aa_stroke", "aa_myocard", "aa_hypertens", "aa_diabetes", "aa_renal", "aa_renal_stones", "aa_dialyse", 
             "aa_ntx", "smoking", "hospital", "BL_med_raas_ace", "BL_med_raas_at1", "BL_med_raas_single", 
             "BL_med_raas_double", "BL_med_caanta", "BL_med_bblocker", "BL_med_diuretic", "BL_med_diuretic_loop", 
             "BL_med_diuretic_thiazid", "BL_med_diuretic_aldost", "biopsy", "ckd_diab", "ckd_vasc", "ckd_syst", 
             "ckd_glom_prim", "ckd_interst", "ckd_heredit", "ckd_aki", "ckd_single", "ckd_obstr", "ckd_oth", 
             "ckd_uk", "diabetes", "hypertension", "valve", "coronary", "myocard", "bypass", "ptca", "cerebrovasc", 
             "stroke", "carotic_surg", "carotic_interv", "pavk", "amput", "ygraft", "pta", "cardiovasc", 
             "pavk_surgery", "incl_egfr", "education", "BL_age", "BL_ku_bmi", "BL_ku_weight", "BL_ku_height_cm")
GCKD_df1 <- GCKD_df1 %>% mutate(across(all_of(col_cat), as.character))
col_num <- c("BL_ku_sys", "BL_ku_dia", "BL_ku_map", "BL_ku_ruhepuls", "BL_creavalue", "BL_cysvalue", 
             "BL_gfr_mdrd", "BL_gfr_ckdepi", "BL_gfr_ckdepi_cys", "BL_gfr_ckdepi_creacys", "BL_uacr")
GCKD_df1 <- GCKD_df1 %>% mutate(across(all_of(col_num), as.numeric))

# BMI, weight & height back calculations
GCKD_df1_QI <- GCKD_df1 %>% select(BL_ku_bmi, BL_ku_weight, BL_ku_height_cm)
## define new columns with lower and upper bound respectively
GCKD_df1_QI[c("height_cm_low", "height_cm_up")] <- str_split_fixed(GCKD_df1_QI$BL_ku_height_cm, ", ", 2)
GCKD_df1_QI$height_cm_low <- gsub("^.", "", as.character(GCKD_df1_QI$height_cm_low))
GCKD_df1_QI$height_cm_low <- as.numeric(GCKD_df1_QI$height_cm_low)
GCKD_df1_QI$height_cm_up <- gsub(".$", "", as.character(GCKD_df1_QI$height_cm_up))
GCKD_df1_QI$height_cm_up <- as.numeric(GCKD_df1_QI$height_cm_up)
GCKD_df1_QI[c("weight_low", "weight_up")] <- str_split_fixed(GCKD_df1_QI$BL_ku_weight, ", ", 2)
GCKD_df1_QI$weight_low <- gsub("^.", "", as.character(GCKD_df1_QI$weight_low))
GCKD_df1_QI$weight_low <- as.numeric(GCKD_df1_QI$weight_low)
GCKD_df1_QI$weight_up <- gsub(".$", "", as.character(GCKD_df1_QI$weight_up))
GCKD_df1_QI$weight_up <- as.numeric(GCKD_df1_QI$weight_up)
GCKD_df1_QI[c("BMI_low", "BMI_up")] <- str_split_fixed(GCKD_df1_QI$BL_ku_bmi, ", ", 2)
GCKD_df1_QI$BMI_low <- gsub("^.", "", as.character(GCKD_df1_QI$BMI_low))
GCKD_df1_QI$BMI_low <- as.numeric(GCKD_df1_QI$BMI_low)
GCKD_df1_QI$BMI_up <- gsub(".$", "", as.character(GCKD_df1_QI$BMI_up))
GCKD_df1_QI$BMI_up <- as.numeric(GCKD_df1_QI$BMI_up)
## replace arbitrary min/max (</>) hierarchy bounds with NA
GCKD_df1_QI <- GCKD_df1_QI %>% mutate_at(c("height_cm_low"), ~na_if(., 20))
GCKD_df1_QI <- GCKD_df1_QI %>% mutate_at(c("height_cm_up"), ~na_if(., 280))
GCKD_df1_QI <- GCKD_df1_QI %>% mutate_at(c("weight_low"), ~na_if(., 0))
GCKD_df1_QI <- GCKD_df1_QI %>% mutate_at(c("weight_up"), ~na_if(., 300))
GCKD_df1_QI <- GCKD_df1_QI %>% mutate_at(c("height_cm_low"), ~na_if(., 20))
GCKD_df1_QI <- GCKD_df1_QI %>% mutate_at(c("height_cm_up"), ~na_if(., 280))
## calculation of interval bounds
GCKD_df1_QI$BMI_low_calc <- as.numeric(GCKD_df1_QI$weight_low/(GCKD_df1_QI$height_cm_up/100)^2)
GCKD_df1_QI$BMI_up_calc <- as.numeric(GCKD_df1_QI$weight_up/(GCKD_df1_QI$height_cm_low/100)^2)
GCKD_df1_QI$BMI_up_calc <- round(GCKD_df1_QI$BMI_up_calc ,digit=1)
GCKD_df1_QI$BMI_low_calc <- round(GCKD_df1_QI$BMI_low_calc ,digit=1)
GCKD_df1_QI$height_cm_low_calc <- as.numeric(sqrt(GCKD_df1_QI$weight_low/GCKD_df1_QI$BMI_up)*100)
GCKD_df1_QI$height_cm_up_calc <- as.numeric(sqrt(GCKD_df1_QI$weight_up/GCKD_df1_QI$BMI_low)*100)
GCKD_df1_QI$height_cm_low_calc <- round(GCKD_df1_QI$height_cm_low_calc ,digit=1)
GCKD_df1_QI$height_cm_up_calc <- round(GCKD_df1_QI$height_cm_up_calc ,digit=1)
GCKD_df1_QI$weight_low_calc <- as.numeric(GCKD_df1_QI$BMI_low*(GCKD_df1_QI$height_cm_low/100)^2)
GCKD_df1_QI$weight_up_calc <- as.numeric(GCKD_df1_QI$BMI_up*(GCKD_df1_QI$height_cm_up/100)^2)
GCKD_df1_QI$weight_low_calc <- round(GCKD_df1_QI$weight_low_calc ,digit=1)
GCKD_df1_QI$weight_up_calc <- round(GCKD_df1_QI$weight_up_calc ,digit=1)
## description of original and calculated intervals
GCKD_df1_QI$BMI_interv <- as.numeric(GCKD_df1_QI$BMI_up-GCKD_df1_QI$BMI_low)
GCKD_df1_QI$height_cm_interv <- as.numeric(GCKD_df1_QI$height_cm_up-GCKD_df1_QI$height_cm_low)
GCKD_df1_QI$weight_interv <- as.numeric(GCKD_df1_QI$weight_up-GCKD_df1_QI$weight_low)
summary(GCKD_df1_QI$BMI_interv)
summary(GCKD_df1_QI$height_cm_interv)
summary(GCKD_df1_QI$weight_interv)
GCKD_df1_QI$BMI_calc_interv <- as.numeric(GCKD_df1_QI$BMI_up_calc-GCKD_df1_QI$BMI_low_calc)
GCKD_df1_QI$height_cm_calc_interv <- as.numeric(GCKD_df1_QI$height_cm_up_calc-GCKD_df1_QI$height_cm_low_calc)
GCKD_df1_QI$weight_calc_interv <- as.numeric(GCKD_df1_QI$weight_up_calc-GCKD_df1_QI$weight_low_calc)
summary(GCKD_df1_QI$BMI_calc_interv)
summary(GCKD_df1_QI$height_cm_calc_interv)
summary(GCKD_df1_QI$weight_calc_interv)
## deviations from indicated (original) intervals
GCKD_df1_QI$BMI_low_diff <- as.numeric(GCKD_df1_QI$BMI_low-GCKD_df1_QI$BMI_low_calc)
GCKD_df1_QI$BMI_up_diff <- as.numeric(GCKD_df1_QI$BMI_up_calc-GCKD_df1_QI$BMI_up)
GCKD_df1_QI$height_cm_low_diff <- as.numeric(GCKD_df1_QI$height_cm_low-GCKD_df1_QI$height_cm_low_calc)
GCKD_df1_QI$height_cm_up_diff <- as.numeric(GCKD_df1_QI$height_cm_up_calc-GCKD_df1_QI$height_cm_up)
GCKD_df1_QI$weight_low_diff <- as.numeric(GCKD_df1_QI$weight_low-GCKD_df1_QI$weight_low_calc)
GCKD_df1_QI$weight_up_diff <- as.numeric(GCKD_df1_QI$weight_up_calc-GCKD_df1_QI$weight_up)
## indication of narrowing intervals (decrease in intended aggregation) for lower, upper and any bound for each QI and overall
sum(!is.na(GCKD_df1_QI$weight_low_diff) & GCKD_df1_QI$weight_low_diff<0)
sum(!is.na(GCKD_df1_QI$weight_up_diff) & GCKD_df1_QI$weight_up_diff<0)
sum((!is.na(GCKD_df1_QI$weight_low_diff) & GCKD_df1_QI$weight_low_diff<0) | 
      (!is.na(GCKD_df1_QI$weight_up_diff) & GCKD_df1_QI$weight_up_diff<0))
sum(!is.na(GCKD_df1_QI$height_cm_low_diff) & GCKD_df1_QI$height_cm_low_diff<0)
sum(!is.na(GCKD_df1_QI$height_cm_up_diff) & GCKD_df1_QI$height_cm_up_diff<0)
sum((!is.na(GCKD_df1_QI$height_cm_up_diff) & GCKD_df1_QI$height_cm_up_diff<0) | 
      (!is.na(GCKD_df1_QI$height_cm_low_diff) & GCKD_df1_QI$height_cm_low_diff<0))
sum(!is.na(GCKD_df1_QI$BMI_low_diff) & GCKD_df1_QI$BMI_low_diff<0)
sum(!is.na(GCKD_df1_QI$BMI_up_diff) & GCKD_df1_QI$BMI_up_diff<0)
sum((!is.na(GCKD_df1_QI$BMI_low_diff) & GCKD_df1_QI$BMI_low_diff<0) | 
      (!is.na(GCKD_df1_QI$BMI_up_diff) & GCKD_df1_QI$BMI_up_diff<0))
sum((!is.na(GCKD_df1_QI$weight_low_diff) & GCKD_df1_QI$weight_low_diff<0) | 
      (!is.na(GCKD_df1_QI$weight_up_diff) & GCKD_df1_QI$weight_up_diff<0) | 
      (!is.na(GCKD_df1_QI$height_cm_up_diff) & GCKD_df1_QI$height_cm_up_diff<0) | 
      (!is.na(GCKD_df1_QI$height_cm_low_diff) & GCKD_df1_QI$height_cm_low_diff<0) | 
      (!is.na(GCKD_df1_QI$BMI_low_diff) & GCKD_df1_QI$BMI_low_diff<0) | 
      (!is.na(GCKD_df1_QI$BMI_up_diff) & GCKD_df1_QI$BMI_up_diff<0))

