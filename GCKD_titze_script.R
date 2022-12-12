# Packages 
pacman::p_load(tidyr, stringr, dplyr, openxlsx, naniar, emmeans, multcomp, 
               plyr, finalfit, ggplot2, tibble, lmtest, sandwich,
               tidyverse, tidyselect, summarytools, scales, gridExtra, 
               lubridate, eeptools, gtsummary, flextable, boot, mosaic, patchwork, rms, coxed, DescTools)

# Dataset
path_results = "C:/Users/User/OneDrive/Documents/PRIVAT/Charite/Forschung/Projekt Computerbasierte Anonymisierung/Titzeetal/Ergebnisse"
path_data = "Z:/GCKD/"
path_data = "D:/BackUp GCKD"
setwd(path_data)
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_df1_origin.xlsx", sep = ";"))
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_df1_generic_k2s10.xlsx", sep = ";"))
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_df1_generic_k5s10.xlsx", sep = ";"))
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_df1_generic_k11s10.xlsx", sep = ";"))
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_df1_specific_k2s10.xlsx", sep = ";"))
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_df1_specific_k5s10.xlsx", sep = ";"))
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_df1_specific_k11s10.xlsx", sep = ";"))
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_generic_k11_bmi.xlsx", sep = ";"))
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_generic_k11_height.xlsx", sep = ";"))
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_generic_k11_weight.xlsx", sep = ";"))
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_generic_k2k11_bmi.xlsx", sep = ";"))
GCKD_df1 <- as_tibble(read.xlsx("GCKD_generic_k2k11_height.xlsx", sep = ";"))
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_generic_k2k11_weight.xlsx", sep = ";"))
dim(GCKD_df1)

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
             "pavk_surgery", "incl_egfr", "education")
GCKD_df1 <- GCKD_df1 %>% mutate(across(all_of(col_cat), as.character))
col_num <- c("BL_ku_sys", "BL_ku_dia", "BL_ku_map", "BL_ku_ruhepuls", "BL_creavalue", "BL_cysvalue", 
             "BL_gfr_mdrd", "BL_gfr_ckdepi", "BL_gfr_ckdepi_cys", "BL_gfr_ckdepi_creacys", "BL_uacr")
GCKD_df1 <- GCKD_df1 %>% mutate(across(all_of(col_num), as.numeric))


# Distribution of Quasi-Identifiers
## Summary
GCKD_df1_QI <- GCKD_df1 %>% select(BL_age, dem_sex, BL_ku_height_cm, BL_ku_weight, BL_ku_bmi, biopsy)
theme_gtsummary_language("en", big.mark = "")
GCKD_df1_QI_table_1 <-
  GCKD_df1_QI %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean}",
                     all_categorical() ~ "{n}"),
    digits = all_continuous() ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_0 = "n or mean") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_QI_table_2 <-
  GCKD_df1_QI %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{sd}",
                     all_categorical() ~ "{p}"),
    digits = ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_0 = "SD or %") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_QI_table <-
  tbl_merge(tbls = list(GCKD_df1_QI_table_1, GCKD_df1_QI_table_2)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_QI_table %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_df1_QI_table.html", 
             path = path_results)
## Figures
### Figure settings: fill="#73A1AA" in anonymized datasets, fill="#B83674" in original dataset
### Changing factor levels for anonymized datasets
### Histograms
GCKD_df1_QI_num <- select_if(GCKD_df1_QI, is.numeric)
for(i in 1:ncol((GCKD_df1_QI_num))) {
  x = colnames(GCKD_df1_QI_num)[i]
  Fig <- ggplot() + 
    geom_histogram(aes_string(x), data = GCKD_df1_QI_num, binwidth=1, colour="white", fill="#73A1AA")
  print(Fig)
}
### Barplots: levels depending on hierarchial level
GCKD_df1_QI_cat <- select_if(GCKD_df1_QI, is.character)
GCKD_df1_QI_cat$BL_ku_weight <- factor(GCKD_df1_QI_cat$BL_ku_weight, levels = c("[40, 80[", "[80, 120[", "[120, 160["))
GCKD_df1_QI_cat$BL_ku_bmi <- factor(GCKD_df1_QI_cat$BL_ku_bmi, levels = c("[15, 18.5[", "[18.5, 25[", "[25, 30[", "[30, 35[", "[35, 40[", "[40, 70["))
for(i in 1:ncol((GCKD_df1_QI_cat))) {
  x = colnames(GCKD_df1_QI_cat)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_QI_cat, !is.na(GCKD_df1_QI_cat[i])), colour="white", fill="#73A1AA")
  print(Fig)
  }

# BMI risk calculations
## define new columns with lower and upper bound respectively
GCKD_df1_QI[c("BMI_low", "BMI_up")] <- str_split_fixed(GCKD_df1_QI$BL_ku_bmi, ", ", 2)
GCKD_df1_QI$BMI_low <- gsub("^.", "", as.character(GCKD_df1_QI$BMI_low))
GCKD_df1_QI$BMI_low <- as.numeric(GCKD_df1_QI$BMI_low)
GCKD_df1_QI$BMI_up <- gsub(".$", "", as.character(GCKD_df1_QI$BMI_up))
GCKD_df1_QI$BMI_up <- as.numeric(GCKD_df1_QI$BMI_up)
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
## replace min/max (</>) hierarchy bounds with NA (generic setting, in specific setting actual bounds available)
GCKD_df1_QI <- GCKD_df1_QI %>% mutate_at(c("BMI_low"), ~na_if(., 5))
GCKD_df1_QI <- GCKD_df1_QI %>% mutate_at(c("BMI_up"), ~na_if(., 250))
GCKD_df1_QI <- GCKD_df1_QI %>% mutate_at(c("height_cm_low"), ~na_if(., 20))
GCKD_df1_QI <- GCKD_df1_QI %>% mutate_at(c("height_cm_up"), ~na_if(., 280))
GCKD_df1_QI <- GCKD_df1_QI %>% mutate_at(c("weight_low"), ~na_if(., 0))
GCKD_df1_QI <- GCKD_df1_QI %>% mutate_at(c("weight_up"), ~na_if(., 300))
## calculate measures out of indicated intervals
GCKD_df1_QI$BMI_low_calc <- as.numeric(GCKD_df1_QI$weight_low/(GCKD_df1_QI$height_cm_up/100)^2)
GCKD_df1_QI$BMI_up_calc <- as.numeric(GCKD_df1_QI$weight_up/(GCKD_df1_QI$height_cm_low/100)^2)
GCKD_df1_QI$height_cm_low_calc <- as.numeric(sqrt(GCKD_df1_QI$weight_low/GCKD_df1_QI$BMI_up)*100)
GCKD_df1_QI$height_cm_up_calc <- as.numeric(sqrt(GCKD_df1_QI$weight_up/GCKD_df1_QI$BMI_low)*100)
GCKD_df1_QI$weight_low_calc <- as.numeric(GCKD_df1_QI$BMI_low*(GCKD_df1_QI$height_cm_low/100)^2)
GCKD_df1_QI$weight_up_calc <- as.numeric(GCKD_df1_QI$BMI_up*(GCKD_df1_QI$height_cm_up/100)^2)
## compare calculated and indicated measures: negative values represent smaller intervalls than initially intended (= risk)
GCKD_df1_QI$BMI_low_diff <- as.numeric(GCKD_df1_QI$BMI_low-GCKD_df1_QI$BMI_low_calc)
GCKD_df1_QI$BMI_up_diff <- as.numeric(GCKD_df1_QI$BMI_up_calc-GCKD_df1_QI$BMI_up)
GCKD_df1_QI$height_cm_low_diff <- as.numeric(GCKD_df1_QI$height_cm_low-GCKD_df1_QI$height_cm_low_calc)
GCKD_df1_QI$height_cm_up_diff <- as.numeric(GCKD_df1_QI$height_cm_up_calc-GCKD_df1_QI$height_cm_up)
GCKD_df1_QI$weight_low_diff <- as.numeric(GCKD_df1_QI$weight_low-GCKD_df1_QI$weight_low_calc)
GCKD_df1_QI$weight_up_diff <- as.numeric(GCKD_df1_QI$weight_up_calc-GCKD_df1_QI$weight_up)
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
GCKD_df1_QI$BMI_calc_interv <- as.numeric(GCKD_df1_QI$BMI_up_calc-GCKD_df1_QI$BMI_low_calc)
summary(GCKD_df1_QI$BMI_calc_interv)
GCKD_df1_QI <- GCKD_df1_QI %>% 
  mutate(BMI_calc = ifelse(BMI_up_calc < 18.5, "< 18.5",
                           ifelse(BMI_up_calc < 25 & BMI_low_calc >= 18.5, "18.5-25",
                                  ifelse(BMI_up_calc < 30 & BMI_low_calc >= 25, "25-30",
                                         ifelse(BMI_up_calc < 35 & BMI_low_calc >= 30, "30-35", 
                                                ifelse(BMI_up_calc < 40 & BMI_low_calc >= 35, "35-40", 
                                                       ifelse(BMI_low_calc >= 40, ">40", 
                                                              ifelse(BMI_up_calc < 25, "< 25", 
                                                                     ifelse(BMI_up_calc < 30 & BMI_low_calc >= 18.5, "18.5-30",
                                                                            ifelse(BMI_up_calc < 35 & BMI_low_calc >= 25, "25-35", 
                                                                                   ifelse(BMI_up_calc < 40 & BMI_low_calc >= 30, "30-40", 
                                                                                          ifelse(BMI_low_calc >= 35, "> 35", 
                                                                                                 ifelse(BMI_up_calc < 30, "< 30", 
                                                                                                        ifelse(BMI_up_calc < 35 & BMI_low_calc >= 18.5, "18.5-35",
                                                                                                               ifelse(BMI_up_calc < 40 & BMI_low_calc >= 25, "25-40",
                                                                                                                      ifelse(BMI_low_calc >= 30, "> 30",
                                                                                                                             ifelse(BMI_up_calc < 35, "< 35", 
                                                                                                                                    ifelse(BMI_up_calc < 40 & BMI_low_calc >= 18.5, "18.5-40",
                                                                                                                                    ifelse(BMI_low_calc >= 25, "> 25", 
                                                                                                                                           ifelse(BMI_up_calc < 40, "< 40", 
                                                                                                                                                  ifelse(BMI_low_calc >= 18.5, "> 18.5", 
                                                                                                                                                         ifelse(BMI_up_calc < 50, "< 50", "other"))))))))))))))))))))))
table(GCKD_df1_QI$BMI_calc, useNA = "always")

GCKD_df1_QI$height_cm_calc_interv <- as.numeric(GCKD_df1_QI$height_cm_up_calc-GCKD_df1_QI$height_cm_low_calc)
summary(GCKD_df1_QI$height_cm_calc_interv)
GCKD_df1_QI <- GCKD_df1_QI %>% 
  mutate(height_cm_calc = ifelse(height_cm_up_calc < 160, "< 160",
                           ifelse(height_cm_up_calc < 170 & height_cm_low_calc >= 160, "160-170",
                                  ifelse(height_cm_up_calc < 180 & height_cm_low_calc >= 170, "170-180",
                                         ifelse(height_cm_up_calc < 190 & height_cm_low_calc >= 180, "180-190", 
                                                ifelse(height_cm_up_calc < 200 & height_cm_low_calc >= 190, "190-200",
                                                       ifelse(height_cm_up_calc < 210 & height_cm_low_calc >= 200, "200-210",
                                                              ifelse(height_cm_low_calc >= 210, "> 210", 
                                                                     ifelse(height_cm_up_calc < 180 & height_cm_low_calc >= 160, "160-180",
                                                                            ifelse(height_cm_up_calc < 200 & height_cm_low_calc >= 180, "180-200",
                                                                                   ifelse(height_cm_low_calc >= 200, "> 200",
                                                                                          ifelse(height_cm_up_calc < 200 & height_cm_low_calc >= 160, "160-200",
                                                                                                 ifelse(height_cm_up_calc < 210 & height_cm_low_calc >= 160, "160-210",
                                                                                                        ifelse(height_cm_up_calc < 170, "< 170",
                                                                                                               ifelse(height_cm_low_calc >= 190, ">= 190",
                                                                                                                      ifelse(height_cm_low_calc >= 180, ">= 180",
                                                                                                                      ifelse(height_cm_up_calc < 180, "< 180",
                                                                                                                             ifelse(height_cm_up_calc < 190, "< 190", 
                                                                                                                                    ifelse(height_cm_up_calc < 200, "< 200",
                                                                                                                                           ifelse(height_cm_up_calc < 210, "< 210",
                                                                                                                                                  ifelse(height_cm_low_calc >= 170, ">= 170", 
                                                                                                                                                         ifelse(height_cm_low_calc >= 160, ">= 160", 
                                                                                                                                                                ifelse(height_cm_low_calc >= 150, ">= 150","other")))))))))))))))))))))))
table(GCKD_df1_QI$height_cm_calc, useNA = "always")                                                                                                            
                    
GCKD_df1_QI$weight_calc_interv <- as.numeric(GCKD_df1_QI$weight_up_calc-GCKD_df1_QI$weight_low_calc)
summary(GCKD_df1_QI$weight_calc_interv)

GCKD_df1_QI <- GCKD_df1_QI %>% 
  mutate(weight_calc = ifelse(weight_up_calc < 60, "< 60",
                                 ifelse(weight_up_calc < 60 & weight_low_calc >= 70, "60-70",
                                        ifelse(weight_up_calc < 80 & weight_low_calc >= 70, "70-80",
                                               ifelse(weight_up_calc < 90 & weight_low_calc >= 80, "80-90", 
                                                      ifelse(weight_up_calc < 100 & weight_low_calc >= 90, "90-100",
                                                             ifelse(weight_up_calc < 110 & weight_low_calc >= 100, "100-110",
                                                                    ifelse(weight_up_calc < 120 & weight_low_calc >= 110, "110-120",
                                                                           ifelse(weight_low_calc >= 120, "> 120",
                                                                                  ifelse(weight_up_calc < 80 & weight_low_calc >= 60, "60-80",
                                                                                         ifelse(weight_up_calc < 100 & weight_low_calc >= 80, "80-100",
                                                                                                ifelse(weight_up_calc < 120 & weight_low_calc >= 100, "100-120",
                                                                                                       ifelse(weight_up_calc < 100 & weight_low_calc >= 60, "60-100",
                                                                                                              ifelse(weight_up_calc < 120 & weight_low_calc >= 60, "60-120",
                                                                                                                     ifelse(weight_up_calc < 70, "< 70",
                                                                                                                     ifelse(weight_up_calc < 80, "< 80",
                                                                                                                            ifelse(weight_up_calc < 90, "< 90",
                                                                                                                                   ifelse(weight_up_calc < 100, "< 100",
                                                                                                                                          ifelse(weight_up_calc < 110, "< 110",
                                                                                                                                          ifelse(weight_low_calc >= 110, "> 110",
                                                                                                                                                 ifelse(weight_low_calc >= 100, "> 100",
                                                                                                                                                        ifelse(weight_low_calc >= 90, "> 90",
                                                                                                                                                               ifelse(weight_low_calc >= 80, "> 80",
                                                                                                                                                                      ifelse(weight_low_calc >= 70, "> 70", "other"))))))))))))))))))))))))
table(GCKD_df1_QI$weight_calc, useNA = "always")                                                                                   

# Titze et al 
## Table 1
var_table_1 <- c("BL_age", "dem_sex", "aa_stroke", "aa_myocard", "aa_hypertens", "aa_diabetes", "aa_renal", 
                 "aa_renal_stones", "aa_dialyse", "aa_ntx", "smoking", "hospital", "BL_ku_height_cm","BL_ku_weight", 
                 "BL_ku_bmi", "BL_ku_sys", "BL_ku_dia", "BL_ku_map", "BL_ku_ruhepuls", "BL_creavalue", "BL_cysvalue",
                 "BL_gfr_mdrd", "BL_uacr", "BL_med_raas_ace", "BL_med_raas_at1", "BL_med_raas_double", 
                 "BL_med_raas_single", "BL_med_diuretic", "BL_med_diuretic_thiazid", "BL_med_diuretic_aldost", 
                 "BL_med_diuretic_loop", "BL_med_caanta", "BL_med_bblocker", "biopsy", "diabetes")
theme_gtsummary_language("en", big.mark = "")
GCKD_df1_table_1a <-
  GCKD_df1 %>%
  select(all_of(var_table_1)) %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean}",
                     all_categorical() ~ "{n}"),
    digits = all_continuous() ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_0 = "n or mean") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_1b <-
  GCKD_df1 %>%
  select(all_of(var_table_1)) %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{sd}",
                     all_categorical() ~ "{p}"),
    digits = ~ 1,
    missing_text = "(Missing)") %>%
  add_ci(statistic=list(all_categorical() ~ "{conf.low}-{conf.high}", all_continuous() ~ "{conf.low}-{conf.high}"),
         style_fun=list(all_categorical() ~ purrr::partial(style_sigfig, digits=3, scale = 100), 
                        all_continuous() ~ purrr::partial(style_sigfig, digits=3))) %>%
  modify_header(label = "Variable", stat_0 = "SD or %") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_1 <-
  tbl_merge(tbls = list(GCKD_df1_table_1a, GCKD_df1_table_1b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_table_1 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_titze_table_1.html", 
             path = path_results)

# adjustments depending on hierarchical level
if(is.character(GCKD_df1$BL_ku_bmi) == TRUE){
  print(table(GCKD_df1$BL_ku_bmi == "[30, 35[" | GCKD_df1$BL_ku_bmi == "[35, 40[" | GCKD_df1$BL_ku_bmi == "[40, 70[",  useNA = "always"))  
  print(prop.table(table(GCKD_df1$BL_ku_bmi == "[30, 35[" | GCKD_df1$BL_ku_bmi == "[35, 40[" | GCKD_df1$BL_ku_bmi == "[40, 70["))%>% '*'(100) %>% round(1))
  print(table(GCKD_df1$BL_ku_bmi == "[25, 30[",  useNA = "always"))
  print(prop.table(table(GCKD_df1$BL_ku_bmi == "[25, 30["))%>% '*'(100) %>% round(1))
  print(table(GCKD_df1$BL_ku_bmi == "[15, 18.5[" | GCKD_df1$BL_ku_bmi == "[18.5, 25[",  useNA = "always"))
  print(prop.table(table(GCKD_df1$BL_ku_bmi == "[15, 18.5[" | GCKD_df1$BL_ku_bmi == "[18.5, 25[")) %>% '*'(100) %>% round(1))
} else{
  print(table(GCKD_df1$BL_ku_bmi > 30,  useNA = "always"))
  print(prop.table(table(GCKD_df1$BL_ku_bmi > 30))%>% '*'(100) %>% round(1))
  print(table(GCKD_df1$BL_ku_bmi <= 30 & GCKD_df1$BL_ku_bmi > 25,  useNA = "always"))
  print(prop.table(table(GCKD_df1$BL_ku_bmi <= 30 & GCKD_df1$BL_ku_bmi > 25))%>% '*'(100) %>% round(1))
  print(table(GCKD_df1$BL_ku_bmi <= 25,  useNA = "always"))
  print(prop.table(table(GCKD_df1$BL_ku_bmi <= 25))%>% '*'(100) %>% round(1))
}
table(GCKD_df1$BL_ku_sys < 130 & GCKD_df1$BL_ku_dia < 80,  useNA = "always")
prop.table(table(GCKD_df1$BL_ku_sys < 130 & GCKD_df1$BL_ku_dia < 80))%>% '*'(100) %>% round(1)
table(GCKD_df1$BL_ku_sys < 140 & GCKD_df1$BL_ku_dia < 90,  useNA = "always")
prop.table(table(GCKD_df1$BL_ku_sys < 140 & GCKD_df1$BL_ku_dia < 90))%>% '*'(100) %>% round(1)
table(GCKD_df1$BL_gfr_mdrd >= 60,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_mdrd >= 60)) %>% '*'(100) %>% round(1)
table(GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60)) %>% '*'(100) %>% round(1)
table(GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45)) %>% '*'(100) %>% round(1)
table(GCKD_df1$BL_gfr_mdrd < 30,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_mdrd < 30)) %>% '*'(100) %>% round(1)
summary(GCKD_df1$BL_uacr)
MedianCI(GCKD_df1$BL_uacr, na.rm=TRUE)
table(GCKD_df1$BL_uacr < 30,  useNA = "always")
prop.table(table(GCKD_df1$BL_uacr < 30)) %>% '*'(100) %>% round(1)
table(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300,  useNA = "always")
prop.table(table(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300)) %>% '*'(100) %>% round(1)
table(GCKD_df1$BL_uacr > 300,  useNA = "always")
prop.table(table(GCKD_df1$BL_uacr > 300)) %>% '*'(100) %>% round(1)
### Subsetting female
GCKD_df1_fem <- GCKD_df1 %>% subset(dem_sex == "Female")
theme_gtsummary_language("en", big.mark = "")
table(GCKD_df1_fem$diabetes, useNA = "always")
GCKD_df1_fem_table_1a <-
  GCKD_df1_fem %>%
  select(all_of(var_table_1)) %>%
  tbl_summary(
    by = diabetes,
    statistic = list(all_continuous() ~ "{mean}",
                     all_categorical() ~ "{n}"),
    digits = all_continuous() ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_1 = "Diabetics; n or mean", stat_2 = "Non-diabetics; n or mean") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_fem_table_1b <-
  GCKD_df1_fem %>%
  select(all_of(var_table_1)) %>%
  tbl_summary(
    by = diabetes,
    statistic = list(all_continuous() ~ "{sd}",
                     all_categorical() ~ "{p}"),
    digits = ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_1 = "Diabetics; SD or %", stat_2 = "Non-diabetics; SD or %") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_fem_table_1 <-
  tbl_merge(tbls = list(GCKD_df1_fem_table_1a, GCKD_df1_fem_table_1b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_fem_table_1 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_titze_table_1_fem.html", 
             path = path_results)
if(is.character(GCKD_df1_fem$BL_ku_bmi) == TRUE){
  print(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi == "[30, 35[" | GCKD_df1_fem$BL_ku_bmi == "[35, 40[" | GCKD_df1_fem$BL_ku_bmi == "[40, 70[",  useNA = "always"))  
  print(prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi == "[30, 35[" | GCKD_df1_fem$BL_ku_bmi == "[35, 40[" | GCKD_df1_fem$BL_ku_bmi == "[40, 70["), 1) %>% '*'(100) %>% round(1))
  print(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi == "[25, 30[",  useNA = "always"))
  print(prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi == "[25, 30["), 1) %>% '*'(100) %>% round(1))
  print(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi == "[15, 18.5[" | GCKD_df1_fem$BL_ku_bmi == "[18.5, 25[",  useNA = "always"))
  print(prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi == "[15, 18.5[" | GCKD_df1_fem$BL_ku_bmi == "[18.5, 25["), 1) %>% '*'(100) %>% round(1))
  }else{
  print(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi > 30,  useNA = "always"))
  print(prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi > 30), 1) %>% '*'(100) %>% round(1))
  print(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi <= 30 & GCKD_df1_fem$BL_ku_bmi > 25,  useNA = "always"))
  print(prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi <= 30 & GCKD_df1_fem$BL_ku_bmi > 25), 1) %>% '*'(100) %>% round(1))
  print(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi <= 25,  useNA = "always"))
  print(prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi <= 25), 1) %>% '*'(100) %>% round(1))
  }
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_sys < 130 & GCKD_df1_fem$BL_ku_dia < 80,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_sys < 130 & GCKD_df1_fem$BL_ku_dia < 80), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_sys < 140 & GCKD_df1_fem$BL_ku_dia < 90,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_sys < 140 & GCKD_df1_fem$BL_ku_dia < 90), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd >= 60,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd >= 60), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd >= 45 & GCKD_df1_fem$BL_gfr_mdrd < 60,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd >= 45 & GCKD_df1_fem$BL_gfr_mdrd < 60), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd >= 30 & GCKD_df1_fem$BL_gfr_mdrd < 45,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd >= 30 & GCKD_df1_fem$BL_gfr_mdrd < 45), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd < 30,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd < 30), 1) %>% '*'(100) %>% round(1)
GCKD_df1_fem %>% group_by(diabetes) %>%
  summarise_at(vars(BL_uacr), list(~ round(quantile(., na.rm=TRUE), 1)))
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_uacr < 30,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_uacr < 30), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_uacr >= 30 & GCKD_df1_fem$BL_uacr <= 300,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_uacr >= 30 & GCKD_df1_fem$BL_uacr <= 300), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_uacr > 300,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_uacr > 300), 1) %>% '*'(100) %>% round(1)
### Subsetting male
GCKD_df1_male <- GCKD_df1 %>% subset(dem_sex == "Male")
theme_gtsummary_language("en", big.mark = "")
table(GCKD_df1_male$diabetes, useNA = "always")
GCKD_df1_male_table_1a <-
  GCKD_df1_male %>%
  select(all_of(var_table_1)) %>%
  tbl_summary(
    by = diabetes,
    statistic = list(all_continuous() ~ "{mean}",
                     all_categorical() ~ "{n}"),
    digits = all_continuous() ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_1 = "Diabetics; n or mean", stat_2 = "Non-diabetics; n or mean") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_male_table_1b <-
  GCKD_df1_male %>%
  select(all_of(var_table_1)) %>%
  tbl_summary(
    by = diabetes,
    statistic = list(all_continuous() ~ "{sd}",
                     all_categorical() ~ "{p}"),
    digits = ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_1 = "Diabetics; SD or %", stat_2 = "Non-diabetics; SD or %") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_male_table_1 <-
  tbl_merge(tbls = list(GCKD_df1_male_table_1a, GCKD_df1_male_table_1b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_male_table_1 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_titze_table_1_male.html", 
             path = path_results)
if(is.character(GCKD_df1_male$BL_ku_bmi) == TRUE){
  print(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi == "[30, 35[" | GCKD_df1_male$BL_ku_bmi == "[35, 40[" | GCKD_df1_male$BL_ku_bmi == "[40, 70[",  useNA = "always"))  
  print(prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi == "[30, 35[" | GCKD_df1_male$BL_ku_bmi == "[35, 40[" | GCKD_df1_male$BL_ku_bmi == "[40, 70["), 1) %>% '*'(100) %>% round(1))
  print(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi == "[25, 30[",  useNA = "always"))
  print(prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi == "[25, 30["), 1) %>% '*'(100) %>% round(1))
  print(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi == "[15, 18.5[" | GCKD_df1_male$BL_ku_bmi == "[18.5, 25[",  useNA = "always"))
  print(prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi == "[15, 18.5[" | GCKD_df1_male$BL_ku_bmi == "[18.5, 25["), 1) %>% '*'(100) %>% round(1))
}else{
  print(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi > 30,  useNA = "always"))
  print(prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi > 30), 1) %>% '*'(100) %>% round(1))
  print(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi <= 30 & GCKD_df1_male$BL_ku_bmi > 25,  useNA = "always"))
  print(prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi <= 30 & GCKD_df1_male$BL_ku_bmi > 25), 1) %>% '*'(100) %>% round(1))
  print(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi <= 25,  useNA = "always"))
  print(prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi <= 25), 1) %>% '*'(100) %>% round(1))
}
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_sys < 130 & GCKD_df1_male$BL_ku_dia < 80,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_sys < 130 & GCKD_df1_male$BL_ku_dia < 80), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_sys < 140 & GCKD_df1_male$BL_ku_dia < 90,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_sys < 140 & GCKD_df1_male$BL_ku_dia < 90), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd >= 60,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd >= 60), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd >= 45 & GCKD_df1_male$BL_gfr_mdrd < 60,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd >= 45 & GCKD_df1_male$BL_gfr_mdrd < 60), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd >= 30 & GCKD_df1_male$BL_gfr_mdrd < 45,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd >= 30 & GCKD_df1_male$BL_gfr_mdrd < 45), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd < 30,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd < 30), 1) %>% '*'(100) %>% round(1)
GCKD_df1_male %>% group_by(diabetes) %>%
  summarise_at(vars(BL_uacr), list(~ round(quantile(., na.rm=TRUE), 1)))
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_uacr < 30,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_uacr < 30), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_uacr >= 30 & GCKD_df1_male$BL_uacr <= 300,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_uacr >= 30 & GCKD_df1_male$BL_uacr <= 300), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_uacr > 300,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_uacr > 300), 1) %>% '*'(100) %>% round(1)

## Figure 1: KDIGO categories
prop.table(table(GCKD_df1$BL_uacr < 30, GCKD_df1$BL_gfr_mdrd >= 90))
prop.table(table(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300, GCKD_df1$BL_gfr_mdrd >= 90))
prop.table(table(GCKD_df1$BL_uacr > 300, GCKD_df1$BL_gfr_mdrd >= 90))
prop.table(table(GCKD_df1$BL_uacr < 30, GCKD_df1$BL_gfr_mdrd >= 60 & GCKD_df1$BL_gfr_mdrd < 90))
prop.table(table(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300, GCKD_df1$BL_gfr_mdrd >= 60 & GCKD_df1$BL_gfr_mdrd < 90))
prop.table(table(GCKD_df1$BL_uacr > 300, GCKD_df1$BL_gfr_mdrd >= 60 & GCKD_df1$BL_gfr_mdrd < 90))
prop.table(table(GCKD_df1$BL_uacr < 30, GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60))
prop.table(table(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300, GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60))
prop.table(table(GCKD_df1$BL_uacr > 300, GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60))
prop.table(table(GCKD_df1$BL_uacr < 30, GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45))
prop.table(table(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300, GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45))
prop.table(table(GCKD_df1$BL_uacr > 300, GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45))
prop.table(table(GCKD_df1$BL_uacr < 30, GCKD_df1$BL_gfr_mdrd >= 15 & GCKD_df1$BL_gfr_mdrd < 30))
prop.table(table(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300, GCKD_df1$BL_gfr_mdrd >= 15 & GCKD_df1$BL_gfr_mdrd < 30))
prop.table(table(GCKD_df1$BL_uacr > 300, GCKD_df1$BL_gfr_mdrd >= 15 & GCKD_df1$BL_gfr_mdrd < 30))
prop.table(table(GCKD_df1$BL_uacr < 30, GCKD_df1$BL_gfr_mdrd < 15))
prop.table(table(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300, GCKD_df1$BL_gfr_mdrd < 15))
prop.table(table(GCKD_df1$BL_uacr > 300, GCKD_df1$BL_gfr_mdrd < 15))

## Table 2: Characteristics grouped by inclusion criteria 
table(GCKD_df1$incl_egfr)
var_table_2 <- c("BL_age", "dem_sex", "aa_stroke", "aa_myocard", "aa_hypertens", "aa_diabetes", "aa_renal", 
                 "aa_renal_stones", "aa_dialyse", "aa_ntx", "smoking", "diabetes", "BL_creavalue", "BL_cysvalue",
                 "BL_gfr_mdrd", "BL_uacr", "biopsy", "ckd_lead", "cardiovasc", "incl_egfr")
theme_gtsummary_language("en", big.mark = "")
GCKD_df1_table_2a <-
  GCKD_df1 %>%
  select(all_of(var_table_2)) %>%
  tbl_summary(
    by = incl_egfr,
    statistic = list(all_continuous() ~ "{mean}",
                     all_categorical() ~ "{n}"),
    digits = all_continuous() ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_1 = "eGFR 30-60; n or mean", stat_2 = "Overt proteinuria; n or mean") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_2b <-
  GCKD_df1 %>%
  select(all_of(var_table_2)) %>%
  tbl_summary(
    by = incl_egfr,
    statistic = list(all_continuous() ~ "{sd}",
                     all_categorical() ~ "{p}"),
    digits = ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_1 = "eGFR 30-60; SD or %", stat_2 = "Overt proteinuria; SD or %") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_2 <-
  tbl_merge(tbls = list(GCKD_df1_table_2a, GCKD_df1_table_2b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_table_2 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_titze_table_2.html", 
             path = path_results)
# adjustments depending on hierarchical level
if(is.character(GCKD_df1$BL_age) == TRUE){
  GCKD_df1_less60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[20, 30[" | GCKD_df1$BL_age == "[30, 40[" | GCKD_df1$BL_age == "[40, 50[" | GCKD_df1$BL_age == "[50, 60[")
  print(table(GCKD_df1_less60$incl_egfr, GCKD_df1_less60$cardiovasc == 1,  useNA = "always"))  
  print(prop.table(table(GCKD_df1_less60$incl_egfr, GCKD_df1_less60$cardiovasc == 1), 1) %>% '*'(100) %>% round(1))
  GCKD_df1_more60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[60, 70[" | GCKD_df1$BL_age == "[70, 80[")
  print(table(GCKD_df1_more60$incl_egfr, GCKD_df1_more60$cardiovasc == 1,  useNA = "always"))  
  print(prop.table(table(GCKD_df1_more60$incl_egfr, GCKD_df1_more60$cardiovasc == 1), 1) %>% '*'(100) %>% round(1))
  }else{
    GCKD_df1_less60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age <= 60)
    print(table(GCKD_df1_less60$incl_egfr, GCKD_df1_less60$cardiovasc == 1,  useNA = "always"))  
    print(prop.table(table(GCKD_df1_less60$incl_egfr, GCKD_df1_less60$cardiovasc == 1), 1) %>% '*'(100) %>% round(1))
    GCKD_df1_more60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age > 60)
    print(table(GCKD_df1_more60$incl_egfr, GCKD_df1_more60$cardiovasc == 1,  useNA = "always"))  
    print(prop.table(table(GCKD_df1_more60$incl_egfr, GCKD_df1_more60$cardiovasc == 1), 1) %>% '*'(100) %>% round(1))
    }

## Figure 2: Leading causes of CKD
Fig2_vasc = c(sum(GCKD_df1$ckd_vasc == 1), sum(GCKD_df1$ckd_lead == "ckd_vask"))
Fig2_diab = c(sum(GCKD_df1$ckd_diab == 1), sum(GCKD_df1$ckd_lead == "ckd_diab"))
Fig2_glom_prim = c(sum(GCKD_df1$ckd_glom_prim == 1), sum(GCKD_df1$ckd_lead == "ckd_glom_prim"))
Fig2_syst = c(sum(GCKD_df1$ckd_syst == 1), sum(GCKD_df1$ckd_lead == "ckd_syst"))
Fig2_interst = c(sum(GCKD_df1$ckd_interst == 1), sum(GCKD_df1$ckd_lead == "ckd_interst"))
Fig2_obstr = c(sum(GCKD_df1$ckd_obstr == 1), sum(GCKD_df1$ckd_lead == "ckd_obstr"))
Fig2_single = c(sum(GCKD_df1$ckd_single == 1), sum(GCKD_df1$ckd_lead == "ckd_single"))
Fig2_aki = c(sum(GCKD_df1$ckd_aki == 1), sum(GCKD_df1$ckd_lead == "ckd_aki"))
Fig2_oth = c(sum(GCKD_df1$ckd_oth == 1), sum(GCKD_df1$ckd_lead == "ckd_oth"))
Fig2_heredit = c(sum(GCKD_df1$ckd_heredit == 1), sum(GCKD_df1$ckd_lead == "ckd_heredit"))
Fig2_uk = c(0, sum(GCKD_df1$ckd_lead == "ckd_lead_uk"))
GCKD_df1_Fig2 <- data.frame(Fig2_vasc, Fig2_diab, Fig2_glom_prim, Fig2_syst, Fig2_interst, Fig2_obstr, Fig2_single, Fig2_aki, Fig2_oth, Fig2_heredit, Fig2_uk)
barplot(as.matrix(GCKD_df1_Fig2),
        ylab = "Count",
        beside = TRUE,
        col = c("black", "azure4"),
        border = NA, 
        ylim=range(pretty(c(0, 2450))))
abline(h = c(0, 500, 1000, 1500, 2000), col = "black")
barplot(as.matrix(GCKD_df1_Fig2),
        ylab = "Count",
        beside = TRUE,
        col = c("black", "azure4"),
        border = NA,
        ylim=range(pretty(c(0, 2450))),
        add = TRUE)
prop.table(table(GCKD_df1$diabetes, GCKD_df1$ckd_lead), 2)
prop.table(table(GCKD_df1$hypertension, GCKD_df1$ckd_lead), 2)

## Table 3: Causes of CKD leading & additional
var_table_3 <- c("ckd_lead", "ckd_diab", "ckd_vasc", "ckd_syst", "ckd_glom_prim", "ckd_interst", 
                 "ckd_aki", "ckd_heredit", "ckd_single", "ckd_obstr", "ckd_oth", "ckd_uk", "biopsy")
theme_gtsummary_language("en", big.mark = "")
GCKD_df1_table_3a <-
  GCKD_df1 %>%
  select(all_of(var_table_3)) %>%
  tbl_summary(
    by = ckd_lead,
    statistic = list(all_continuous() ~ "{mean}",
                     all_categorical() ~ "{n}"),
    digits = all_continuous() ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable") %>%
  modify_footnote(update = everything() ~ NA)
sum(GCKD_df1$ckd_lead == "ckd_diab" & (GCKD_df1$ckd_vasc == 1 | GCKD_df1$ckd_syst == 1 | GCKD_df1$ckd_glom_prim == 1 | GCKD_df1$ckd_interst == 1 | GCKD_df1$ckd_aki == 1 | GCKD_df1$ckd_single == 1 | GCKD_df1$ckd_heredit == 1 | GCKD_df1$ckd_obstr == 1 | GCKD_df1$ckd_oth == 1))
sum(GCKD_df1$ckd_lead == "ckd_vask" & (GCKD_df1$ckd_diab == 1 | GCKD_df1$ckd_syst == 1 | GCKD_df1$ckd_glom_prim == 1 | GCKD_df1$ckd_interst == 1 | GCKD_df1$ckd_aki == 1 | GCKD_df1$ckd_single == 1 | GCKD_df1$ckd_heredit == 1 | GCKD_df1$ckd_obstr == 1 | GCKD_df1$ckd_oth == 1))
sum(GCKD_df1$ckd_lead == "ckd_syst" & (GCKD_df1$ckd_vasc == 1 | GCKD_df1$ckd_diab == 1 | GCKD_df1$ckd_glom_prim == 1 | GCKD_df1$ckd_interst == 1 | GCKD_df1$ckd_aki == 1 | GCKD_df1$ckd_single == 1 | GCKD_df1$ckd_heredit == 1 | GCKD_df1$ckd_obstr == 1 | GCKD_df1$ckd_oth == 1))
sum(GCKD_df1$ckd_lead == "ckd_glom_prim" & (GCKD_df1$ckd_vasc == 1 | GCKD_df1$ckd_syst == 1 | GCKD_df1$ckd_diab == 1 | GCKD_df1$ckd_interst == 1 | GCKD_df1$ckd_aki == 1 | GCKD_df1$ckd_single == 1 | GCKD_df1$ckd_heredit == 1 | GCKD_df1$ckd_obstr == 1 | GCKD_df1$ckd_oth == 1))
sum(GCKD_df1$ckd_lead == "ckd_interst" & (GCKD_df1$ckd_vasc == 1 | GCKD_df1$ckd_syst == 1 | GCKD_df1$ckd_glom_prim == 1 | GCKD_df1$ckd_diab == 1 | GCKD_df1$ckd_aki == 1 | GCKD_df1$ckd_single == 1 | GCKD_df1$ckd_heredit == 1 | GCKD_df1$ckd_obstr == 1 | GCKD_df1$ckd_oth == 1))
sum(GCKD_df1$ckd_lead == "ckd_aki" & (GCKD_df1$ckd_vasc == 1 | GCKD_df1$ckd_syst == 1 | GCKD_df1$ckd_glom_prim == 1 | GCKD_df1$ckd_interst == 1 | GCKD_df1$ckd_diab == 1 | GCKD_df1$ckd_single == 1 | GCKD_df1$ckd_heredit == 1 | GCKD_df1$ckd_obstr == 1 | GCKD_df1$ckd_oth == 1))
sum(GCKD_df1$ckd_lead == "ckd_single" & (GCKD_df1$ckd_vasc == 1 | GCKD_df1$ckd_syst == 1 | GCKD_df1$ckd_glom_prim == 1 | GCKD_df1$ckd_interst == 1 | GCKD_df1$ckd_aki == 1 | GCKD_df1$ckd_diab == 1 | GCKD_df1$ckd_heredit == 1 | GCKD_df1$ckd_obstr == 1 | GCKD_df1$ckd_oth == 1))
sum(GCKD_df1$ckd_lead == "ckd_heredit" & (GCKD_df1$ckd_vasc == 1 | GCKD_df1$ckd_syst == 1 | GCKD_df1$ckd_glom_prim == 1 | GCKD_df1$ckd_interst == 1 | GCKD_df1$ckd_aki == 1 | GCKD_df1$ckd_single == 1 | GCKD_df1$ckd_diab == 1 | GCKD_df1$ckd_obstr == 1 | GCKD_df1$ckd_oth == 1))
sum(GCKD_df1$ckd_lead == "ckd_obstr" & (GCKD_df1$ckd_vasc == 1 | GCKD_df1$ckd_syst == 1 | GCKD_df1$ckd_glom_prim == 1 | GCKD_df1$ckd_interst == 1 | GCKD_df1$ckd_aki == 1 | GCKD_df1$ckd_single == 1 | GCKD_df1$ckd_heredit == 1 | GCKD_df1$ckd_diab == 1 | GCKD_df1$ckd_oth == 1))
sum(GCKD_df1$ckd_lead == "ckd_oth" & (GCKD_df1$ckd_vasc == 1 | GCKD_df1$ckd_syst == 1 | GCKD_df1$ckd_glom_prim == 1 | GCKD_df1$ckd_interst == 1 | GCKD_df1$ckd_aki == 1 | GCKD_df1$ckd_single == 1 | GCKD_df1$ckd_heredit == 1 | GCKD_df1$ckd_obstr == 1 | GCKD_df1$ckd_diab == 1))
sum(GCKD_df1$ckd_lead == "ckd_lead_uk" & (GCKD_df1$ckd_diab == 1 | GCKD_df1$ckd_vasc == 1 | GCKD_df1$ckd_syst == 1 | GCKD_df1$ckd_glom_prim == 1 | GCKD_df1$ckd_interst == 1 | GCKD_df1$ckd_aki == 1 | GCKD_df1$ckd_single == 1 | GCKD_df1$ckd_heredit == 1 | GCKD_df1$ckd_obstr == 1 | GCKD_df1$ckd_oth == 1))
sum(GCKD_df1$ckd_lead == "ckd_diab" & (GCKD_df1$ckd_vasc ==2 & GCKD_df1$ckd_syst ==2 & GCKD_df1$ckd_glom_prim ==2 & GCKD_df1$ckd_interst ==2 & GCKD_df1$ckd_aki ==2 & GCKD_df1$ckd_single ==2 & GCKD_df1$ckd_heredit ==2 & GCKD_df1$ckd_obstr ==2 & GCKD_df1$ckd_oth == 2))
sum(GCKD_df1$ckd_lead == "ckd_vask" & (GCKD_df1$ckd_diab ==2 & GCKD_df1$ckd_syst ==2 & GCKD_df1$ckd_glom_prim ==2 & GCKD_df1$ckd_interst ==2 & GCKD_df1$ckd_aki ==2 & GCKD_df1$ckd_single ==2 & GCKD_df1$ckd_heredit ==2 & GCKD_df1$ckd_obstr ==2 & GCKD_df1$ckd_oth == 2))
sum(GCKD_df1$ckd_lead == "ckd_syst" & (GCKD_df1$ckd_vasc ==2 & GCKD_df1$ckd_diab ==2 & GCKD_df1$ckd_glom_prim ==2 & GCKD_df1$ckd_interst ==2 & GCKD_df1$ckd_aki ==2 & GCKD_df1$ckd_single ==2 & GCKD_df1$ckd_heredit ==2 & GCKD_df1$ckd_obstr ==2 & GCKD_df1$ckd_oth == 2))
sum(GCKD_df1$ckd_lead == "ckd_glom_prim" & (GCKD_df1$ckd_vasc ==2 & GCKD_df1$ckd_syst ==2 & GCKD_df1$ckd_diab ==2 & GCKD_df1$ckd_interst ==2 & GCKD_df1$ckd_aki ==2 & GCKD_df1$ckd_single ==2 & GCKD_df1$ckd_heredit ==2 & GCKD_df1$ckd_obstr ==2 & GCKD_df1$ckd_oth == 2))
sum(GCKD_df1$ckd_lead == "ckd_interst" & (GCKD_df1$ckd_vasc ==2 & GCKD_df1$ckd_syst ==2 & GCKD_df1$ckd_glom_prim ==2 & GCKD_df1$ckd_diab ==2 & GCKD_df1$ckd_aki ==2 & GCKD_df1$ckd_single ==2 & GCKD_df1$ckd_heredit ==2 & GCKD_df1$ckd_obstr ==2 & GCKD_df1$ckd_oth == 2))
sum(GCKD_df1$ckd_lead == "ckd_aki" & (GCKD_df1$ckd_vasc ==2 & GCKD_df1$ckd_syst ==2 & GCKD_df1$ckd_glom_prim ==2 & GCKD_df1$ckd_interst ==2 & GCKD_df1$ckd_diab ==2 & GCKD_df1$ckd_single ==2 & GCKD_df1$ckd_heredit ==2 & GCKD_df1$ckd_obstr ==2 & GCKD_df1$ckd_oth == 2))
sum(GCKD_df1$ckd_lead == "ckd_single" & (GCKD_df1$ckd_vasc ==2 & GCKD_df1$ckd_syst ==2 & GCKD_df1$ckd_glom_prim ==2 & GCKD_df1$ckd_interst ==2 & GCKD_df1$ckd_aki ==2 & GCKD_df1$ckd_diab ==2 & GCKD_df1$ckd_heredit ==2 & GCKD_df1$ckd_obstr ==2 & GCKD_df1$ckd_oth == 2))
sum(GCKD_df1$ckd_lead == "ckd_heredit" & (GCKD_df1$ckd_vasc ==2 & GCKD_df1$ckd_syst ==2 & GCKD_df1$ckd_glom_prim ==2 & GCKD_df1$ckd_interst ==2 & GCKD_df1$ckd_aki ==2 & GCKD_df1$ckd_single ==2 & GCKD_df1$ckd_diab ==2 & GCKD_df1$ckd_obstr ==2 & GCKD_df1$ckd_oth == 2))
sum(GCKD_df1$ckd_lead == "ckd_obstr" & (GCKD_df1$ckd_vasc ==2 & GCKD_df1$ckd_syst ==2 & GCKD_df1$ckd_glom_prim ==2 & GCKD_df1$ckd_interst ==2 & GCKD_df1$ckd_aki ==2 & GCKD_df1$ckd_single ==2 & GCKD_df1$ckd_heredit ==2 & GCKD_df1$ckd_diab ==2 & GCKD_df1$ckd_oth == 2))
sum(GCKD_df1$ckd_lead == "ckd_oth" & (GCKD_df1$ckd_vasc ==2 & GCKD_df1$ckd_syst ==2 & GCKD_df1$ckd_glom_prim ==2 & GCKD_df1$ckd_interst ==2 & GCKD_df1$ckd_aki ==2 & GCKD_df1$ckd_single ==2 & GCKD_df1$ckd_heredit ==2 & GCKD_df1$ckd_obstr ==2 & GCKD_df1$ckd_diab == 2))
sum(GCKD_df1$ckd_lead == "ckd_lead_uk" & (GCKD_df1$ckd_diab ==2 & GCKD_df1$ckd_vasc ==2 & GCKD_df1$ckd_syst ==2 & GCKD_df1$ckd_glom_prim ==2 & GCKD_df1$ckd_interst ==2 & GCKD_df1$ckd_aki ==2 & GCKD_df1$ckd_single ==2 & GCKD_df1$ckd_heredit ==2 & GCKD_df1$ckd_obstr ==2 & GCKD_df1$ckd_oth == 2))
prop.table(table(GCKD_df1$biopsy, GCKD_df1$ckd_lead), 2) %>% '*'(100) %>% round(1)

## Figure 3: Patient awareness and treatment
Fig3_6mo = c(sum(GCKD_df1$awareness == "< 6 mo", na.rm = TRUE), sum(GCKD_df1$treatment == "< 6 mo", na.rm = TRUE))
Fig3_1yr = c(sum(GCKD_df1$awareness == "6 mo - < 1 yr", na.rm = TRUE), sum(GCKD_df1$treatment == "6 mo - < 1 yr", na.rm = TRUE))
Fig3_3yr = c(sum(GCKD_df1$awareness == "1 yr - < 3 yr", na.rm = TRUE), sum(GCKD_df1$treatment == "1 yr - < 3 yr", na.rm = TRUE))
Fig3_5yr = c(sum(GCKD_df1$awareness == "3 yr - < 5 yr", na.rm = TRUE), sum(GCKD_df1$treatment == "3 yr - < 5 yr", na.rm = TRUE))
Fig3_more5yr = c(sum(GCKD_df1$awareness == ">= 5 yr", na.rm = TRUE), sum(GCKD_df1$treatment == ">= 5 yr", na.rm = TRUE))
Fig3_uk = c(sum(is.na(GCKD_df1$awareness)), sum(is.na(GCKD_df1$treatment)))
GCKD_df1_Fig3 <- data.frame(Fig3_6mo, Fig3_1yr , Fig3_3yr, Fig3_5yr, Fig3_more5yr, Fig3_uk)
barplot(as.matrix(GCKD_df1_Fig3),
        ylab = "Count",
        beside = TRUE,
        col = c("azure4", "black"),
        border = NA, 
        ylim=range(pretty(c(0, 2950))))
abline(h = c(0, 500, 1000, 1500, 2000, 2500, 3000), col = "black")
barplot(as.matrix(GCKD_df1_Fig3),
        ylab = "Count",
        beside = TRUE,
        col = c("azure4", "black"),
        border = NA, 
        ylim=range(pretty(c(0, 2950))), 
        add = TRUE)

## Table 4: Cardiovascular disease
var_table_4 <- c("BL_age", "dem_sex", "diabetes", "cardiovasc", "hypertension", "valve", "coronary", "myocard", "bypass", "ptca", "cerebrovasc", 
             "stroke", "carotic_surg", "carotic_interv", "pavk", "amput", "ygraft", "pavk_surgery", "pta")
if(is.character(GCKD_df1$BL_age) == TRUE){
  GCKD_df1_less60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[20, 30[" | GCKD_df1$BL_age == "[30, 40[" | GCKD_df1$BL_age == "[40, 50[" | GCKD_df1$BL_age == "[50, 60[")
  print(table(GCKD_df1_less60$cardiovasc == 1,  useNA = "always"))  
  print(prop.table(table(GCKD_df1_less60$cardiovasc == 1)) %>% '*'(100) %>% round(1))
  GCKD_df1_more60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[60, 70[" | GCKD_df1$BL_age == "[70, 80[")
  print(table(GCKD_df1_more60$cardiovasc == 1,  useNA = "always"))  
  print(prop.table(table(GCKD_df1_more60$cardiovasc == 1)) %>% '*'(100) %>% round(1))
}else{
  GCKD_df1_less60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age <= 60)
  print(table(GCKD_df1_less60$cardiovasc == 1,  useNA = "always"))  
  print(prop.table(table(GCKD_df1_less60$cardiovasc == 1)) %>% '*'(100) %>% round(1))
  GCKD_df1_more60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age > 60)
  print(table(GCKD_df1_more60$cardiovasc == 1,  useNA = "always"))  
  print(prop.table(table(GCKD_df1_more60$cardiovasc == 1)) %>% '*'(100) %>% round(1))
}
theme_gtsummary_language("en", big.mark = "")
GCKD_df1_table_4a <-
  GCKD_df1 %>%
  select(all_of(var_table_4)) %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean}",
                     all_categorical() ~ "{n}"),
    digits = all_continuous() ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_0 = "n or mean") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_4b <-
  GCKD_df1 %>%
  select(all_of(var_table_4)) %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{sd}",
                     all_categorical() ~ "{p}"),
    digits = ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_0 = "SD or %") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_4 <-
  tbl_merge(tbls = list(GCKD_df1_table_4a, GCKD_df1_table_4b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_table_4 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_titze_table_4.html", 
             path = path_results)
### Subsetting female
if(is.character(GCKD_df1_fem$BL_age) == TRUE){
  GCKD_df1_fem_less60 <- GCKD_df1_fem %>% subset(GCKD_df1_fem$BL_age == "[20, 30[" | GCKD_df1_fem$BL_age == "[30, 40[" | GCKD_df1_fem$BL_age == "[40, 50[" | GCKD_df1_fem$BL_age == "[50, 60[")
  print(table(GCKD_df1_fem_less60$diabetes, GCKD_df1_fem_less60$cardiovasc == 1,  useNA = "always"))  
  print(prop.table(table(GCKD_df1_fem_less60$diabetes, GCKD_df1_fem_less60$cardiovasc == 1), 1) %>% '*'(100) %>% round(1))
  GCKD_df1_fem_more60 <- GCKD_df1_fem %>% subset(GCKD_df1_fem$BL_age == "[60, 70[" | GCKD_df1_fem$BL_age == "[70, 80[")
  print(table(GCKD_df1_fem_more60$diabetes, GCKD_df1_fem_more60$cardiovasc == 1,  useNA = "always"))  
  print(prop.table(table(GCKD_df1_fem_more60$diabetes, GCKD_df1_fem_more60$cardiovasc == 1), 1) %>% '*'(100) %>% round(1))
}else{
  GCKD_df1_fem_less60 <- GCKD_df1_fem %>% subset(GCKD_df1_fem$BL_age <= 60)
  print(table(GCKD_df1_fem_less60$diabetes, GCKD_df1_fem_less60$cardiovasc == 1,  useNA = "always"))  
  print(prop.table(table(GCKD_df1_fem_less60$diabetes, GCKD_df1_fem_less60$cardiovasc == 1), 1) %>% '*'(100) %>% round(1))
  GCKD_df1_fem_more60 <- GCKD_df1_fem %>% subset(GCKD_df1_fem$BL_age > 60)
  print(table(GCKD_df1_more60$diabetes, GCKD_df1_more60$cardiovasc == 1,  useNA = "always"))  
  print(prop.table(table(GCKD_df1_more60$diabetes, GCKD_df1_more60$cardiovasc == 1), 1) %>% '*'(100) %>% round(1))
}
theme_gtsummary_language("en", big.mark = "")
GCKD_df1_fem_table_4a <-
  GCKD_df1_fem %>%
  select(all_of(var_table_4)) %>%
  tbl_summary(
    by = diabetes,
    statistic = list(all_continuous() ~ "{mean}",
                     all_categorical() ~ "{n}"),
    digits = all_continuous() ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_1 = "Diabetics; n or mean", stat_2 = "Non-diabetics; n or mean") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_fem_table_4b <-
  GCKD_df1_fem %>%
  select(all_of(var_table_4)) %>%
  tbl_summary(
    by = diabetes,
    statistic = list(all_continuous() ~ "{sd}",
                     all_categorical() ~ "{p}"),
    digits = ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_1 = "Diabetics; SD or %", stat_2 = "Non-diabetics; SD or %") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_fem_table_4 <-
  tbl_merge(tbls = list(GCKD_df1_fem_table_4a, GCKD_df1_fem_table_4b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_fem_table_4 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_titze_table_4_fem.html", 
             path = path_results)
### Subsetting male
if(is.character(GCKD_df1_male$BL_age) == TRUE){
  GCKD_df1_male_less60 <- GCKD_df1_male %>% subset(GCKD_df1_male$BL_age == "[20, 30[" | GCKD_df1_male$BL_age == "[30, 40[" | GCKD_df1_male$BL_age == "[40, 50[" | GCKD_df1_male$BL_age == "[50, 60[")
  print(table(GCKD_df1_male_less60$diabetes, GCKD_df1_male_less60$cardiovasc == 1,  useNA = "always"))  
  print(prop.table(table(GCKD_df1_male_less60$diabetes, GCKD_df1_male_less60$cardiovasc == 1), 1) %>% '*'(100) %>% round(1))
  GCKD_df1_male_more60 <- GCKD_df1_male %>% subset(GCKD_df1_male$BL_age == "[60, 70[" | GCKD_df1_male$BL_age == "[70, 80[")
  print(table(GCKD_df1_male_more60$diabetes, GCKD_df1_male_more60$cardiovasc == 1,  useNA = "always"))  
  print(prop.table(table(GCKD_df1_male_more60$diabetes, GCKD_df1_male_more60$cardiovasc == 1), 1) %>% '*'(100) %>% round(1))
}else{
  GCKD_df1_male_less60 <- GCKD_df1_male %>% subset(GCKD_df1_male$BL_age <= 60)
  print(table(GCKD_df1_male_less60$diabetes, GCKD_df1_male_less60$cardiovasc == 1,  useNA = "always"))  
  print(prop.table(table(GCKD_df1_male_less60$diabetes, GCKD_df1_male_less60$cardiovasc == 1), 1) %>% '*'(100) %>% round(1))
  GCKD_df1_male_more60 <- GCKD_df1_male %>% subset(GCKD_df1_male$BL_age > 60)
  print(table(GCKD_df1_more60$diabetes, GCKD_df1_more60$cardiovasc == 1,  useNA = "always"))  
  print(prop.table(table(GCKD_df1_more60$diabetes, GCKD_df1_more60$cardiovasc == 1), 1) %>% '*'(100) %>% round(1))
}
theme_gtsummary_language("en", big.mark = "")
GCKD_df1_male_table_4a <-
  GCKD_df1_male %>%
  select(all_of(var_table_4)) %>%
  tbl_summary(
    by = diabetes,
    statistic = list(all_continuous() ~ "{mean}",
                     all_categorical() ~ "{n}"),
    digits = all_continuous() ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_1 = "Diabetics; n or mean", stat_2 = "Non-diabetics; n or mean") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_male_table_4b <-
  GCKD_df1_male %>%
  select(all_of(var_table_4)) %>%
  tbl_summary(
    by = diabetes,
    statistic = list(all_continuous() ~ "{sd}",
                     all_categorical() ~ "{p}"),
    digits = ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_1 = "Diabetics; SD or %", stat_2 = "Non-diabetics; SD or %") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_male_table_4 <-
  tbl_merge(tbls = list(GCKD_df1_male_table_4a, GCKD_df1_male_table_4b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_male_table_4 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_titze_table_4_male.html", 
             path = path_results)

## Suppl. Table 2: GFR categories
table(GCKD_df1$BL_gfr_mdrd >= 90,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_mdrd >= 90))
table(GCKD_df1$BL_gfr_mdrd >=60 & GCKD_df1$BL_gfr_mdrd < 90,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_mdrd >= 60 & GCKD_df1$BL_gfr_mdrd < 90))
table(GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60))
table(GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45))
table(GCKD_df1$BL_gfr_mdrd < 30,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_mdrd < 30))
GCKD_df1 %>%
  summarise_at(vars(BL_gfr_mdrd), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1 %>%
  summarise_at(vars(BL_gfr_mdrd), list(~ round(sd(., na.rm=TRUE),1)))
table(GCKD_df1$BL_gfr_ckdepi >= 90,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_ckdepi >= 90))
table(GCKD_df1$BL_gfr_ckdepi >=60 & GCKD_df1$BL_gfr_ckdepi < 90,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_ckdepi >= 60 & GCKD_df1$BL_gfr_ckdepi < 90))
table(GCKD_df1$BL_gfr_ckdepi >= 45 & GCKD_df1$BL_gfr_ckdepi < 60,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_ckdepi >= 45 & GCKD_df1$BL_gfr_ckdepi < 60))
table(GCKD_df1$BL_gfr_ckdepi >= 30 & GCKD_df1$BL_gfr_ckdepi < 45,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_ckdepi >= 30 & GCKD_df1$BL_gfr_ckdepi < 45))
table(GCKD_df1$BL_gfr_ckdepi < 30,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_ckdepi < 30))
GCKD_df1 %>%
  summarise_at(vars(BL_gfr_ckdepi), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1 %>%
  summarise_at(vars(BL_gfr_ckdepi), list(~ round(sd(., na.rm=TRUE),1)))
table(GCKD_df1$BL_gfr_ckdepi_cys >= 90,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_ckdepi_cys >= 90))
table(GCKD_df1$BL_gfr_ckdepi_cys >=60 & GCKD_df1$BL_gfr_ckdepi_cys < 90,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_ckdepi_cys >= 60 & GCKD_df1$BL_gfr_ckdepi_cys < 90))
table(GCKD_df1$BL_gfr_ckdepi_cys >= 45 & GCKD_df1$BL_gfr_ckdepi_cys < 60,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_ckdepi_cys >= 45 & GCKD_df1$BL_gfr_ckdepi_cys < 60))
table(GCKD_df1$BL_gfr_ckdepi_cys >= 30 & GCKD_df1$BL_gfr_ckdepi_cys < 45,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_ckdepi_cys >= 30 & GCKD_df1$BL_gfr_ckdepi_cys < 45))
table(GCKD_df1$BL_gfr_ckdepi_cys < 30,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_ckdepi_cys < 30))
GCKD_df1 %>%
  summarise_at(vars(BL_gfr_ckdepi_cys), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1 %>%
  summarise_at(vars(BL_gfr_ckdepi_cys), list(~ round(sd(., na.rm=TRUE),1)))
table(GCKD_df1$BL_gfr_ckdepi_creacys >= 90,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_ckdepi_creacys >= 90))
table(GCKD_df1$BL_gfr_ckdepi_creacys >=60 & GCKD_df1$BL_gfr_ckdepi_creacys < 90,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_ckdepi_creacys >= 60 & GCKD_df1$BL_gfr_ckdepi_creacys < 90))
table(GCKD_df1$BL_gfr_ckdepi_creacys >= 45 & GCKD_df1$BL_gfr_ckdepi_creacys < 60,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_ckdepi_creacys >= 45 & GCKD_df1$BL_gfr_ckdepi_creacys < 60))
table(GCKD_df1$BL_gfr_ckdepi_creacys >= 30 & GCKD_df1$BL_gfr_ckdepi_creacys < 45,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_ckdepi_creacys >= 30 & GCKD_df1$BL_gfr_ckdepi_creacys < 45))
table(GCKD_df1$BL_gfr_ckdepi_creacys < 30,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_ckdepi_creacys < 30))
GCKD_df1 %>%
  summarise_at(vars(BL_gfr_ckdepi_creacys), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1 %>%
  summarise_at(vars(BL_gfr_ckdepi_creacys), list(~ round(sd(., na.rm=TRUE),1)))

## Suppl. Table 3: Diabetic nephropathy
GCKD_df1 <- GCKD_df1 %>% mutate(DM = ifelse(GCKD_df1$diabetes == 2, "No DM", ifelse(GCKD_df1$ckd_lead == "ckd_diab", "DMwDN", "DMwoDN")))
var_table_s3 <- c("BL_age", "education", "aa_stroke", "aa_myocard", "aa_hypertens", "aa_diabetes", "aa_renal", 
                 "aa_renal_stones", "aa_dialyse", "aa_ntx", "smoking", "hospital", "BL_ku_height_cm","BL_ku_weight", 
                 "BL_ku_bmi", "BL_ku_sys", "BL_ku_dia", "BL_ku_map", "BL_ku_ruhepuls", "BL_creavalue", "BL_cysvalue",
                 "BL_gfr_mdrd", "BL_uacr", "BL_med_raas_ace", "BL_med_raas_at1", "BL_med_raas_double", 
                 "BL_med_raas_single", "BL_med_diuretic", "BL_med_diuretic_thiazid", "BL_med_diuretic_aldost", 
                 "BL_med_diuretic_loop", "BL_med_caanta", "BL_med_bblocker", "biopsy", "DM")
table(GCKD_df1$DM)
theme_gtsummary_language("en", big.mark = "")
GCKD_df1_table_s3a <-
  GCKD_df1 %>%
  select(all_of(var_table_s3)) %>%
  tbl_summary(
    by = DM,
    statistic = list(all_continuous() ~ "{mean}",
                     all_categorical() ~ "{n}"),
    digits = all_continuous() ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_1 = "DM w DN; n or mean", stat_2 = "DM wo DN; n or mean", stat_3 = "No DM; n or mean") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_s3b <-
  GCKD_df1 %>%
  select(all_of(var_table_s3)) %>%
  tbl_summary(
    by = DM,
    statistic = list(all_continuous() ~ "{sd}",
                     all_categorical() ~ "{p}"),
    digits = ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_1 = "DM w DN; SD or %", stat_2 = "DM wo DN; SD or %", stat_3 = "No DM; SD or %") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_s3 <-
  tbl_merge(tbls = list(GCKD_df1_table_s3a, GCKD_df1_table_s3b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_table_s3 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_titze_table_s3.html", 
             path = path_results)
if(is.character(GCKD_df1$BL_ku_bmi) == TRUE){
  print(table(GCKD_df1$DM, GCKD_df1$BL_ku_bmi == "[30, 35[" | GCKD_df1$BL_ku_bmi == "[35, 40[" | GCKD_df1$BL_ku_bmi == "[40, 70[",  useNA = "always"))  
  print(prop.table(table(GCKD_df1$DM, GCKD_df1$BL_ku_bmi == "[30, 35[" | GCKD_df1$BL_ku_bmi == "[35, 40[" | GCKD_df1$BL_ku_bmi == "[40, 70["), 1) %>% '*'(100) %>% round(1))
  print(table(GCKD_df1$DM, GCKD_df1$BL_ku_bmi == "[25, 30[",  useNA = "always"))
  print(prop.table(table(GCKD_df1$DM, GCKD_df1$BL_ku_bmi == "[25, 30["), 1) %>% '*'(100) %>% round(1))
  print(table(GCKD_df1$DM, GCKD_df1$BL_ku_bmi == "[15, 18.5[" | GCKD_df1$BL_ku_bmi == "[18.5, 25[",  useNA = "always"))
  print(prop.table(table(GCKD_df1$DM, GCKD_df1$BL_ku_bmi == "[15, 18.5[" | GCKD_df1$BL_ku_bmi == "[18.5, 25["), 1) %>% '*'(100) %>% round(1))
}else{
  print(table(GCKD_df1$DM, GCKD_df1$BL_ku_bmi > 30,  useNA = "always"))
  print(prop.table(table(GCKD_df1$DM, GCKD_df1$BL_ku_bmi > 30), 1) %>% '*'(100) %>% round(1))
  print(table(GCKD_df1$DM, GCKD_df1$BL_ku_bmi <= 30 & GCKD_df1$BL_ku_bmi > 25,  useNA = "always"))
  print(prop.table(table(GCKD_df1$DM, GCKD_df1$BL_ku_bmi <= 30 & GCKD_df1$BL_ku_bmi > 25), 1) %>% '*'(100) %>% round(1))
  print(table(GCKD_df1$DM, GCKD_df1$BL_ku_bmi <= 25,  useNA = "always"))
  print(prop.table(table(GCKD_df1$DM, GCKD_df1$BL_ku_bmi <= 25), 1) %>% '*'(100) %>% round(1))
}
table(GCKD_df1$DM, GCKD_df1$BL_ku_sys < 130 & GCKD_df1$BL_ku_dia < 80,  useNA = "always")
prop.table(table(GCKD_df1$DM, GCKD_df1$BL_ku_sys < 130 & GCKD_df1$BL_ku_dia < 80), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1$DM, GCKD_df1$BL_ku_sys < 140 & GCKD_df1$BL_ku_dia < 90,  useNA = "always")
prop.table(table(GCKD_df1$DM, GCKD_df1$BL_ku_sys < 140 & GCKD_df1$BL_ku_dia < 90), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1$DM, GCKD_df1$BL_gfr_mdrd >= 60,  useNA = "always")
prop.table(table(GCKD_df1$DM, GCKD_df1$BL_gfr_mdrd >= 60), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1$DM, GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60,  useNA = "always")
prop.table(table(GCKD_df1$DM, GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1$DM, GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45,  useNA = "always")
prop.table(table(GCKD_df1$DM, GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1$DM, GCKD_df1$BL_gfr_mdrd < 30,  useNA = "always")
prop.table(table(GCKD_df1$DM, GCKD_df1$BL_gfr_mdrd < 30), 1) %>% '*'(100) %>% round(1)
GCKD_df1 %>% group_by(diabetes) %>%
  summarise_at(vars(BL_uacr), list(~ round(quantile(., na.rm=TRUE), 1)))
table(GCKD_df1$DM, GCKD_df1$BL_uacr < 30,  useNA = "always")
prop.table(table(GCKD_df1$DM, GCKD_df1$BL_uacr < 30), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1$DM, GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300,  useNA = "always")
prop.table(table(GCKD_df1$DM, GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300), 1) %>% '*'(100) %>% round(1)
table(GCKD_df1$DM, GCKD_df1$BL_uacr > 300,  useNA = "always")
prop.table(table(GCKD_df1$DM, GCKD_df1$BL_uacr > 300), 1) %>% '*'(100) %>% round(1)

## Suppl. Figure 1: Age distribution stratified
GCKD_df1_FigS1 <- GCKD_df1
if(is.character(GCKD_df1_FigS1$BL_age) == TRUE){
  GCKD_df1_FigS1 <- GCKD_df1_FigS1 %>% mutate(BL_age_cat = ifelse(
    GCKD_df1_FigS1$BL_age == "[20, 40[", 1, ifelse(GCKD_df1_FigS1$BL_age == "[40, 60[", 2, 3)))
  print(GCKD_df1_FigS1 %>%
    filter(dem_sex == "Male") %>%
    ggplot(aes(x=as.factor(BL_age_cat))) +
    geom_bar(color="white", fill="#73A1AA", width=1.0)  +
    scale_y_continuous(limits=c(0,2100), breaks=c(0,300,600,900,1200,1500,1800,2100)) + 
    coord_flip() +
    theme(aspect.ratio = 3/2))
  print(GCKD_df1_FigS1 %>%
    filter(dem_sex == "Female") %>%
    ggplot(aes(x=as.factor(BL_age_cat))) +
    geom_bar(color="white", fill="#73A1AA", alpha = 0.5, width=1.0)  +
    scale_y_continuous(limits=c(0,2100), breaks=c(0,300,600,900,1200,1500,1800,2100)) + 
    coord_flip() +
    theme(aspect.ratio = 3/2))
  print(GCKD_df1_FigS1 %>%
    filter(!is.na(BL_age_cat)) %>%
    filter (diabetes == 1) %>%
    ggplot(aes(x=as.factor(BL_age_cat))) +
    geom_bar(color="white", fill="#73A1AA", width=1.0)  +
    scale_y_continuous(limits=c(0,2100), breaks=c(0,300,600,900,1200,1500,1800,2100)) + 
    coord_flip() +
    theme(aspect.ratio = 3/2))
  print(GCKD_df1_FigS1 %>%
    filter(!is.na(BL_age_cat)) %>%
    filter(diabetes == 2) %>%
    ggplot(aes(x=as.factor(BL_age_cat))) +
    geom_bar(color="white", fill="#73A1AA", alpha = 0.5, width=1.0)  +
    scale_y_continuous(limits=c(0,2100), breaks=c(0,300,600,900,1200,1500,1800,2100)) + 
    coord_flip() +
    theme(aspect.ratio = 3/2))
} else{
  GCKD_df1_FigS1 <- GCKD_df1_FigS1 %>% mutate(
    BL_age_cat = ifelse(GCKD_df1_FigS1$BL_age < 20, 1, ifelse(GCKD_df1_FigS1$BL_age >= 20 & GCKD_df1_FigS1$BL_age < 30, 2, ifelse(
      GCKD_df1_FigS1$BL_age >= 30 & GCKD_df1_FigS1$BL_age < 40, 3, ifelse(GCKD_df1_FigS1$BL_age >= 40 & GCKD_df1_FigS1$BL_age < 50, 4, ifelse(
        GCKD_df1_FigS1$BL_age >= 50 & GCKD_df1_FigS1$BL_age < 60, 5, ifelse(GCKD_df1_FigS1$BL_age >= 60 & GCKD_df1_FigS1$BL_age < 70, 6, 7)))))))
  print(GCKD_df1_FigS1 %>%
    filter(dem_sex == "Male") %>%
    ggplot(aes(x=as.factor(BL_age_cat))) +
    geom_bar(color="white", fill="#B83674", width=1.0)  +
    scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
    coord_flip() +
    theme(aspect.ratio = 3/2))
  print(GCKD_df1_FigS1 %>%
    filter(dem_sex == "Female") %>%
    ggplot(aes(x=as.factor(BL_age_cat))) +
    geom_bar(color="white", fill="#B83674", alpha = 0.5, width=1.0) +
    scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
    coord_flip() +
    theme(aspect.ratio = 3/2))
  print(GCKD_df1_FigS1 %>%
    filter(diabetes == 1) %>%
    ggplot(aes(x=as.factor(BL_age_cat))) +
    geom_bar(color="white", fill="#B83674", width=1.0)  +
    scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
    coord_flip() +
    theme(aspect.ratio = 3/2))
  print(GCKD_df1_FigS1 %>%
    filter(diabetes == 2) %>%
    ggplot(aes(x=as.factor(BL_age_cat))) +
    geom_bar(color="white", fill="#B83674", alpha = 0.5, width=1.0) +
    scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
    coord_flip() +
    theme(aspect.ratio = 3/2))
}


if(is.character(GCKD_df1_FigS1$BL_age) == TRUE){
  GCKD_df1_FigS1 <- GCKD_df1_FigS1 %>% mutate(BL_age_cat = ifelse(
    GCKD_df1_FigS1$BL_age == "[20, 30[", 1, ifelse(GCKD_df1_FigS1$BL_age == "[30, 40[", 2, ifelse(GCKD_df1_FigS1$BL_age == "[40, 50[", 3, ifelse(GCKD_df1_FigS1$BL_age == "[50, 60[", 4, ifelse(GCKD_df1_FigS1$BL_age == "[60, 70[", 5, 6))))))
  print(GCKD_df1_FigS1 %>%
          filter(dem_sex == "Male") %>%
          ggplot(aes(x=as.factor(BL_age_cat))) +
          geom_bar(color="white", fill="#73A1AA", width=1.0)  +
          scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
          coord_flip() +
          theme(aspect.ratio = 3/2))
  print(GCKD_df1_FigS1 %>%
          filter(dem_sex == "Female") %>%
          ggplot(aes(x=as.factor(BL_age_cat))) +
          geom_bar(color="white", fill="#73A1AA", alpha = 0.5, width=1.0)  +
          scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
          coord_flip() +
          theme(aspect.ratio = 3/2))
  print(GCKD_df1_FigS1 %>%
          filter(!is.na(BL_age_cat)) %>%
          filter (diabetes == 1) %>%
          ggplot(aes(x=as.factor(BL_age_cat))) +
          geom_bar(color="white", fill="#73A1AA", width=1.0)  +
          scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
          coord_flip() +
          theme(aspect.ratio = 3/2))
  print(GCKD_df1_FigS1 %>%
          filter(!is.na(BL_age_cat)) %>%
          filter(diabetes == 2) %>%
          ggplot(aes(x=as.factor(BL_age_cat))) +
          geom_bar(color="white", fill="#73A1AA", alpha = 0.5, width=1.0)  +
          scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
          coord_flip() +
          theme(aspect.ratio = 3/2))
} else{
  GCKD_df1_FigS1 <- GCKD_df1_FigS1 %>% mutate(
    BL_age_cat = ifelse(GCKD_df1_FigS1$BL_age < 20, 1, ifelse(GCKD_df1_FigS1$BL_age >= 20 & GCKD_df1_FigS1$BL_age < 30, 2, ifelse(
      GCKD_df1_FigS1$BL_age >= 30 & GCKD_df1_FigS1$BL_age < 40, 3, ifelse(GCKD_df1_FigS1$BL_age >= 40 & GCKD_df1_FigS1$BL_age < 50, 4, ifelse(
        GCKD_df1_FigS1$BL_age >= 50 & GCKD_df1_FigS1$BL_age < 60, 5, ifelse(GCKD_df1_FigS1$BL_age >= 60 & GCKD_df1_FigS1$BL_age < 70, 6, 7)))))))
  print(GCKD_df1_FigS1 %>%
          filter(dem_sex == "Male") %>%
          ggplot(aes(x=as.factor(BL_age_cat))) +
          geom_bar(color="white", fill="#B83674", width=1.0)  +
          scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
          coord_flip() +
          theme(aspect.ratio = 3/2))
  print(GCKD_df1_FigS1 %>%
          filter(dem_sex == "Female") %>%
          ggplot(aes(x=as.factor(BL_age_cat))) +
          geom_bar(color="white", fill="#B83674", alpha = 0.5, width=1.0) +
          scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
          coord_flip() +
          theme(aspect.ratio = 3/2))
  print(GCKD_df1_FigS1 %>%
          filter(diabetes == 1) %>%
          ggplot(aes(x=as.factor(BL_age_cat))) +
          geom_bar(color="white", fill="#B83674", width=1.0)  +
          scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
          coord_flip() +
          theme(aspect.ratio = 3/2))
  print(GCKD_df1_FigS1 %>%
          filter(diabetes == 2) %>%
          ggplot(aes(x=as.factor(BL_age_cat))) +
          geom_bar(color="white", fill="#B83674", alpha = 0.5, width=1.0) +
          scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
          coord_flip() +
          theme(aspect.ratio = 3/2))
}

# Comparison anonymized vs original dataset
GCKD_df1_o <- as_tibble(read.xlsx("GCKD_df1_origin.xlsx", sep = ";"))
GCKD_df1 <- as_tibble(read.xlsx("GCKD_df1_generic_k2s10.xlsx", sep = ";"))
GCKD_df2 <- as_tibble(read.xlsx("GCKD_df1_generic_k5s10.xlsx", sep = ";"))
GCKD_df3 <- as_tibble(read.xlsx("GCKD_df1_generic_k11s10.xlsx", sep = ";"))
# Data Cleansing
## Marking NA
GCKD_df1 <- GCKD_df1 %>% na_if("*")
GCKD_df1 <- GCKD_df1 %>% na_if("NULL")
GCKD_df1 <- GCKD_df1 %>% na_if("null")
GCKD_df2 <- GCKD_df2 %>% na_if("*")
GCKD_df2 <- GCKD_df2 %>% na_if("NULL")
GCKD_df2 <- GCKD_df2 %>% na_if("null")
GCKD_df3 <- GCKD_df3 %>% na_if("*")
GCKD_df3 <- GCKD_df3 %>% na_if("NULL")
GCKD_df3 <- GCKD_df3 %>% na_if("null")
## Data Type
GCKD_df1_o$ckd_lead <- factor(GCKD_df1_o$ckd_lead, levels = c("ckd_diab", "ckd_vask", "ckd_syst", "ckd_glom_prim", 
                                                              "ckd_interst", "ckd_aki", "ckd_single", "ckd_heredit", 
                                                              "ckd_obstr", "ckd_oth", "ckd_lead_uk"))
GCKD_df1_o <- GCKD_df1_o %>% mutate(across(all_of(col_cat), as.character))
GCKD_df1_o <- GCKD_df1_o %>% mutate(across(all_of(col_num), as.numeric))
GCKD_df1_o_fem <- GCKD_df1_o %>% subset(dem_sex == "Female")
GCKD_df1_o_male <- GCKD_df1_o %>% subset(dem_sex == "Male")
GCKD_df1 <- GCKD_df1 %>% mutate(across(all_of(col_cat), as.character))
GCKD_df1 <- GCKD_df1 %>% mutate(across(all_of(col_num), as.numeric))
GCKD_df2 <- GCKD_df2 %>% mutate(across(all_of(col_cat), as.character))
GCKD_df2 <- GCKD_df2 %>% mutate(across(all_of(col_num), as.numeric))
GCKD_df3 <- GCKD_df3 %>% mutate(across(all_of(col_cat), as.character))
GCKD_df3 <- GCKD_df3 %>% mutate(across(all_of(col_num), as.numeric))

## Visual comparison for datatype changes: age, height, weight, bmi (table 1) subset female diabetics
GCKD_df1_o_femd <- GCKD_df1_o %>% subset(dem_sex == "Female" & diabetes == 1)
GCKD_df1_femd <- GCKD_df1 %>% subset(dem_sex == "Female" & diabetes == 1)
GCKD_df2_femd <- GCKD_df2 %>% subset(dem_sex == "Female" & diabetes == 1)
GCKD_df3_femd <- GCKD_df3 %>% subset(dem_sex == "Female" & diabetes == 1)
GCKD_df1_o_femd_QI <- GCKD_df1_o_femd %>% select(BL_age, dem_sex, BL_ku_height_cm, BL_ku_weight, BL_ku_bmi, biopsy)
GCKD_df1_femd_QI <- GCKD_df1_femd %>% select(BL_age, dem_sex, BL_ku_height_cm, BL_ku_weight, BL_ku_bmi, biopsy)
GCKD_df2_femd_QI <- GCKD_df2_femd %>% select(BL_age, dem_sex, BL_ku_height_cm, BL_ku_weight, BL_ku_bmi, biopsy)
GCKD_df3_femd_QI <- GCKD_df3_femd %>% select(BL_age, dem_sex, BL_ku_height_cm, BL_ku_weight, BL_ku_bmi, biopsy)

GCKD_df1_o_femd_QI_num <- select_if(GCKD_df1_o_femd_QI, is.numeric)
for(i in 1:ncol((GCKD_df1_o_femd_QI_num))) {
  x = colnames(GCKD_df1_o_femd_QI_num)[i]
  Fig <- ggplot() + 
    geom_histogram(aes_string(x), data = GCKD_df1_o_femd_QI_num, binwidth=1, colour="white", fill="azure4") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  print(Fig)
}

GCKD_df1_femd_QI_cat <- select_if(GCKD_df1_femd_QI, is.character)
GCKD_df1_femd_QI_cat$BL_ku_weight <- factor(GCKD_df1_femd_QI_cat$BL_ku_weight, levels = c("[40, 60[", "[60, 80[", "[80, 100[", "[100, 120[", "[120, 140[", "[140, 160["))
GCKD_df1_femd_QI_cat$BL_ku_bmi <- factor(GCKD_df1_femd_QI_cat$BL_ku_bmi, levels = c("[5, 18.5[", "[18.5, 25[", "[25, 30[", "[30, 35[", "[35, 40[", "[40, 250["))
for(i in 1:ncol((GCKD_df1_femd_QI_cat))) {
  x = colnames(GCKD_df1_femd_QI_cat)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_femd_QI_cat, !is.na(GCKD_df1_femd_QI_cat[i])), colour="white", fill="green4") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  print(Fig)
}

GCKD_df2_femd_QI_cat <- select_if(GCKD_df2_femd_QI, is.character)
GCKD_df2_femd_QI_cat$BL_ku_weight <- factor(GCKD_df2_femd_QI_cat$BL_ku_weight, levels = c("[40, 80[", "[80, 120[", "[120, 160["))
GCKD_df2_femd_QI_cat$BL_ku_bmi <- factor(GCKD_df2_femd_QI_cat$BL_ku_bmi, levels = c("[5, 18.5[", "[18.5, 25[", "[25, 30[", "[30, 35[", "[35, 40[", "[40, 250["))
for(i in 1:ncol((GCKD_df2_femd_QI_cat))) {
  x = colnames(GCKD_df2_femd_QI_cat)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df2_femd_QI_cat, !is.na(GCKD_df2_femd_QI_cat[i])), colour="white", fill="red4") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
  print(Fig)
}

GCKD_df3_femd_QI_cat <- select_if(GCKD_df3_femd_QI, is.character)
GCKD_df3_femd_QI_cat$BL_ku_weight <- factor(GCKD_df3_femd_QI_cat$BL_ku_weight, levels = c("[40, 80[", "[80, 120[", "[120, 160["))
GCKD_df3_femd_QI_cat$BL_ku_bmi <- factor(GCKD_df3_femd_QI_cat$BL_ku_bmi, levels = c("[5, 18.5[", "[18.5, 25[", "[25, 30[", "[30, 35[", "[35, 40[", "[40, 250["))
for(i in 1:ncol((GCKD_df3_femd_QI_cat))) {
  x = colnames(GCKD_df3_femd_QI_cat)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df3_femd_QI_cat, !is.na(GCKD_df3_femd_QI_cat[i])), colour="white", fill="gold") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  print(Fig)
}

### k=2: green4, k=5 red4, k=11 gold, original azure4 (alternative steelblue4)
GCKD_df1_mut <- GCKD_df1
GCKD_df1_mut$BL_age <- mapvalues(GCKD_df1_mut$BL_age, from = c("[20, 40[", "[40, 60[", "[60, 80["), to = c("30", "50", "70"))
GCKD_df1_mut$BL_age <- as.numeric(GCKD_df1_mut$BL_age)
GCKD_df2_mut <- GCKD_df2
GCKD_df2_mut$BL_age <- mapvalues(GCKD_df2_mut$BL_age, from = c("[20, 40[", "[40, 60[", "[60, 80["), to = c("30", "50", "70"))
GCKD_df2_mut$BL_age <- as.numeric(GCKD_df2_mut$BL_age)
GCKD_df3_mut <- GCKD_df3
GCKD_df3_mut$BL_age <- mapvalues(GCKD_df3_mut$BL_age, from = c("[20, 40[", "[40, 60[", "[60, 80["), to = c("30", "50", "70"))
GCKD_df3_mut$BL_age <- as.numeric(GCKD_df3_mut$BL_age)

Fig <- ggplot() + 
  geom_density(data = GCKD_df1_o, aes(BL_age, y = ..density..*70000), colour="transparent", fill = "azure4", size = 1) +
  geom_bar(data = subset(GCKD_df1_mut, !is.na(GCKD_df1_mut$BL_age)), aes(BL_age), colour="green4", size = 1, fill="transparent", width=20) +
  geom_bar(data = subset(GCKD_df2_mut, !is.na(GCKD_df2_mut$BL_age)), aes(BL_age), colour="red4", size = 1, fill="transparent", width=20) +
  geom_bar(data = subset(GCKD_df3_mut, !is.na(GCKD_df3_mut$BL_age)), aes(BL_age), colour="gold", size = 1, fill="transparent", width=20) +
  scale_y_continuous(sec.axis = sec_axis(~. /70000, name = "2nd")) + 
  scale_x_continuous(sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
print(Fig)

Fig_1 <- ggplot() + 
  geom_histogram(data = GCKD_df1_o, aes(BL_age, y = ..count..*10), colour="white", fill="azure4", alpha=0.5, binwidth=1) +
  geom_bar(data = subset(GCKD_df1_mut, !is.na(GCKD_df1_mut$BL_age)), aes(BL_age), colour="green4", size = 1, fill="transparent", width=20) +
  geom_bar(data = subset(GCKD_df2_mut, !is.na(GCKD_df2_mut$BL_age)), aes(BL_age), colour="red4", size = 1, fill="transparent", width=20) +
  geom_bar(data = subset(GCKD_df3_mut, !is.na(GCKD_df3_mut$BL_age)), aes(BL_age), colour="gold", size = 1, fill="transparent", width=20) +
  scale_y_continuous(sec.axis = sec_axis(~. /10)) + 
  scale_x_continuous(sec.axis = sec_axis(~.)) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
print(Fig_1)



## Bootstrapping
boot_gender = do(10000)*table(mosaic::resample(GCKD_df1_o)$dem_sex)
confint(boot_gender, level = 0.95)

boot_prop_stroke = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$aa_stroke))*100)
bca_prop_stroke <- bca(boot_prop_stroke$X1)
boot_prop_stroke_bca <- t(data.frame(bca_prop_stroke))
boot_prop_myocard = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$aa_myocard))*100)
bca_prop_myocard <- bca(boot_prop_myocard$X1)
boot_prop_myocard_bca <- t(data.frame(bca_prop_myocard))
boot_prop_hypertens = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$aa_hypertens))*100)
bca_prop_hypertens <- bca(boot_prop_hypertens$X1)
boot_prop_hypertens_bca <- t(data.frame(bca_prop_hypertens))
boot_prop_diabetes = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$aa_diabetes))*100)
bca_prop_diabetes <- bca(boot_prop_diabetes$X1)
boot_prop_diabetes_bca <- t(data.frame(bca_prop_diabetes))
boot_prop_renal = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$aa_renal))*100)
bca_prop_renal <- bca(boot_prop_renal$X1)
boot_prop_renal_bca <- t(data.frame(bca_prop_renal))
boot_prop_renal_stones = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$aa_renal_stones))*100)
bca_prop_renal_stones <- bca(boot_prop_renal_stones$X1)
boot_prop_renal_stones_bca <- t(data.frame(bca_prop_renal_stones))
boot_prop_dialyse = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$aa_dialyse))*100)
bca_prop_dialyse <- bca(boot_prop_dialyse$X1)
boot_prop_dialyse_bca <- t(data.frame(bca_prop_dialyse))
boot_prop_ntx = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$aa_ntx))*100)
bca_prop_ntx <- bca(boot_prop_ntx$X1)
boot_prop_ntx_bca <- t(data.frame(bca_prop_ntx))
boot_prop_smoking = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$smoking))*100)
bca_prop_smoking_1 <- bca(boot_prop_smoking$X1)
bca_prop_smoking_2 <- bca(boot_prop_smoking$X2)
bca_prop_smoking_3 <- bca(boot_prop_smoking$X3)
boot_prop_smoking_1_bca <- t(data.frame(bca_prop_smoking_1))
boot_prop_smoking_2_bca <- t(data.frame(bca_prop_smoking_2))
boot_prop_smoking_3_bca <- t(data.frame(bca_prop_smoking_3))
boot_prop_hospital = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$hospital))*100)
bca_prop_hospital <- bca(boot_prop_hospital$X1)
boot_prop_hospital_bca <- t(data.frame(bca_prop_hospital))
boot_prop_BL_med_raas_ace = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$BL_med_raas_ace))*100)
bca_prop_BL_med_raas_ace <- bca(boot_prop_BL_med_raas_ace$X1)
boot_prop_BL_med_raas_ace_bca <- t(data.frame(bca_prop_BL_med_raas_ace))
boot_prop_BL_med_raas_at1 = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$BL_med_raas_at1))*100)
bca_prop_BL_med_raas_at1 <- bca(boot_prop_BL_med_raas_at1$X1)
boot_prop_BL_med_raas_at1_bca <- t(data.frame(bca_prop_BL_med_raas_at1))
boot_prop_BL_med_raas_single = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$BL_med_raas_single))*100)
bca_prop_BL_med_raas_single <- bca(boot_prop_BL_med_raas_single$X1)
boot_prop_BL_med_raas_single_bca <- t(data.frame(bca_prop_BL_med_raas_single))
boot_prop_BL_med_raas_double = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$BL_med_raas_double))*100)
bca_prop_BL_med_raas_double <- bca(boot_prop_BL_med_raas_double$X1)
boot_prop_BL_med_raas_double_bca <- t(data.frame(bca_prop_BL_med_raas_double))
boot_prop_BL_med_diuretic = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$BL_med_diuretic))*100)
bca_prop_BL_med_diuretic <- bca(boot_prop_BL_med_diuretic$X1)
boot_prop_BL_med_diuretic_bca <- t(data.frame(bca_prop_BL_med_diuretic))
boot_prop_BL_med_diuretic_thiazid = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$BL_med_diuretic_thiazid))*100)
bca_prop_BL_med_diuretic_thiazid <- bca(boot_prop_BL_med_diuretic_thiazid$X1)
boot_prop_BL_med_diuretic_thiazid_bca <- t(data.frame(bca_prop_BL_med_diuretic_thiazid))
boot_prop_BL_med_diuretic_aldost = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$BL_med_diuretic_aldost))*100)
bca_prop_BL_med_diuretic_aldost <- bca(boot_prop_BL_med_diuretic_aldost$X1)
boot_prop_BL_med_diuretic_aldost_bca <- t(data.frame(bca_prop_BL_med_diuretic_aldost))
boot_prop_BL_med_diuretic_loop = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$BL_med_diuretic_loop))*100)
bca_prop_BL_med_diuretic_loop <- bca(boot_prop_BL_med_diuretic_loop$X1)
boot_prop_BL_med_diuretic_loop_bca <- t(data.frame(bca_prop_BL_med_diuretic_loop))
boot_prop_BL_med_caanta = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$BL_med_caanta))*100)
bca_prop_BL_med_caanta <- bca(boot_prop_BL_med_caanta$X1)
boot_prop_BL_med_caanta_bca <- t(data.frame(bca_prop_BL_med_caanta))
boot_prop_BL_med_bblocker = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$BL_med_bblocker))*100)
bca_prop_BL_med_bblocker <- bca(boot_prop_BL_med_bblocker$X1)
boot_prop_BL_med_bblocker_bca <- t(data.frame(bca_prop_BL_med_bblocker))
boot_prop_biopsy = do(10000)*(prop.table(table(mosaic::resample(GCKD_df1_o)$biopsy))*100)
bca_prop_biopsy <- bca(boot_prop_biopsy$X1)
boot_prop_biopsy_bca <- t(data.frame(bca_prop_biopsy))

boot_prop_bca <- rbind(boot_prop_stroke_bca, boot_prop_myocard_bca, boot_prop_hypertens_bca, boot_prop_diabetes_bca, 
                       boot_prop_renal_bca, boot_prop_renal_stones_bca, boot_prop_dialyse_bca, boot_prop_ntx_bca, 
                       boot_prop_smoking_1_bca, boot_prop_smoking_2_bca, boot_prop_smoking_3_bca, boot_prop_hospital_bca, 
                       boot_prop_BL_med_raas_ace_bca, boot_prop_BL_med_raas_at1_bca, boot_prop_BL_med_raas_single_bca, 
                       boot_prop_BL_med_raas_double_bca, boot_prop_BL_med_diuretic_bca, boot_prop_BL_med_diuretic_thiazid_bca, 
                       boot_prop_BL_med_diuretic_aldost_bca, boot_prop_BL_med_diuretic_loop_bca, boot_prop_BL_med_caanta_bca, 
                       boot_prop_BL_med_bblocker_bca, boot_prop_biopsy_bca)
write.csv(boot_prop_bca, "GCKD_df1_origin_bca.csv")

confint(boot_prop_stroke, level = 0.95)
ggplot(boot_prop_stroke) + 
  geom_density(aes(x=X1))

col_cat_table_1 <- c("dem_sex", "aa_stroke", "aa_myocard", "aa_hypertens", "aa_diabetes", "aa_renal", "aa_renal_stones", 
                     "aa_dialyse", 
                     "aa_ntx", "smoking", "hospital", "BL_med_raas_ace", "BL_med_raas_at1", "BL_med_raas_single", 
                     "BL_med_raas_double", "BL_med_diuretic", "BL_med_diuretic_thiazid", "BL_med_diuretic_aldost", 
                     "BL_med_diuretic_loop", 
                     "BL_med_caanta", "BL_med_bblocker", "biopsy")

for(i in col_cat_table_1){
  boot_prop[[i]] = do(10)*(prop.table(table(mosaic::resample(GCKD_df1_o)[[i]]))*100)
  print(i)
  print(confint(boot_prop, level = 0.95))
}

chisq.test(table(GCKD_df1[[i]], GCKD_df1_o[[i]]))

## Bootstrapping with package

fc_count <- function(d, i) {
  d2 <- d[i,]
  return(table(d2$dem_sex))
}


boot_gender_1 <- boot(data=GCKD_df1, fc_count, R=10000)

boot.ci(boot_gender_1, type = "bca")

## Chi2 for categorical variables (no multiple comparison), t-test for numerical (parametrisch, no Mann Whitney U)
GCKD_df1_o <- as_tibble(read.xlsx("GCKD_df1_origin.xlsx", sep = ";"))

## Data Type


### Table 1
col_cat_table_1 <- c("dem_sex", "aa_stroke", "aa_myocard", "aa_hypertens", "aa_diabetes", "aa_renal", "aa_renal_stones", "aa_dialyse", 
             "aa_ntx", "smoking", "hospital", "BL_med_raas_ace", "BL_med_raas_at1", "BL_med_raas_single", 
             "BL_med_raas_double", "BL_med_diuretic", "BL_med_diuretic_thiazid", "BL_med_diuretic_aldost", "BL_med_diuretic_loop", 
             "BL_med_caanta", "BL_med_bblocker", "biopsy")
col_num_table_1 <- c("BL_ku_ruhepuls", "BL_ku_sys", "BL_ku_dia", "BL_ku_map", "BL_creavalue", "BL_cysvalue", 
             "BL_gfr_mdrd", "BL_uacr")

for(i in col_cat_table_1){
  chi2 <- chisq.test(table(GCKD_df1[[i]], GCKD_df1_o[[i]]))  
  print(chi2$p.value)
}
for(i in col_num_table_1){
  ttest <- t.test(GCKD_df1[[i]], GCKD_df1_o[[i]], paired = TRUE)
  print(ttest$p.value)
}
for(i in col_cat_table_1){
  chi2 <- chisq.test(table(GCKD_df1_fem[[i]], GCKD_df1_o_fem[[i]]))  
  print(chi2$p.value)
}
for(i in col_num_table_1){
  ttest <- t.test(GCKD_df1_fem[[i]], GCKD_df1_o_fem[[i]], paired = TRUE)
  print(ttest$p.value)
}

chisq.test(table(GCKD_df1_fem$dem_sex, GCKD_df1_o_fem$dem_sex))

col_table_1 <- vector("character")
p_table_1 <- vector("numeric")

for(i in col_cat_table_1){
  chi2 <- chisq.test(table(GCKD_df1[[i]], GCKD_df1_o[[i]]))  
  print(i)
  print(chi2$p.value)
}


chisq.test(table(GCKD_df1$dem_sex, GCKD_df1_o$dem_sex))

for(i in colnames(GCKD_df1)){
  if(i %in% col_cat_table_1 == TRUE){
    output <- chisq.test(table(GCKD_df1[[i]], GCKD_df1_o[[i]]))
    print(i)
    print(output$p.value)
    print(chisq.test(table(GCKD_df1[[i]], GCKD_df1_o[[i]])))
  }else{
    print(i)
    print("Numerical")
  }
}
   