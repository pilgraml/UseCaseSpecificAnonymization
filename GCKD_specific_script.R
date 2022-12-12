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
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_specific_k2.xlsx", sep = ";"))
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_specific_k5.xlsx", sep = ";"))
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_specific_k11.xlsx", sep = ";"))
GCKD_df1 <- as_tibble(read.xlsx("GCKD_specific_k11k2.xlsx", sep = ";"))
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
             "pavk_surgery", "incl_egfr", "education", "BL_age", "BL_ku_bmi", "BL_ku_weight")
GCKD_df1 <- GCKD_df1 %>% mutate(across(all_of(col_cat), as.character))
col_num <- c("BL_ku_sys", "BL_ku_dia", "BL_ku_map", "BL_ku_ruhepuls", "BL_creavalue", "BL_cysvalue", 
             "BL_gfr_mdrd", "BL_gfr_ckdepi", "BL_gfr_ckdepi_cys", "BL_gfr_ckdepi_creacys", "BL_uacr")
GCKD_df1 <- GCKD_df1 %>% mutate(across(all_of(col_num), as.numeric))
GCKD_df1 <- GCKD_df1 %>% mutate(DM = ifelse(GCKD_df1$diabetes == 2, "No DM", ifelse(GCKD_df1$ckd_lead == "ckd_diab", "DMwDN", "DMwoDN")))



# Titze et al 
# Titze et al: Disease burden and risk profile in referred patients with moderate chronic kidney disease: composition of the German Chronic Kidney Disease (GCKD) cohort
## Table 1 including 95% CI for distribution or mean: apart from QI identical to original
var_table_1_red <- c("dem_sex", "biopsy")
theme_gtsummary_language("en", big.mark = "")
GCKD_df1_table_1a <-
  GCKD_df1 %>%
  select(all_of(var_table_1_red)) %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean}",
                     all_categorical() ~ "{n}"),
    digits = all_continuous() ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_0 = "n or mean") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_1b <-
  GCKD_df1 %>%
  select(all_of(var_table_1_red)) %>%
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
  gt::gtsave(filename = "GCKD_specific_k11k2_table_1.html", 
             path = path_results)
### calculated values in Table 1: apart from bmi groups identical to original
table(GCKD_df1$BL_ku_bmi == "[30, 35[" | GCKD_df1$BL_ku_bmi == "[35, 40[" | GCKD_df1$BL_ku_bmi == "[40, 70[",  useNA = "always")
BinomCI(x=sum(GCKD_df1$BL_ku_bmi == "[30, 35[" | GCKD_df1$BL_ku_bmi == "[35, 40[" | GCKD_df1$BL_ku_bmi == "[40, 70[", na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$BL_ku_bmi,  useNA = "always")
BinomCI(x=sum(GCKD_df1$BL_ku_bmi == "[25, 30[", na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_ku_bmi == "[18.5, 25[", na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
### illustration of parameters with change in datatype: age, bmi, weight
GCKD_df1_QI_cat <- select(GCKD_df1, BL_age, BL_ku_bmi, BL_ku_weight)
#### k11
GCKD_df1_QI_cat$BL_ku_weight <- factor(GCKD_df1_QI_cat$BL_ku_weight, levels = c("[40, 80[", "[80, 120[", "[120, 160["))
#### k11k2
GCKD_df1_QI_cat$BL_ku_weight <- factor(GCKD_df1_QI_cat$BL_ku_weight, levels = c("[40, 50[", "[50, 60[", "[60, 70[", "[70, 80[", "[80, 90[", "[90, 100[", "[100, 110[", "[110, 120[", "[120, 130[", "[130, 140[", "[140, 150[", "[150, 160[", "[160, 170["))
#### all
for(i in 1:ncol((GCKD_df1_QI_cat))) {
  x = colnames(GCKD_df1_QI_cat)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_QI_cat, !is.na(GCKD_df1_QI_cat[i])), colour="white", fill="azure4")
  print(Fig)
}
for(i in 1:ncol((GCKD_df1_QI_cat))) {
  x = colnames(GCKD_df1_QI_cat)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_QI_cat, !is.na(GCKD_df1_QI_cat[i])), colour="white", fill="azure4") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  print(Fig)
}
### calculation of height
GCKD_df1_height <- select(GCKD_df1, BL_ku_bmi, BL_ku_weight, dem_sex, diabetes, DM)
#### define new columns with lower and upper bound respectively
GCKD_df1_height[c("BMI_low", "BMI_up")] <- str_split_fixed(GCKD_df1_height$BL_ku_bmi, ", ", 2)
GCKD_df1_height$BMI_low <- gsub("^.", "", as.character(GCKD_df1_height$BMI_low))
GCKD_df1_height$BMI_low <- as.numeric(GCKD_df1_height$BMI_low)
GCKD_df1_height$BMI_up <- gsub(".$", "", as.character(GCKD_df1_height$BMI_up))
GCKD_df1_height$BMI_up <- as.numeric(GCKD_df1_height$BMI_up)
GCKD_df1_height[c("weight_low", "weight_up")] <- str_split_fixed(GCKD_df1_height$BL_ku_weight, ", ", 2)
GCKD_df1_height$weight_low <- gsub("^.", "", as.character(GCKD_df1_height$weight_low))
GCKD_df1_height$weight_low <- as.numeric(GCKD_df1_height$weight_low)
GCKD_df1_height$weight_up <- gsub(".$", "", as.character(GCKD_df1_height$weight_up))
GCKD_df1_height$weight_up <- as.numeric(GCKD_df1_height$weight_up)
#### calculate and illustrate height out of weight & bmi
GCKD_df1_height$height_cm_low_calc <- as.numeric(sqrt(GCKD_df1_height$weight_low/GCKD_df1_height$BMI_up)*100)
GCKD_df1_height$height_cm_low_calc <- round(GCKD_df1_height$height_cm_low_calc ,digit=1)
GCKD_df1_height$height_cm_up_calc <- as.numeric(sqrt(GCKD_df1_height$weight_up/GCKD_df1_height$BMI_low)*100)
GCKD_df1_height$height_cm_up_calc <- round(GCKD_df1_height$height_cm_up_calc ,digit=1)
table(GCKD_df1_height$height_cm_low_calc, GCKD_df1_height$height_cm_up_calc, useNA = "always")
#### k11
GCKD_df1_height <- GCKD_df1_height %>% 
  mutate(height_cm_group = ifelse(height_cm_low_calc == 106.9 & height_cm_up_calc == 163.3, 1, 
                                  ifelse(height_cm_low_calc == 106.9 & height_cm_up_calc == 173.2, 2,
                                         ifelse(height_cm_low_calc == 115.5, 3, 
                                                ifelse(height_cm_low_calc == 126.5, 4, 
                                                       ifelse(height_cm_low_calc == 130.9, 5,
                                                              ifelse(height_cm_low_calc == 141.4, 6,
                                                                     ifelse(height_cm_low_calc == 151.2, 7,
                                                                            ifelse(height_cm_low_calc == 163.3, 8,
                                                                                   ifelse(height_cm_low_calc == 173.2, 9,
                                                                                          ifelse(height_cm_low_calc == 178.9, 10, 9999)))))))))))

#### k11k2
GCKD_df1_height <- GCKD_df1_height %>% 
  mutate(height_cm_group = ifelse(height_cm_low_calc == 113.4, 1, 
                                  ifelse(height_cm_low_calc == 119.5, 2, 
                                         ifelse(height_cm_low_calc == 125.4, 3, 
                                                ifelse(height_cm_low_calc == 126.5, 4, 
                                                       ifelse(height_cm_low_calc == 129.1, 5,
                                                              ifelse(height_cm_low_calc == 130.9, 6,
                                                                     ifelse(height_cm_low_calc == 136.3, 7,
                                                                            ifelse(height_cm_low_calc == 141.4 & height_cm_up_calc == 160.4, 8,
                                                                                   ifelse(height_cm_low_calc == 141.4 & height_cm_up_calc == 163.3, 9,
                                                                                          ifelse(height_cm_low_calc == 141.4 & height_cm_up_calc == 167.3, 10, 
                                                                                                 ifelse(height_cm_low_calc == 141.4 & height_cm_up_calc == 180.1, 11, 
                                                                                                        ifelse(height_cm_low_calc == 141.4 & height_cm_up_calc == 193.6, 12,
                                                                                                               ifelse(height_cm_low_calc == 146.4, 13,
                                                                                                                      ifelse(height_cm_low_calc == 150, 14,
                                                                                                                             ifelse(height_cm_low_calc == 151.2 & height_cm_up_calc == 173.2, 15,
                                                                                                                                    ifelse(height_cm_low_calc == 151.2 & height_cm_up_calc == 206.2, 16, 
                                                                                                                                           ifelse(height_cm_low_calc == 152.8, 17, 
                                                                                                                                                  ifelse(height_cm_low_calc == 154.9, 18, 
                                                                                                                                                         ifelse(height_cm_low_calc == 158.1, 19, 
                                                                                                                                                                ifelse(height_cm_low_calc == 160.4, 20, 
                                                                                                                                                                       ifelse(height_cm_low_calc == 163.3, 21, 
                                                                                                                                                                              ifelse(height_cm_low_calc == 165.8, 22, 
                                                                                                                                                                                     ifelse(height_cm_low_calc == 167.3, 23, 
                                                                                                                                                                                            ifelse(height_cm_low_calc == 169, 24, 
                                                                                                                                                                                                   ifelse(height_cm_low_calc == 173.2  & height_cm_up_calc == 192.7, 25, 
                                                                                                                                                                                                          ifelse(height_cm_low_calc == 173.2  & height_cm_up_calc == 200, 26,
                                                                                                                                                                                                                 ifelse(height_cm_low_calc == 177.3, 27, 
                                                                                                                                                                                                                        ifelse(height_cm_low_calc == 178.9, 28, 
                                                                                                                                                                                                                               ifelse(height_cm_low_calc == 180.3, 29,
                                                                                                                                                                                                                                      ifelse(height_cm_low_calc == 182.6, 30,
                                                                                                                                                                                                                                             ifelse(height_cm_low_calc == 185.2, 31, 9999))))))))))))))))))))))))))))))))
#### all
table(GCKD_df1_height$height_cm_group, useNA = "always")
GCKD_df1_height$ID <- seq.int(nrow(GCKD_df1_height))
GCKD_df1_height_cm_low <- GCKD_df1_height[,c("height_cm_low_calc", "ID", "height_cm_group")]
GCKD_df1_height_cm_low$bound = "low"
names(GCKD_df1_height_cm_low)[names(GCKD_df1_height_cm_low) == "height_cm_low_calc"] <- "height_cm"
GCKD_df1_height_cm_up <- GCKD_df1_height[,c("height_cm_up_calc", "ID", "height_cm_group")]
GCKD_df1_height_cm_up$bound = "up"
names(GCKD_df1_height_cm_up)[names(GCKD_df1_height_cm_up) == "height_cm_up_calc"] <- "height_cm"
GCKD_df1_height_cm <- rbind(GCKD_df1_height_cm_low, GCKD_df1_height_cm_up)
ggplot(data=GCKD_df1_height_cm, aes(x = height_cm, y = height_cm_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3)
ggplot(data=GCKD_df1_height_cm, aes(x = height_cm, y = height_cm_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

### Table 1 subset female
var_table_1 <- c("dem_sex", "aa_stroke", "aa_myocard", "aa_hypertens", "aa_diabetes", "aa_renal", 
                 "aa_renal_stones", "aa_dialyse", "aa_ntx", "smoking", "hospital", 
                 "BL_ku_sys", "BL_ku_dia", "BL_ku_map", "BL_ku_ruhepuls", "BL_creavalue", "BL_cysvalue",
                 "BL_gfr_mdrd", "BL_med_raas_ace", "BL_med_raas_at1", "BL_med_raas_double", 
                 "BL_med_raas_single", "BL_med_diuretic", "BL_med_diuretic_thiazid", "BL_med_diuretic_aldost", 
                 "BL_med_diuretic_loop", "BL_med_caanta", "BL_med_bblocker", "biopsy", "diabetes")
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
  add_ci(statistic=list(all_categorical() ~ "{conf.low}-{conf.high}", all_continuous() ~ "{conf.low}-{conf.high}"),
         style_fun=list(all_categorical() ~ purrr::partial(style_sigfig, digits=3, scale = 100), 
                        all_continuous() ~ purrr::partial(style_sigfig, digits=3))) %>%
  modify_header(label = "Variable", stat_1 = "Diabetics; SD or %", ci_stat_1 = "Diabetics; 95% CI", stat_2 = "Non-diabetics; SD or %", ci_stat_2 = "Non-diabetics; 95% CI") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_fem_table_1 <-
  tbl_merge(tbls = list(GCKD_df1_fem_table_1a, GCKD_df1_fem_table_1b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_fem_table_1 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_specific_k11k2_table_1_fem.html", 
             path = path_results)
### calculated values in Table 1 subset female
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi,  useNA = "always")
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi == "[30, 35[" | GCKD_df1_fem$BL_ku_bmi == "[35, 40[" | GCKD_df1_fem$BL_ku_bmi == "[40, 70[",  useNA = "always")
BinomCI(x=sum(GCKD_df1_fem$diabetes == "1" & (GCKD_df1_fem$BL_ku_bmi == "[30, 35[" | GCKD_df1_fem$BL_ku_bmi == "[35, 40[" | GCKD_df1_fem$BL_ku_bmi == "[40, 70["), na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes == "1" & !is.na(GCKD_df1_fem$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem$diabetes == "1" & GCKD_df1_fem$BL_ku_bmi == "[25, 30[", na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes == "1" & !is.na(GCKD_df1_fem$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem$diabetes == "1" & GCKD_df1_fem$BL_ku_bmi == "[18.5, 25[", na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes == "1" & !is.na(GCKD_df1_fem$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem$diabetes == "2" & (GCKD_df1_fem$BL_ku_bmi == "[30, 35[" | GCKD_df1_fem$BL_ku_bmi == "[35, 40[" | GCKD_df1_fem$BL_ku_bmi == "[40, 70["), na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes == "2" & !is.na(GCKD_df1_fem$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem$diabetes == "2" & GCKD_df1_fem$BL_ku_bmi == "[25, 30[", na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes == "2" & !is.na(GCKD_df1_fem$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem$diabetes == "2" & GCKD_df1_fem$BL_ku_bmi == "[18.5, 25[", na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes == "2" & !is.na(GCKD_df1_fem$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_sys < 130 & GCKD_df1_fem$BL_ku_dia < 80,  useNA = "always")
BinomCI(x=sum(GCKD_df1_fem$diabetes =="1" & GCKD_df1_fem$BL_ku_sys < 130 & GCKD_df1_fem$BL_ku_dia < 80, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes == "1" & !is.na(GCKD_df1_fem$BL_ku_sys)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem$diabetes =="2" & GCKD_df1_fem$BL_ku_sys < 130 & GCKD_df1_fem$BL_ku_dia < 80, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes == "2" & !is.na(GCKD_df1_fem$BL_ku_sys)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_sys < 140 & GCKD_df1_fem$BL_ku_dia < 90,  useNA = "always")
BinomCI(x=sum(GCKD_df1_fem$diabetes =="1" & GCKD_df1_fem$BL_ku_sys < 140 & GCKD_df1_fem$BL_ku_dia < 90, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes == "1" & !is.na(GCKD_df1_fem$BL_ku_sys)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem$diabetes =="2" & GCKD_df1_fem$BL_ku_sys < 140 & GCKD_df1_fem$BL_ku_dia < 90, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes == "2" & !is.na(GCKD_df1_fem$BL_ku_sys)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd >= 60,  useNA = "always")
BinomCI(x=sum(GCKD_df1_fem$diabetes =="1" & GCKD_df1_fem$BL_gfr_mdrd >= 60, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes =="1" & !is.na(GCKD_df1_fem$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem$diabetes =="2" & GCKD_df1_fem$BL_gfr_mdrd >= 60, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes =="2" & !is.na(GCKD_df1_fem$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd >= 45 & GCKD_df1_fem$BL_gfr_mdrd < 60,  useNA = "always")
BinomCI(x=sum(GCKD_df1_fem$diabetes =="1" & GCKD_df1_fem$BL_gfr_mdrd >= 45 & GCKD_df1_fem$BL_gfr_mdrd < 60, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes =="1" & !is.na(GCKD_df1_fem$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem$diabetes =="2" & GCKD_df1_fem$BL_gfr_mdrd >= 45 & GCKD_df1_fem$BL_gfr_mdrd < 60, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes =="2" & !is.na(GCKD_df1_fem$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd >= 30 & GCKD_df1_fem$BL_gfr_mdrd < 45,  useNA = "always")
BinomCI(x=sum(GCKD_df1_fem$diabetes =="1" & GCKD_df1_fem$BL_gfr_mdrd >= 30 & GCKD_df1_fem$BL_gfr_mdrd < 45, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes =="1" & !is.na(GCKD_df1_fem$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem$diabetes =="2" & GCKD_df1_fem$BL_gfr_mdrd >= 30 & GCKD_df1_fem$BL_gfr_mdrd < 45, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes =="2" & !is.na(GCKD_df1_fem$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd < 30,  useNA = "always")
BinomCI(x=sum(GCKD_df1_fem$diabetes =="1" & GCKD_df1_fem$BL_gfr_mdrd < 30, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes =="1" & !is.na(GCKD_df1_fem$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem$diabetes =="2" & GCKD_df1_fem$BL_gfr_mdrd < 30, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes =="2" & !is.na(GCKD_df1_fem$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
GCKD_df1_fem %>% group_by(diabetes) %>%
  summarise_at(vars(BL_uacr), list(~ round(quantile(., na.rm=TRUE), 1)))
GCKD_df1_fem_d <- GCKD_df1_fem %>% subset(diabetes == "1")
GCKD_df1_fem_nd <- GCKD_df1_fem %>% subset(diabetes == "2")
MedianCI(GCKD_df1_fem_d$BL_uacr, na.rm=TRUE)
MedianCI(GCKD_df1_fem_nd$BL_uacr, na.rm=TRUE)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_uacr < 30,  useNA = "always")
BinomCI(x=sum(GCKD_df1_fem$diabetes =="1" & GCKD_df1_fem$BL_uacr < 30, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes =="1" & !is.na(GCKD_df1_fem$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem$diabetes =="2" & GCKD_df1_fem$BL_uacr < 30, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes =="2" & !is.na(GCKD_df1_fem$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_uacr >= 30 & GCKD_df1_fem$BL_uacr <= 300,  useNA = "always")
BinomCI(x=sum(GCKD_df1_fem$diabetes =="1" & GCKD_df1_fem$BL_uacr >= 30 & GCKD_df1_fem$BL_uacr <= 300, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes =="1" & !is.na(GCKD_df1_fem$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem$diabetes =="2" & GCKD_df1_fem$BL_uacr >= 30 & GCKD_df1_fem$BL_uacr <= 300, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes =="2" & !is.na(GCKD_df1_fem$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_uacr > 300,  useNA = "always")
BinomCI(x=sum(GCKD_df1_fem$diabetes =="1" & GCKD_df1_fem$BL_uacr > 300, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes =="1" & !is.na(GCKD_df1_fem$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem$diabetes =="2" & GCKD_df1_fem$BL_uacr > 300, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes =="2" & !is.na(GCKD_df1_fem$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
### illustration of parameters with change in datatype: age, height, weight
GCKD_df1_fem_d <- GCKD_df1_fem %>% subset(diabetes == "1")
GCKD_df1_QI_cat_fem_d <- select(GCKD_df1_fem_d, BL_age, BL_ku_bmi, BL_ku_weight)
#### k11
GCKD_df1_QI_cat_fem_d$BL_ku_weight <- factor(GCKD_df1_QI_cat_fem_d$BL_ku_weight, levels = c("[40, 80[", "[80, 120[", "[120, 160["))
#### k11k2
GCKD_df1_QI_cat_fem_d$BL_ku_weight <- factor(GCKD_df1_QI_cat_fem_d$BL_ku_weight, levels = c("[40, 50[", "[50, 60[", "[60, 70[", "[70, 80[", "[80, 90[", "[90, 100[", "[100, 110[", "[110, 120[", "[120, 130[", "[130, 140[", "[140, 150[", "[150, 160[", "[160, 170["))
#### all
for(i in 1:ncol((GCKD_df1_QI_cat_fem_d))) {
  x = colnames(GCKD_df1_QI_cat_fem_d)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_QI_cat_fem_d, !is.na(GCKD_df1_QI_cat_fem_d[i])), colour="white", fill="azure4")
  print(Fig)
}
for(i in 1:ncol((GCKD_df1_QI_cat_fem_d))) {
  x = colnames(GCKD_df1_QI_cat_fem_d)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_QI_cat_fem_d, !is.na(GCKD_df1_QI_cat_fem_d[i])), colour="white", fill="azure4") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  print(Fig)
}
GCKD_df1_fem_nd <- GCKD_df1_fem %>% subset(diabetes == "2")
GCKD_df1_QI_cat_fem_nd <- select(GCKD_df1_fem_nd, BL_age, BL_ku_bmi, BL_ku_weight)
#### k11
GCKD_df1_QI_cat_fem_nd$BL_ku_weight <- factor(GCKD_df1_QI_cat_fem_nd$BL_ku_weight, levels = c("[40, 80[", "[80, 120[", "[120, 160["))
#### k11k2
GCKD_df1_QI_cat_fem_nd$BL_ku_weight <- factor(GCKD_df1_QI_cat_fem_nd$BL_ku_weight, levels = c("[40, 50[", "[50, 60[", "[60, 70[", "[70, 80[", "[80, 90[", "[90, 100[", "[100, 110[", "[110, 120[", "[120, 130[", "[130, 140[", "[140, 150[", "[150, 160[", "[160, 170["))
#### all
for(i in 1:ncol((GCKD_df1_QI_cat_fem_nd))) {
  x = colnames(GCKD_df1_QI_cat_fem_nd)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_QI_cat_fem_nd, !is.na(GCKD_df1_QI_cat_fem_nd[i])), colour="white", fill="azure4")
  print(Fig)
}
for(i in 1:ncol((GCKD_df1_QI_cat_fem_nd))) {
  x = colnames(GCKD_df1_QI_cat_fem_nd)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_QI_cat_fem_nd, !is.na(GCKD_df1_QI_cat_fem_nd[i])), colour="white", fill="azure4") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  print(Fig)
}
### calculation of height
GCKD_df1_height_fem <- GCKD_df1_height %>% subset(dem_sex == "Female")
GCKD_df1_height_fem_d <- GCKD_df1_height_fem %>% subset(diabetes == "1")
GCKD_df1_height_fem_d$ID <- seq.int(nrow(GCKD_df1_height_fem_d))
GCKD_df1_height_cm_fem_d_low <- GCKD_df1_height_fem_d[,c("height_cm_low_calc", "ID", "height_cm_group")]
GCKD_df1_height_cm_fem_d_low$bound = "low"
names(GCKD_df1_height_cm_fem_d_low)[names(GCKD_df1_height_cm_fem_d_low) == "height_cm_low_calc"] <- "height_cm"
GCKD_df1_height_cm_fem_d_up <- GCKD_df1_height_fem_d[,c("height_cm_up_calc", "ID", "height_cm_group")]
GCKD_df1_height_cm_fem_d_up$bound = "up"
names(GCKD_df1_height_cm_fem_d_up)[names(GCKD_df1_height_cm_fem_d_up) == "height_cm_up_calc"] <- "height_cm"
GCKD_df1_height_cm_fem_d <- rbind(GCKD_df1_height_cm_fem_d_low, GCKD_df1_height_cm_fem_d_up)
ggplot(data=GCKD_df1_height_cm_fem_d, aes(x = height_cm, y = height_cm_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3)
ggplot(data=GCKD_df1_height_cm_fem_d, aes(x = height_cm, y = height_cm_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
GCKD_df1_height_fem_nd <- GCKD_df1_height_fem %>% subset(diabetes == "2")
GCKD_df1_height_fem_nd$ID <- seq.int(nrow(GCKD_df1_height_fem_nd))
GCKD_df1_height_cm_fem_nd_low <- GCKD_df1_height_fem_nd[,c("height_cm_low_calc", "ID", "height_cm_group")]
GCKD_df1_height_cm_fem_nd_low$bound = "low"
names(GCKD_df1_height_cm_fem_nd_low)[names(GCKD_df1_height_cm_fem_nd_low) == "height_cm_low_calc"] <- "height_cm"
GCKD_df1_height_cm_fem_nd_up <- GCKD_df1_height_fem_nd[,c("height_cm_up_calc", "ID", "height_cm_group")]
GCKD_df1_height_cm_fem_nd_up$bound = "up"
names(GCKD_df1_height_cm_fem_nd_up)[names(GCKD_df1_height_cm_fem_nd_up) == "height_cm_up_calc"] <- "height_cm"
GCKD_df1_height_cm_fem_nd <- rbind(GCKD_df1_height_cm_fem_nd_low, GCKD_df1_height_cm_fem_nd_up)
ggplot(data=GCKD_df1_height_cm_fem_nd, aes(x = height_cm, y = height_cm_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3)
ggplot(data=GCKD_df1_height_cm_fem_nd, aes(x = height_cm, y = height_cm_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

### Table 1 subset male
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
  add_ci(statistic=list(all_categorical() ~ "{conf.low}-{conf.high}", all_continuous() ~ "{conf.low}-{conf.high}"),
         style_fun=list(all_categorical() ~ purrr::partial(style_sigfig, digits=3, scale = 100), 
                        all_continuous() ~ purrr::partial(style_sigfig, digits=3))) %>%
  modify_header(label = "Variable", stat_1 = "Diabetics; SD or %", ci_stat_1 = "Diabetics; 95% CI", stat_2 = "Non-diabetics; SD or %", ci_stat_2 = "Non-diabetics; 95% CI") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_male_table_1 <-
  tbl_merge(tbls = list(GCKD_df1_male_table_1a, GCKD_df1_male_table_1b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_male_table_1 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_specific_k11k2_table_1_male.html", 
             path = path_results)
### calculated values in Table 1 subset male
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi,  useNA = "always")
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi == "[30, 35[" | GCKD_df1_male$BL_ku_bmi == "[35, 40[" | GCKD_df1_male$BL_ku_bmi == "[40, 70[",  useNA = "always")
BinomCI(x=sum(GCKD_df1_male$diabetes == "1" & (GCKD_df1_male$BL_ku_bmi == "[30, 35[" | GCKD_df1_male$BL_ku_bmi == "[35, 40[" | GCKD_df1_male$BL_ku_bmi == "[40, 70["), na.rm=TRUE), n=sum(GCKD_df1_male$diabetes == "1" & !is.na(GCKD_df1_male$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male$diabetes == "1" & GCKD_df1_male$BL_ku_bmi == "[25, 30[", na.rm=TRUE), n=sum(GCKD_df1_male$diabetes == "1" & !is.na(GCKD_df1_male$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male$diabetes == "1" & GCKD_df1_male$BL_ku_bmi == "[18.5, 25[", na.rm=TRUE), n=sum(GCKD_df1_male$diabetes == "1" & !is.na(GCKD_df1_male$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male$diabetes == "2" & (GCKD_df1_male$BL_ku_bmi == "[30, 35[" | GCKD_df1_male$BL_ku_bmi == "[35, 40[" | GCKD_df1_male$BL_ku_bmi == "[40, 70["), na.rm=TRUE), n=sum(GCKD_df1_male$diabetes == "2" & !is.na(GCKD_df1_male$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male$diabetes == "2" & GCKD_df1_male$BL_ku_bmi == "[25, 30[", na.rm=TRUE), n=sum(GCKD_df1_male$diabetes == "2" & !is.na(GCKD_df1_male$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male$diabetes == "2" & GCKD_df1_male$BL_ku_bmi == "[18.5, 25[", na.rm=TRUE), n=sum(GCKD_df1_male$diabetes == "2" & !is.na(GCKD_df1_male$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_sys < 130 & GCKD_df1_male$BL_ku_dia < 80,  useNA = "always")
BinomCI(x=sum(GCKD_df1_male$diabetes =="1" & GCKD_df1_male$BL_ku_sys < 130 & GCKD_df1_male$BL_ku_dia < 80, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes == "1" & !is.na(GCKD_df1_male$BL_ku_sys)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male$diabetes =="2" & GCKD_df1_male$BL_ku_sys < 130 & GCKD_df1_male$BL_ku_dia < 80, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes == "2" & !is.na(GCKD_df1_male$BL_ku_sys)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_sys < 140 & GCKD_df1_male$BL_ku_dia < 90,  useNA = "always")
BinomCI(x=sum(GCKD_df1_male$diabetes =="1" & GCKD_df1_male$BL_ku_sys < 140 & GCKD_df1_male$BL_ku_dia < 90, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes == "1" & !is.na(GCKD_df1_male$BL_ku_sys)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male$diabetes =="2" & GCKD_df1_male$BL_ku_sys < 140 & GCKD_df1_male$BL_ku_dia < 90, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes == "2" & !is.na(GCKD_df1_male$BL_ku_sys)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd >= 60,  useNA = "always")
BinomCI(x=sum(GCKD_df1_male$diabetes =="1" & GCKD_df1_male$BL_gfr_mdrd >= 60, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes =="1" & !is.na(GCKD_df1_male$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male$diabetes =="2" & GCKD_df1_male$BL_gfr_mdrd >= 60, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes =="2" & !is.na(GCKD_df1_male$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd >= 45 & GCKD_df1_male$BL_gfr_mdrd < 60,  useNA = "always")
BinomCI(x=sum(GCKD_df1_male$diabetes =="1" & GCKD_df1_male$BL_gfr_mdrd >= 45 & GCKD_df1_male$BL_gfr_mdrd < 60, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes =="1" & !is.na(GCKD_df1_male$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male$diabetes =="2" & GCKD_df1_male$BL_gfr_mdrd >= 45 & GCKD_df1_male$BL_gfr_mdrd < 60, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes =="2" & !is.na(GCKD_df1_male$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd >= 30 & GCKD_df1_male$BL_gfr_mdrd < 45,  useNA = "always")
BinomCI(x=sum(GCKD_df1_male$diabetes =="1" & GCKD_df1_male$BL_gfr_mdrd >= 30 & GCKD_df1_male$BL_gfr_mdrd < 45, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes =="1" & !is.na(GCKD_df1_male$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male$diabetes =="2" & GCKD_df1_male$BL_gfr_mdrd >= 30 & GCKD_df1_male$BL_gfr_mdrd < 45, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes =="2" & !is.na(GCKD_df1_male$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd < 30,  useNA = "always")
BinomCI(x=sum(GCKD_df1_male$diabetes =="1" & GCKD_df1_male$BL_gfr_mdrd < 30, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes =="1" & !is.na(GCKD_df1_male$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male$diabetes =="2" & GCKD_df1_male$BL_gfr_mdrd < 30, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes =="2" & !is.na(GCKD_df1_male$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
GCKD_df1_male %>% group_by(diabetes) %>%
  summarise_at(vars(BL_uacr), list(~ round(quantile(., na.rm=TRUE), 1)))
GCKD_df1_male_d <- GCKD_df1_male %>% subset(diabetes == "1")
GCKD_df1_male_nd <- GCKD_df1_male %>% subset(diabetes == "2")
MedianCI(GCKD_df1_male_d$BL_uacr, na.rm=TRUE)
MedianCI(GCKD_df1_male_nd$BL_uacr, na.rm=TRUE)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_uacr < 30,  useNA = "always")
BinomCI(x=sum(GCKD_df1_male$diabetes =="1" & GCKD_df1_male$BL_uacr < 30, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes =="1" & !is.na(GCKD_df1_male$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male$diabetes =="2" & GCKD_df1_male$BL_uacr < 30, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes =="2" & !is.na(GCKD_df1_male$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_uacr >= 30 & GCKD_df1_male$BL_uacr <= 300,  useNA = "always")
BinomCI(x=sum(GCKD_df1_male$diabetes =="1" & GCKD_df1_male$BL_uacr >= 30 & GCKD_df1_male$BL_uacr <= 300, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes =="1" & !is.na(GCKD_df1_male$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male$diabetes =="2" & GCKD_df1_male$BL_uacr >= 30 & GCKD_df1_male$BL_uacr <= 300, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes =="2" & !is.na(GCKD_df1_male$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_uacr > 300,  useNA = "always")
BinomCI(x=sum(GCKD_df1_male$diabetes =="1" & GCKD_df1_male$BL_uacr > 300, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes =="1" & !is.na(GCKD_df1_male$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male$diabetes =="2" & GCKD_df1_male$BL_uacr > 300, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes =="2" & !is.na(GCKD_df1_male$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
### illustration of parameters with change in datatype: age, height, weight
GCKD_df1_male_d <- GCKD_df1_male %>% subset(diabetes == "1")
GCKD_df1_QI_cat_male_d <- select(GCKD_df1_male_d, BL_age, BL_ku_bmi, BL_ku_weight)
#### k11
GCKD_df1_QI_cat_male_d$BL_ku_weight <- factor(GCKD_df1_QI_cat_male_d$BL_ku_weight, levels = c("[40, 80[", "[80, 120[", "[120, 160["))
#### k11k2
GCKD_df1_QI_cat_male_d$BL_ku_weight <- factor(GCKD_df1_QI_cat_male_d$BL_ku_weight, levels = c("[40, 50[", "[50, 60[", "[60, 70[", "[70, 80[", "[80, 90[", "[90, 100[", "[100, 110[", "[110, 120[", "[120, 130[", "[130, 140[", "[140, 150[", "[150, 160[", "[160, 170["))
#### all
for(i in 1:ncol((GCKD_df1_QI_cat_male_d))) {
  x = colnames(GCKD_df1_QI_cat_male_d)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_QI_cat_male_d, !is.na(GCKD_df1_QI_cat_male_d[i])), colour="white", fill="azure4")
  print(Fig)
}
for(i in 1:ncol((GCKD_df1_QI_cat_male_d))) {
  x = colnames(GCKD_df1_QI_cat_male_d)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_QI_cat_male_d, !is.na(GCKD_df1_QI_cat_male_d[i])), colour="white", fill="azure4") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  print(Fig)
}
GCKD_df1_male_nd <- GCKD_df1_male %>% subset(diabetes == "2")
GCKD_df1_QI_cat_male_nd <- select(GCKD_df1_male_nd, BL_age, BL_ku_bmi, BL_ku_weight)
#### k11
GCKD_df1_QI_cat_male_nd$BL_ku_weight <- factor(GCKD_df1_QI_cat_male_nd$BL_ku_weight, levels = c("[40, 80[", "[80, 120[", "[120, 160["))
#### k11k2
GCKD_df1_QI_cat_male_nd$BL_ku_weight <- factor(GCKD_df1_QI_cat_male_nd$BL_ku_weight, levels = c("[40, 50[", "[50, 60[", "[60, 70[", "[70, 80[", "[80, 90[", "[90, 100[", "[100, 110[", "[110, 120[", "[120, 130[", "[130, 140[", "[140, 150[", "[150, 160[", "[160, 170["))
#### all
for(i in 1:ncol((GCKD_df1_QI_cat_male_nd))) {
  x = colnames(GCKD_df1_QI_cat_male_nd)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_QI_cat_male_nd, !is.na(GCKD_df1_QI_cat_male_nd[i])), colour="white", fill="azure4")
  print(Fig)
}
for(i in 1:ncol((GCKD_df1_QI_cat_male_nd))) {
  x = colnames(GCKD_df1_QI_cat_male_nd)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_QI_cat_male_nd, !is.na(GCKD_df1_QI_cat_male_nd[i])), colour="white", fill="azure4") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  print(Fig)
}
### calculation of height
GCKD_df1_height_male <- GCKD_df1_height %>% subset(dem_sex == "Male")
GCKD_df1_height_male_d <- GCKD_df1_height_male %>% subset(diabetes == "1")
GCKD_df1_height_male_d$ID <- seq.int(nrow(GCKD_df1_height_male_d))
GCKD_df1_height_cm_male_d_low <- GCKD_df1_height_male_d[,c("height_cm_low_calc", "ID", "height_cm_group")]
GCKD_df1_height_cm_male_d_low$bound = "low"
names(GCKD_df1_height_cm_male_d_low)[names(GCKD_df1_height_cm_male_d_low) == "height_cm_low_calc"] <- "height_cm"
GCKD_df1_height_cm_male_d_up <- GCKD_df1_height_male_d[,c("height_cm_up_calc", "ID", "height_cm_group")]
GCKD_df1_height_cm_male_d_up$bound = "up"
names(GCKD_df1_height_cm_male_d_up)[names(GCKD_df1_height_cm_male_d_up) == "height_cm_up_calc"] <- "height_cm"
GCKD_df1_height_cm_male_d <- rbind(GCKD_df1_height_cm_male_d_low, GCKD_df1_height_cm_male_d_up)
ggplot(data=GCKD_df1_height_cm_male_d, aes(x = height_cm, y = height_cm_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3)
ggplot(data=GCKD_df1_height_cm_male_d, aes(x = height_cm, y = height_cm_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
GCKD_df1_height_male_nd <- GCKD_df1_height_male %>% subset(diabetes == "2")
GCKD_df1_height_male_nd$ID <- seq.int(nrow(GCKD_df1_height_male_nd))
GCKD_df1_height_cm_male_nd_low <- GCKD_df1_height_male_nd[,c("height_cm_low_calc", "ID", "height_cm_group")]
GCKD_df1_height_cm_male_nd_low$bound = "low"
names(GCKD_df1_height_cm_male_nd_low)[names(GCKD_df1_height_cm_male_nd_low) == "height_cm_low_calc"] <- "height_cm"
GCKD_df1_height_cm_male_nd_up <- GCKD_df1_height_male_nd[,c("height_cm_up_calc", "ID", "height_cm_group")]
GCKD_df1_height_cm_male_nd_up$bound = "up"
names(GCKD_df1_height_cm_male_nd_up)[names(GCKD_df1_height_cm_male_nd_up) == "height_cm_up_calc"] <- "height_cm"
GCKD_df1_height_cm_male_nd <- rbind(GCKD_df1_height_cm_male_nd_low, GCKD_df1_height_cm_male_nd_up)
ggplot(data=GCKD_df1_height_cm_male_nd, aes(x = height_cm, y = height_cm_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3)
ggplot(data=GCKD_df1_height_cm_male_nd, aes(x = height_cm, y = height_cm_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

## Figure 1: KDIGO categories
### identical to original data: no QI used

## Table 2: Characteristics grouped by inclusion criteria: : apart from QI identical to original 
table(GCKD_df1$incl_egfr)
var_table_2 <- c("dem_sex", "biopsy", "incl_egfr") 
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
  add_ci(statistic=list(all_categorical() ~ "{conf.low}-{conf.high}", all_continuous() ~ "{conf.low}-{conf.high}"),
         style_fun=list(all_categorical() ~ purrr::partial(style_sigfig, digits=3, scale = 100), 
                        all_continuous() ~ purrr::partial(style_sigfig, digits=3))) %>%
  modify_header(label = "Variable", stat_1 = "eGFR 30-60; SD or %", ci_stat_1 = "eGFR 30-60; 95% CI", stat_2 = "Overt proteinuria; SD or %", ci_stat_2 = "Overt proteinuria; 95% CI") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_2 <-
  tbl_merge(tbls = list(GCKD_df1_table_2a, GCKD_df1_table_2b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_table_2 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_specific_k11k2_table_2.html", 
             path = path_results)
### calculated values in Table 2
#### k11
GCKD_df1_less60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[50, 60[" | GCKD_df1$BL_age == "[40, 50[" | GCKD_df1$BL_age == "[30, 40[" | GCKD_df1$BL_age == "[20, 30[")
GCKD_df1_more60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[60, 70[" | GCKD_df1$BL_age == "[70, 80[")
#### k11k2
GCKD_df1_less60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[20, 25[" | GCKD_df1$BL_age == "[25, 30[" | GCKD_df1$BL_age == "[30, 35[" | GCKD_df1$BL_age == "[35, 40[" | GCKD_df1$BL_age == "[40, 45[" | GCKD_df1$BL_age == "[45, 50[" | GCKD_df1$BL_age == "[50, 55[" | GCKD_df1$BL_age == "[55, 60[")
GCKD_df1_more60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[60, 65[" | GCKD_df1$BL_age == "[65, 70[" | GCKD_df1$BL_age == "[70, 75[" | GCKD_df1$BL_age == "[75, 80[")
#### all
table(GCKD_df1_less60$incl_egfr, GCKD_df1_less60$cardiovasc == "1",  useNA = "always")
BinomCI(x=sum(GCKD_df1_less60$incl_egfr == "1" & GCKD_df1_less60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_less60$incl_egfr == "1" & !is.na(GCKD_df1_less60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_less60$incl_egfr == "2" & GCKD_df1_less60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_less60$incl_egfr == "2" & !is.na(GCKD_df1_less60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_more60$incl_egfr, GCKD_df1_more60$cardiovasc == "1",  useNA = "always") 
BinomCI(x=sum(GCKD_df1_more60$incl_egfr == "1" & GCKD_df1_more60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_more60$incl_egfr == "1" & !is.na(GCKD_df1_more60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_more60$incl_egfr == "2" & GCKD_df1_more60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_more60$incl_egfr == "2" & !is.na(GCKD_df1_more60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
GCKD_df1 %>% group_by(incl_egfr) %>%
  summarise_at(vars(BL_uacr), list(~ round(quantile(., na.rm=TRUE), 1)))
GCKD_df1_gfr <- GCKD_df1 %>% subset(incl_egfr == "1")
GCKD_df1_protein <- GCKD_df1 %>% subset(incl_egfr == "2")
MedianCI(GCKD_df1_gfr$BL_uacr, na.rm=TRUE)
MedianCI(GCKD_df1_protein$BL_uacr, na.rm=TRUE)
### illustration of parameters with change in datatype: age
print(GCKD_df1 %>%
        filter(!is.na(BL_age) & incl_egfr == "1") %>%
        ggplot(aes(x=as.factor(BL_age))) +
        geom_bar(color="white", fill="azure4"))
print(GCKD_df1 %>%
        filter(!is.na(BL_age) & incl_egfr == "1") %>%
        ggplot(aes(x=as.factor(BL_age))) +
        geom_bar(color="white", fill="azure4") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))
print(GCKD_df1 %>%
        filter(!is.na(BL_age) & incl_egfr == "2") %>%
        ggplot(aes(x=as.factor(BL_age))) +
        geom_bar(color="white", fill="azure4"))
print(GCKD_df1 %>%
        filter(!is.na(BL_age) & incl_egfr == "2") %>%
        ggplot(aes(x=as.factor(BL_age))) +
        geom_bar(color="white", fill="azure4") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

## Figure 2: Leading causes of CKD
### identical to original data: no QI used

## Table 3: Causes of CKD leading & additional
### apart from biopsy rate identical to original data
var_table_3 <- c("ckd_lead", "biopsy")
theme_gtsummary_language("en", big.mark = "")
GCKD_df1_table_3a <-
  GCKD_df1 %>%
  select(all_of(var_table_3)) %>%
  tbl_summary(
    by = ckd_lead,
    statistic = list(all_categorical() ~ "{n}"),
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_3b <-
  GCKD_df1 %>%
  select(all_of(var_table_3)) %>%
  tbl_summary(
    by = ckd_lead,
    statistic = list(all_categorical() ~ "{p}"),
    digits = ~ 1,
    missing_text = "(Missing)") %>%
  add_ci(statistic=list(all_categorical() ~ "{conf.low}-{conf.high}", all_continuous() ~ "{conf.low}-{conf.high}"),
         style_fun=list(all_categorical() ~ purrr::partial(style_sigfig, digits=3, scale = 100), 
                        all_continuous() ~ purrr::partial(style_sigfig, digits=3))) %>%
  modify_header(label = "Variable") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_3 <-
  tbl_merge(tbls = list(GCKD_df1_table_3a, GCKD_df1_table_3b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_table_3 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_specific_k11k2_table_3.html", 
             path = path_results)

## Figure 3: Patient awareness and treatment
### identical to original data

## Table 4: Cardiovascular disease: identical to original data
### calculated values in Table 4
#### k11
GCKD_df1_less60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[50, 60[" | GCKD_df1$BL_age == "[40, 50[" | GCKD_df1$BL_age == "[30, 40[" | GCKD_df1$BL_age == "[20, 30[")
GCKD_df1_more60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[60, 70[" | GCKD_df1$BL_age == "[70, 80[")
#### k11k2
GCKD_df1_less60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[20, 25[" | GCKD_df1$BL_age == "[25, 30[" | GCKD_df1$BL_age == "[30, 35[" | GCKD_df1$BL_age == "[35, 40[" | GCKD_df1$BL_age == "[40, 45[" | GCKD_df1$BL_age == "[45, 50[" | GCKD_df1$BL_age == "[50, 55[" | GCKD_df1$BL_age == "[55, 60[")
GCKD_df1_more60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[60, 65[" | GCKD_df1$BL_age == "[65, 70[" | GCKD_df1$BL_age == "[70, 75[" | GCKD_df1$BL_age == "[75, 80[")
#### all
table(GCKD_df1_less60$cardiovasc == "1",  useNA = "always") 
BinomCI(x=sum(GCKD_df1_less60$cardiovasc == "1", na.rm=TRUE), n=sum(!is.na(GCKD_df1_less60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_more60$cardiovasc == "1",  useNA = "always")
BinomCI(x=sum(GCKD_df1_more60$cardiovasc == "1", na.rm=TRUE), n=sum(!is.na(GCKD_df1_more60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)

## Table 4 subset female
var_table_4 <- c("dem_sex", "diabetes", "cardiovasc", "hypertension", "valve", "coronary", "myocard", "bypass", "ptca", "cerebrovasc", 
                 "stroke", "carotic_surg", "carotic_interv", "pavk", "amput", "ygraft", "pavk_surgery", "pta")
GCKD_df1_fem <- GCKD_df1 %>% subset(dem_sex == "Female")
theme_gtsummary_language("en", big.mark = "")
GCKD_df1_fem_table_4a <-
  GCKD_df1_fem %>%
  select(all_of(var_table_4)) %>%
  tbl_summary(
    by = diabetes,
    statistic = list(all_categorical() ~ "{n}"),
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_1 = "Diabetics; n or mean", stat_2 = "Non-diabetics; n or mean") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_fem_table_4b <-
  GCKD_df1_fem %>%
  select(all_of(var_table_4)) %>%
  tbl_summary(
    by = diabetes,
    statistic = list(all_categorical() ~ "{p}"),
    digits = ~ 1,
    missing_text = "(Missing)") %>%
  add_ci(statistic=list(all_categorical() ~ "{conf.low}-{conf.high}", all_continuous() ~ "{conf.low}-{conf.high}"),
         style_fun=list(all_categorical() ~ purrr::partial(style_sigfig, digits=3, scale = 100), 
                        all_continuous() ~ purrr::partial(style_sigfig, digits=3))) %>%
  modify_header(label = "Variable", stat_1 = "Diabetics; %", ci_stat_1 = "Diabetics; 95% CI", stat_2 = "Non-diabetics; %", ci_stat_2 = "Non-diabetics; 95% CI") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_fem_table_4 <-
  tbl_merge(tbls = list(GCKD_df1_fem_table_4a, GCKD_df1_fem_table_4b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_fem_table_4 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_specific_k11k2_table_4_fem.html", 
             path = path_results)
### calculated values in Table 4 subset female
#### k11
GCKD_df1_fem_less60 <- GCKD_df1_fem %>% subset(GCKD_df1_fem$BL_age == "[50, 60[" | GCKD_df1_fem$BL_age == "[40, 50[" | GCKD_df1_fem$BL_age == "[30, 40[" | GCKD_df1_fem$BL_age == "[20, 30[")
GCKD_df1_fem_more60 <- GCKD_df1_fem %>% subset(GCKD_df1_fem$BL_age == "[60, 70[" | GCKD_df1_fem$BL_age == "[70, 80[")
#### k11k2
GCKD_df1_fem_less60 <- GCKD_df1_fem %>% subset(GCKD_df1_fem$BL_age == "[20, 25[" | GCKD_df1_fem$BL_age == "[25, 30[" | GCKD_df1_fem$BL_age == "[30, 35[" | GCKD_df1_fem$BL_age == "[35, 40[" | GCKD_df1_fem$BL_age == "[40, 45[" | GCKD_df1_fem$BL_age == "[45, 50[" | GCKD_df1_fem$BL_age == "[50, 55[" | GCKD_df1_fem$BL_age == "[55, 60[")
GCKD_df1_fem_more60 <- GCKD_df1_fem %>% subset(GCKD_df1_fem$BL_age == "[60, 65[" | GCKD_df1_fem$BL_age == "[65, 70[" | GCKD_df1_fem$BL_age == "[70, 75[" | GCKD_df1_fem$BL_age == "[75, 80[")
#### all
table(GCKD_df1_fem_less60$diabetes, GCKD_df1_fem_less60$cardiovasc == 1,  useNA = "always")  
BinomCI(x=sum(GCKD_df1_fem_less60$diabetes == "1" & GCKD_df1_fem_less60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_fem_less60$diabetes == "1" & !is.na(GCKD_df1_fem_less60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem_less60$diabetes == "2" & GCKD_df1_fem_less60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_fem_less60$diabetes == "2" & !is.na(GCKD_df1_fem_less60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_fem_more60$diabetes, GCKD_df1_fem_more60$cardiovasc == 1,  useNA = "always")  
BinomCI(x=sum(GCKD_df1_fem_more60$diabetes == "1" & GCKD_df1_fem_more60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_fem_more60$diabetes == "1" & !is.na(GCKD_df1_fem_more60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem_more60$diabetes == "2" & GCKD_df1_fem_more60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_fem_more60$diabetes == "2" & !is.na(GCKD_df1_fem_more60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)

## Table 4 subset male
GCKD_df1_male <- GCKD_df1 %>% subset(dem_sex == "Male")
theme_gtsummary_language("en", big.mark = "")
GCKD_df1_male_table_4a <-
  GCKD_df1_male %>%
  select(all_of(var_table_4)) %>%
  tbl_summary(
    by = diabetes,
    statistic = list(all_categorical() ~ "{n}"),
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_1 = "Diabetics; n or mean", stat_2 = "Non-diabetics; n or mean") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_male_table_4b <-
  GCKD_df1_male %>%
  select(all_of(var_table_4)) %>%
  tbl_summary(
    by = diabetes,
    statistic = list(all_categorical() ~ "{p}"),
    digits = ~ 1,
    missing_text = "(Missing)") %>%
  add_ci(statistic=list(all_categorical() ~ "{conf.low}-{conf.high}", all_continuous() ~ "{conf.low}-{conf.high}"),
         style_fun=list(all_categorical() ~ purrr::partial(style_sigfig, digits=3, scale = 100), 
                        all_continuous() ~ purrr::partial(style_sigfig, digits=3))) %>%
  modify_header(label = "Variable", stat_1 = "Diabetics; %", ci_stat_1 = "Diabetics; 95% CI", stat_2 = "Non-diabetics; %", ci_stat_2 = "Non-diabetics; 95% CI") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_male_table_4 <-
  tbl_merge(tbls = list(GCKD_df1_male_table_4a, GCKD_df1_male_table_4b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_male_table_4 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_specific_k11k2_table_4_male.html", 
             path = path_results)
### calculated values in Table 4 subset male
#### k11
GCKD_df1_male_less60 <- GCKD_df1_male %>% subset(GCKD_df1_male$BL_age == "[50, 60[" | GCKD_df1_male$BL_age == "[40, 50[" | GCKD_df1_male$BL_age == "[30, 40[" | GCKD_df1_male$BL_age == "[20, 30[")
GCKD_df1_male_more60 <- GCKD_df1_male %>% subset(GCKD_df1_male$BL_age == "[60, 70[" | GCKD_df1_male$BL_age == "[70, 80[")
#### k11k2
GCKD_df1_male_less60 <- GCKD_df1_male %>% subset(GCKD_df1_male$BL_age == "[20, 25[" | GCKD_df1_male$BL_age == "[25, 30[" | GCKD_df1_male$BL_age == "[30, 35[" | GCKD_df1_male$BL_age == "[35, 40[" | GCKD_df1_male$BL_age == "[40, 45[" | GCKD_df1_male$BL_age == "[45, 50[" | GCKD_df1_male$BL_age == "[50, 55[" | GCKD_df1_male$BL_age == "[55, 60[")
GCKD_df1_male_more60 <- GCKD_df1_male %>% subset(GCKD_df1_male$BL_age == "[60, 65[" | GCKD_df1_male$BL_age == "[65, 70[" | GCKD_df1_male$BL_age == "[70, 75[" | GCKD_df1_male$BL_age == "[75, 80[")
#### all
table(GCKD_df1_male_less60$diabetes, GCKD_df1_male_less60$cardiovasc == 1,  useNA = "always")  
BinomCI(x=sum(GCKD_df1_male_less60$diabetes == "1" & GCKD_df1_male_less60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_male_less60$diabetes == "1" & !is.na(GCKD_df1_male_less60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male_less60$diabetes == "2" & GCKD_df1_male_less60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_male_less60$diabetes == "2" & !is.na(GCKD_df1_male_less60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_male_more60$diabetes, GCKD_df1_male_more60$cardiovasc == 1,  useNA = "always")  
BinomCI(x=sum(GCKD_df1_male_more60$diabetes == "1" & GCKD_df1_male_more60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_male_more60$diabetes == "1" & !is.na(GCKD_df1_male_more60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male_more60$diabetes == "2" & GCKD_df1_male_more60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_male_more60$diabetes == "2" & !is.na(GCKD_df1_male_more60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)

## Suppl. Table 2: GFR categories
### identical to original data

## Suppl. Table 3: Diabetic nephropathy: apart from QI identical to original data
var_table_s3 <- c("biopsy", "DM")
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
  add_ci(statistic=list(all_categorical() ~ "{conf.low}-{conf.high}", all_continuous() ~ "{conf.low}-{conf.high}"),
         style_fun=list(all_categorical() ~ purrr::partial(style_sigfig, digits=3, scale = 100), 
                        all_continuous() ~ purrr::partial(style_sigfig, digits=3))) %>%
  modify_header(label = "Variable", stat_1 = "DM w DN; SD or %", ci_stat_1 = "DM w DN; 95% CI", stat_2 = "DM wo DN; SD or %", ci_stat_2 = "DM wo DN; 95% CI", stat_3 = "No DM; SD or %", ci_stat_3 = "No DM; 95% CI") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_s3 <-
  tbl_merge(tbls = list(GCKD_df1_table_s3a, GCKD_df1_table_s3b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_table_s3 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_specific_k11k2_table_s3.html", 
             path = path_results)
### calculated values in Table S3: apart from bmi groups identical to original
table(GCKD_df1$DM, GCKD_df1$BL_ku_bmi,  useNA = "always")
table(GCKD_df1$DM, GCKD_df1$BL_ku_bmi == "[30, 35[" | GCKD_df1$BL_ku_bmi == "[35, 40[" | GCKD_df1$BL_ku_bmi == "[40, 70[",  useNA = "always")
BinomCI(x=sum(GCKD_df1$DM == "DMwDN" & (GCKD_df1$BL_ku_bmi == "[30, 35[" | GCKD_df1$BL_ku_bmi == "[35, 40[" | GCKD_df1$BL_ku_bmi == "[40, 70["), na.rm=TRUE), n=sum(GCKD_df1$DM == "DMwDN" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM == "DMwDN" & GCKD_df1$BL_ku_bmi == "[25, 30[", na.rm=TRUE), n=sum(GCKD_df1$DM == "DMwDN" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM == "DMwDN" & GCKD_df1$BL_ku_bmi == "[18.5, 25[", na.rm=TRUE), n=sum(GCKD_df1$DM == "DMwDN" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM == "DMwoDN" & (GCKD_df1$BL_ku_bmi == "[30, 35[" | GCKD_df1$BL_ku_bmi == "[35, 40[" | GCKD_df1$BL_ku_bmi == "[40, 70["), na.rm=TRUE), n=sum(GCKD_df1$DM == "DMwoDN" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM == "DMwoDN" & GCKD_df1$BL_ku_bmi == "[25, 30[", na.rm=TRUE), n=sum(GCKD_df1$DM == "DMwoDN" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM == "DMwoDN" & GCKD_df1$BL_ku_bmi == "[18.5, 25[", na.rm=TRUE), n=sum(GCKD_df1$DM == "DMwoDN" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM == "No DM" & (GCKD_df1$BL_ku_bmi == "[30, 35[" | GCKD_df1$BL_ku_bmi == "[35, 40[" | GCKD_df1$BL_ku_bmi == "[40, 70["), na.rm=TRUE), n=sum(GCKD_df1$DM == "No DM" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM == "No DM" & GCKD_df1$BL_ku_bmi == "[25, 30[", na.rm=TRUE), n=sum(GCKD_df1$DM == "No DM" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM == "No DM" & GCKD_df1$BL_ku_bmi == "[18.5, 25[", na.rm=TRUE), n=sum(GCKD_df1$DM == "No DM" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
### illustration of parameters with change in datatype: age, bmi, weight
GCKD_df1_dmwdn <- GCKD_df1 %>% subset(DM == "DMwDN")
GCKD_df1_QI_cat_dmwdn <- select(GCKD_df1_dmwdn, BL_age, BL_ku_bmi, BL_ku_weight)
#### k11
GCKD_df1_QI_cat_dmwdn$BL_ku_weight <- factor(GCKD_df1_QI_cat_dmwdn$BL_ku_weight, levels = c("[40, 80[", "[80, 120[", "[120, 160["))
#### k11k2
GCKD_df1_QI_cat_dmwdn$BL_ku_weight <- factor(GCKD_df1_QI_cat_dmwdn$BL_ku_weight, levels = c("[40, 50[", "[50, 60[", "[60, 70[", "[70, 80[", "[80, 90[", "[90, 100[", "[100, 110[", "[110, 120[", "[120, 130[", "[130, 140[", "[140, 150[", "[150, 160[", "[160, 170["))
#### all
for(i in 1:ncol((GCKD_df1_QI_cat_dmwdn))) {
  x = colnames(GCKD_df1_QI_cat_dmwdn)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_QI_cat_dmwdn, !is.na(GCKD_df1_QI_cat_dmwdn[i])), colour="white", fill="azure4")
  print(Fig)
}
for(i in 1:ncol((GCKD_df1_QI_cat_dmwdn))) {
  x = colnames(GCKD_df1_QI_cat_dmwdn)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_QI_cat_dmwdn, !is.na(GCKD_df1_QI_cat_dmwdn[i])), colour="white", fill="azure4") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  print(Fig)
}
GCKD_df1_dmwodn <- GCKD_df1 %>% subset(DM == "DMwoDN")
GCKD_df1_QI_cat_dmwodn <- select(GCKD_df1_dmwodn, BL_age, BL_ku_bmi, BL_ku_weight)
#### k11
GCKD_df1_QI_cat_dmwodn$BL_ku_weight <- factor(GCKD_df1_QI_cat_dmwodn$BL_ku_weight, levels = c("[40, 80[", "[80, 120[", "[120, 160["))
#### k11k2
GCKD_df1_QI_cat_dmwodn$BL_ku_weight <- factor(GCKD_df1_QI_cat_dmwodn$BL_ku_weight, levels = c("[40, 50[", "[50, 60[", "[60, 70[", "[70, 80[", "[80, 90[", "[90, 100[", "[100, 110[", "[110, 120[", "[120, 130[", "[130, 140[", "[140, 150[", "[150, 160[", "[160, 170["))
#### all
for(i in 1:ncol((GCKD_df1_QI_cat_dmwodn))) {
  x = colnames(GCKD_df1_QI_cat_dmwodn)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_QI_cat_dmwodn, !is.na(GCKD_df1_QI_cat_dmwodn[i])), colour="white", fill="azure4")
  print(Fig)
}
for(i in 1:ncol((GCKD_df1_QI_cat_dmwodn))) {
  x = colnames(GCKD_df1_QI_cat_dmwodn)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_QI_cat_dmwodn, !is.na(GCKD_df1_QI_cat_dmwodn[i])), colour="white", fill="azure4") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  print(Fig)
}

GCKD_df1_nodm <- GCKD_df1 %>% subset(DM == "No DM")
GCKD_df1_QI_cat_nodm <- select(GCKD_df1_nodm, BL_age, BL_ku_bmi, BL_ku_weight)
#### k11
GCKD_df1_QI_cat_nodm$BL_ku_weight <- factor(GCKD_df1_QI_cat_nodm$BL_ku_weight, levels = c("[40, 80[", "[80, 120[", "[120, 160["))
#### k11k2
GCKD_df1_QI_cat_nodm$BL_ku_weight <- factor(GCKD_df1_QI_cat_nodm$BL_ku_weight, levels = c("[40, 50[", "[50, 60[", "[60, 70[", "[70, 80[", "[80, 90[", "[90, 100[", "[100, 110[", "[110, 120[", "[120, 130[", "[130, 140[", "[140, 150[", "[150, 160[", "[160, 170["))
#### all
for(i in 1:ncol((GCKD_df1_QI_cat_nodm))) {
  x = colnames(GCKD_df1_QI_cat_nodm)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_QI_cat_nodm, !is.na(GCKD_df1_QI_cat_nodm[i])), colour="white", fill="azure4")
  print(Fig)
}
for(i in 1:ncol((GCKD_df1_QI_cat_nodm))) {
  x = colnames(GCKD_df1_QI_cat_nodm)[i]
  Fig <- ggplot() + 
    geom_bar(aes_string(x), data = subset(GCKD_df1_QI_cat_nodm, !is.na(GCKD_df1_QI_cat_nodm[i])), colour="white", fill="azure4") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  print(Fig)
}

### calculation of height
GCKD_df1_height_dmwdn <- GCKD_df1_height %>% subset(DM == "DMwDN")
GCKD_df1_height_dmwdn$ID <- seq.int(nrow(GCKD_df1_height_dmwdn))
GCKD_df1_height_cm_dmwdn_low <- GCKD_df1_height_dmwdn[,c("height_cm_low_calc", "ID", "height_cm_group")]
GCKD_df1_height_cm_dmwdn_low$bound = "low"
names(GCKD_df1_height_cm_dmwdn_low)[names(GCKD_df1_height_cm_dmwdn_low) == "height_cm_low_calc"] <- "height_cm"
GCKD_df1_height_cm_dmwdn_up <- GCKD_df1_height_dmwdn[,c("height_cm_up_calc", "ID", "height_cm_group")]
GCKD_df1_height_cm_dmwdn_up$bound = "up"
names(GCKD_df1_height_cm_dmwdn_up)[names(GCKD_df1_height_cm_dmwdn_up) == "height_cm_up_calc"] <- "height_cm"
GCKD_df1_height_cm_dmwdn <- rbind(GCKD_df1_height_cm_dmwdn_low, GCKD_df1_height_cm_dmwdn_up)
ggplot(data=GCKD_df1_height_cm_dmwdn, aes(x = height_cm, y = height_cm_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3)
ggplot(data=GCKD_df1_height_cm_dmwdn, aes(x = height_cm, y = height_cm_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
GCKD_df1_height_dmwodn <- GCKD_df1_height %>% subset(DM == "DMwoDN")
GCKD_df1_height_dmwodn$ID <- seq.int(nrow(GCKD_df1_height_dmwodn))
GCKD_df1_height_cm_dmwodn_low <- GCKD_df1_height_dmwodn[,c("height_cm_low_calc", "ID", "height_cm_group")]
GCKD_df1_height_cm_dmwodn_low$bound = "low"
names(GCKD_df1_height_cm_dmwodn_low)[names(GCKD_df1_height_cm_dmwodn_low) == "height_cm_low_calc"] <- "height_cm"
GCKD_df1_height_cm_dmwodn_up <- GCKD_df1_height_dmwodn[,c("height_cm_up_calc", "ID", "height_cm_group")]
GCKD_df1_height_cm_dmwodn_up$bound = "up"
names(GCKD_df1_height_cm_dmwodn_up)[names(GCKD_df1_height_cm_dmwodn_up) == "height_cm_up_calc"] <- "height_cm"
GCKD_df1_height_cm_dmwodn <- rbind(GCKD_df1_height_cm_dmwodn_low, GCKD_df1_height_cm_dmwodn_up)
ggplot(data=GCKD_df1_height_cm_dmwodn, aes(x = height_cm, y = height_cm_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3)
ggplot(data=GCKD_df1_height_cm_dmwodn, aes(x = height_cm, y = height_cm_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
GCKD_df1_height_nodm <- GCKD_df1_height %>% subset(DM == "No DM")
GCKD_df1_height_nodm$ID <- seq.int(nrow(GCKD_df1_height_nodm))
GCKD_df1_height_cm_nodm_low <- GCKD_df1_height_nodm[,c("height_cm_low_calc", "ID", "height_cm_group")]
GCKD_df1_height_cm_nodm_low$bound = "low"
names(GCKD_df1_height_cm_nodm_low)[names(GCKD_df1_height_cm_nodm_low) == "height_cm_low_calc"] <- "height_cm"
GCKD_df1_height_cm_nodm_up <- GCKD_df1_height_nodm[,c("height_cm_up_calc", "ID", "height_cm_group")]
GCKD_df1_height_cm_nodm_up$bound = "up"
names(GCKD_df1_height_cm_nodm_up)[names(GCKD_df1_height_cm_nodm_up) == "height_cm_up_calc"] <- "height_cm"
GCKD_df1_height_cm_nodm <- rbind(GCKD_df1_height_cm_nodm_low, GCKD_df1_height_cm_nodm_up)
ggplot(data=GCKD_df1_height_cm_nodm, aes(x = height_cm, y = height_cm_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3)
ggplot(data=GCKD_df1_height_cm_nodm, aes(x = height_cm, y = height_cm_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

## Suppl. Figure 1: Age distribution stratified
GCKD_df1_FigS1 <- GCKD_df1
#### k11
GCKD_df1_FigS1 <- GCKD_df1_FigS1 %>% mutate(
  BL_age_cat = ifelse(GCKD_df1_FigS1$BL_age == "[10, 20[", 1, ifelse(GCKD_df1_FigS1$BL_age == "[20, 30[", 2, ifelse(
    GCKD_df1_FigS1$BL_age == "[30, 40[", 3, ifelse(GCKD_df1_FigS1$BL_age == "[40, 50[", 4, ifelse(
      GCKD_df1_FigS1$BL_age == "[50, 60[", 5, ifelse(GCKD_df1_FigS1$BL_age == "[60, 70[", 6, 7)))))))
#### k11k2
GCKD_df1_FigS1 <- GCKD_df1_FigS1 %>% mutate(
  BL_age_cat = ifelse(GCKD_df1_FigS1$BL_age == "[15, 20[", 1, ifelse(GCKD_df1_FigS1$BL_age == "[20, 25[" | GCKD_df1_FigS1$BL_age == "[25, 30[", 2, ifelse(
    GCKD_df1_FigS1$BL_age == "[30, 35[" | GCKD_df1_FigS1$BL_age == "[35, 40[", 3, ifelse(GCKD_df1_FigS1$BL_age == "[40, 45[" | GCKD_df1_FigS1$BL_age == "[45, 50[", 4, ifelse(
      GCKD_df1_FigS1$BL_age == "[50, 55[" | GCKD_df1_FigS1$BL_age == "[55, 60[", 5, ifelse(GCKD_df1_FigS1$BL_age == "[60, 65[" | GCKD_df1_FigS1$BL_age == "[65, 70[", 6, 7)))))))
#### all
GCKD_df1_FigS1 %>%
  filter(dem_sex == "Male") %>%
  ggplot(aes(x=as.factor(BL_age_cat))) +
  geom_bar(color="white", fill="azure4", width=1.0)  +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) +
  scale_x_discrete(limits = as.character(1:7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)
GCKD_df1_FigS1 %>%
  filter(dem_sex == "Female") %>%
  ggplot(aes(x=as.factor(BL_age_cat))) +
  geom_bar(color="white", fill="azure4", alpha = 0.5, width=1.0) +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
  scale_x_discrete(limits = as.character(1:7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)
GCKD_df1_FigS1 %>%
  filter(diabetes == 1) %>%
  ggplot(aes(x=as.factor(BL_age_cat))) +
  geom_bar(color="white", fill="azure4", width=1.0)  +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
  scale_x_discrete(limits = as.character(1:7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)
GCKD_df1_FigS1 %>%
  filter(diabetes == 2) %>%
  ggplot(aes(x=as.factor(BL_age_cat))) +
  geom_bar(color="white", fill="azure4", alpha = 0.5, width=1.0) +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
  scale_x_discrete(limits = as.character(1:7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)
