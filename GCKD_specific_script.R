# Packages 
pacman::p_load(tidyr, stringr, dplyr, openxlsx, naniar, emmeans, multcomp, 
               plyr, finalfit, ggplot2, tibble, lmtest, sandwich,
               tidyverse, tidyselect, summarytools, scales, gridExtra, 
               lubridate, eeptools, gtsummary, flextable, boot, mosaic, patchwork, rms, coxed, DescTools, PropCIs)

# Dataset
path_results = "C:/Users/User/OneDrive/Documents/PRIVAT/Charite/Forschung/Projekt Computerbasierte Anonymisierung/Titzeetal/Ergebnisse"
path_data = "D:/BackUp GCKD"
path_tbl = "C:/Users/User/OneDrive/Documents/PRIVAT/Charite/Forschung/Projekt Computerbasierte Anonymisierung/Titzeetal/Ergebnisse/Tbl_perc_CI"
setwd(path_data)
#GCKD_df1 <-  as_tibble(read.csv("GCKD_specific_k11.csv", sep = ";"))
#GCKD_df1 <- as_tibble(read.csv("GCKD_specific_k11k2.csv", sep = ";"))
GCKD_df1 <- as_tibble(read.csv("GCKD_usecase_strictaverage_11.csv", sep = ";"))
dim(GCKD_df1) ## ku_height still included 

# Data Cleansing
## Dropping height
GCKD_df1 <- subset(GCKD_df1, select = -c(BL_ku_height_cm))
GCKD_df1 <- subset(GCKD_df1, select = -c(BL_ku_weight))
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
GCKD_df1$aa_stroke <- mapvalues(GCKD_df1$aa_stroke, from = c("1", "2"), to = c("aa_stroke_yes", "aa_stroke_no"))
GCKD_df1$aa_myocard <- mapvalues(GCKD_df1$aa_myocard, from = c("1", "2"), to = c("aa_myocard_yes", "aa_myocard_no"))
GCKD_df1$aa_hypertens <- mapvalues(GCKD_df1$aa_hypertens, from = c("1", "2"), to = c("aa_hypertens_yes", "aa_hypertens_no"))
GCKD_df1$aa_diabetes <- mapvalues(GCKD_df1$aa_diabetes, from = c("1", "2"), to = c("aa_diabetes_yes", "aa_diabetes_no"))
GCKD_df1$aa_renal <- mapvalues(GCKD_df1$aa_renal, from = c("1", "2"), to = c("aa_renal_yes", "aa_renal_no"))
GCKD_df1$aa_renal_stones <- mapvalues(GCKD_df1$aa_renal_stones, from = c("1", "2"), to = c("aa_renal_stones_yes", "aa_renal_stones_no"))
GCKD_df1$aa_dialyse <- mapvalues(GCKD_df1$aa_dialyse, from = c("1", "2"), to = c("aa_dialyse_yes", "aa_dialyse_no"))
GCKD_df1$aa_ntx <- mapvalues(GCKD_df1$aa_ntx, from = c("1", "2"), to = c("aa_ntx_yes", "aa_ntx_no"))
GCKD_df1$smoking <- mapvalues(GCKD_df1$smoking, from = c("1", "2", "3"), to = c("smoking_never", "smoking_former", "smoking_current"))
GCKD_df1$hospital <- mapvalues(GCKD_df1$hospital, from = c("1", "2"), to = c("hospital_yes", "hospital_no"))
GCKD_df1$BL_med_raas_ace <- mapvalues(GCKD_df1$BL_med_raas_ace, from = c("1", "2"), to = c("BL_med_raas_ace_yes", "BL_med_raas_ace_no"))
GCKD_df1$BL_med_raas_at1 <- mapvalues(GCKD_df1$BL_med_raas_at1, from = c("1", "2"), to = c("BL_med_raas_at1_yes", "BL_med_raas_at1_no"))
GCKD_df1$BL_med_raas_double <- mapvalues(GCKD_df1$BL_med_raas_double, from = c("1", "2"), to = c("BL_med_raas_double_yes", "BL_med_raas_double_no"))
GCKD_df1$BL_med_raas_single <- mapvalues(GCKD_df1$BL_med_raas_single, from = c("1", "2"), to = c("BL_med_raas_single_yes", "BL_med_raas_single_no"))
GCKD_df1$BL_med_diuretic <- mapvalues(GCKD_df1$BL_med_diuretic, from = c("1", "2"), to = c("BL_med_diuretic_yes", "BL_med_diuretic_no"))
GCKD_df1$BL_med_diuretic_thiazid <- mapvalues(GCKD_df1$BL_med_diuretic_thiazid, from = c("1", "2"), to = c("BL_med_diuretic_thiazid_yes", "BL_med_diuretic_thiazid_no"))
GCKD_df1$BL_med_diuretic_aldost <- mapvalues(GCKD_df1$BL_med_diuretic_aldost, from = c("1", "2"), to = c("BL_med_diuretic_aldost_yes", "BL_med_diuretic_aldost_no"))
GCKD_df1$BL_med_diuretic_loop <- mapvalues(GCKD_df1$BL_med_diuretic_loop, from = c("1", "2"), to = c("BL_med_diuretic_loop_yes", "BL_med_diuretic_loop_no"))
GCKD_df1$BL_med_caanta <- mapvalues(GCKD_df1$BL_med_caanta, from = c("1", "2"), to = c("BL_med_caanta_yes", "BL_med_caanta_no"))
GCKD_df1$BL_med_bblocker <- mapvalues(GCKD_df1$BL_med_bblocker, from = c("1", "2"), to = c("BL_med_bblocker_yes", "BL_med_bblocker_no"))
GCKD_df1$biopsy <- mapvalues(GCKD_df1$biopsy, from = c("1", "2"), to = c("biopsy_yes", "biopsy_no"))
GCKD_df1$diabetes <- mapvalues(GCKD_df1$diabetes, from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
GCKD_df1$incl_egfr <- mapvalues(GCKD_df1$incl_egfr, from = c("1", "2"), to = c("egfr", "prot"))
GCKD_df1$cardiovasc <- mapvalues(GCKD_df1$cardiovasc, from = c("1", "2"), to = c("cardiovasc_yes", "cardiovasc_no"))
GCKD_df1$hypertension <- mapvalues(GCKD_df1$hypertension, from = c("1", "2"), to = c("hypertension_yes", "hypertension_no"))
GCKD_df1$valve <- mapvalues(GCKD_df1$valve, from = c("1", "2"), to = c("valve_yes", "valve_no"))
GCKD_df1$coronary <- mapvalues(GCKD_df1$coronary, from = c("1", "2"), to = c("coronary_yes", "coronary_no"))
GCKD_df1$myocard <- mapvalues(GCKD_df1$myocard, from = c("1", "2"), to = c("myocard_yes", "myocard_no"))
GCKD_df1$bypass <- mapvalues(GCKD_df1$bypass, from = c("1", "2"), to = c("bypass_yes", "bypass_no"))
GCKD_df1$ptca <- mapvalues(GCKD_df1$ptca, from = c("1", "2"), to = c("ptca_yes", "ptca_no"))
GCKD_df1$cerebrovasc <- mapvalues(GCKD_df1$cerebrovasc, from = c("1", "2"), to = c("cerebrovasc_yes", "cerebrovasc_no"))
GCKD_df1$stroke <- mapvalues(GCKD_df1$stroke, from = c("1", "2"), to = c("stroke_yes", "stroke_no"))
GCKD_df1$carotic_surg <- mapvalues(GCKD_df1$carotic_surg, from = c("1", "2"), to = c("carotic_surg_yes", "carotic_surg_no"))
GCKD_df1$carotic_interv <- mapvalues(GCKD_df1$carotic_interv, from = c("1", "2"), to = c("carotic_interv_yes", "carotic_interv_no"))
GCKD_df1$pavk <- mapvalues(GCKD_df1$pavk, from = c("1", "2"), to = c("pavk_yes", "pavk_no"))
GCKD_df1$amput <- mapvalues(GCKD_df1$amput, from = c("1", "2"), to = c("amput_yes", "amput_no"))
GCKD_df1$ygraft <- mapvalues(GCKD_df1$ygraft, from = c("1", "2"), to = c("ygraft_yes", "ygraft_no"))
GCKD_df1$pavk_surgery <- mapvalues(GCKD_df1$pavk_surgery, from = c("1", "2"), to = c("pavk_surgery_yes", "pavk_surgery_no"))
GCKD_df1$pta <- mapvalues(GCKD_df1$pta, from = c("1", "2"), to = c("pta_yes", "pta_no"))
GCKD_df1$education <- mapvalues(GCKD_df1$education, from = c("1", "2", "3", "4"), to = c("edu_low", "edu_medium", "edu_high", "edu_uk"))
GCKD_df1$education <- factor(GCKD_df1$education, c("edu_high", "edu_medium", "edu_low", "edu_uk"))
col_num <- c("BL_ku_sys", "BL_ku_dia", "BL_ku_map", "BL_ku_ruhepuls", "BL_creavalue", "BL_cysvalue", 
             "BL_gfr_mdrd", "BL_gfr_ckdepi", "BL_gfr_ckdepi_cys", "BL_gfr_ckdepi_creacys", "BL_uacr")
GCKD_df1 <- GCKD_df1 %>% mutate(across(all_of(col_num), as.numeric))
GCKD_df1 <- GCKD_df1 %>% mutate(RR130 = ifelse(GCKD_df1$BL_ku_sys < 130 & GCKD_df1$BL_ku_dia < 80, "RR130_yes", "RR130_no"))
GCKD_df1 <- GCKD_df1 %>% mutate(RR140 = ifelse(GCKD_df1$BL_ku_sys < 140 & GCKD_df1$BL_ku_dia < 90, "RR140_yes", "RR140_no"))
GCKD_df1 <- GCKD_df1 %>% mutate(gfr_mdrd_cat = ifelse(GCKD_df1$BL_gfr_mdrd >= 60, "GFR60", ifelse(GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60, "GFR4560", 
                                                                                                  ifelse(GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45, "GFR3045", 
                                                                                                         ifelse(GCKD_df1$BL_gfr_mdrd < 30, "GFR30", "GFR_uk")))))
GCKD_df1$gfr_mdrd_cat <- factor(GCKD_df1$gfr_mdrd_cat, c("GFR60", "GFR4560", "GFR3045", "GFR30"))
GCKD_df1 <- GCKD_df1 %>% mutate(uacr_cat = ifelse(GCKD_df1$BL_uacr < 30, "UACR30", ifelse(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300, "UACR30300", 
                                                                                          ifelse(GCKD_df1$BL_uacr > 300, "UACR300", "UACR_uk"))))
GCKD_df1$uacr_cat <- factor(GCKD_df1$uacr_cat, c("UACR30", "UACR30300", "UACR300"))
GCKD_df1 <- GCKD_df1 %>% mutate(DM = ifelse(GCKD_df1$diabetes == "diabetes_no", "No DM", ifelse(GCKD_df1$ckd_lead == "ckd_diab", "DMwDN", "DMwoDN")))




# Table 1 including 95% CI for distribution or mean
## column perc in for numerical data is SD
## apart from dem_sex & biopsy everything identical
var_table_1_red <- c("dem_sex", "biopsy")
theme_gtsummary_language("en", big.mark = "")
GCKD_df1_table_1a <-
  GCKD_df1 %>%
  select(all_of(var_table_1_red)) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n}"),
    digits = all_continuous() ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_0 = "n_mean_total") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_1b <-
  GCKD_df1 %>%
  select(all_of(var_table_1_red)) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{p}"),
    digits = ~ 1,
    missing_text = "(Missing)") %>%
  add_ci(statistic=list(all_categorical() ~ "{conf.low}-{conf.high}", all_continuous() ~ "{conf.low}-{conf.high}"),
         style_fun=list(all_categorical() ~ purrr::partial(style_sigfig, digits=3, scale = 100), 
                        all_continuous() ~ purrr::partial(style_sigfig, digits=3))) %>%
  modify_header(label = "Variable", stat_0 = "perc_total", ci_stat_0 = "CI_total") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_1 <-
  tbl_merge(tbls = list(GCKD_df1_table_1a, GCKD_df1_table_1b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_table_1 %>%
  as_gt() %>%
  gt::gtsave(filename = "tbl1_total_strictaverage_usecase_11_4QI.html", 
             path = path_results)
### calculated values in Table 1: identical to original
GCKD_df1_table_1 <- as_tibble(GCKD_df1_table_1)
setwd(path_tbl)
write.xlsx(GCKD_df1_table_1, "tbl1_total_strictaverage_usecase_11_4QI.xlsx")

## Table 1 subset female
### apart from UACR MedianCI everything included
var_table_1 <- c("dem_sex", "diabetes", "aa_stroke", "aa_myocard", "aa_hypertens", "aa_diabetes", "aa_renal", 
                 "aa_renal_stones", "aa_dialyse", "aa_ntx", "smoking", "hospital", "BL_ku_ruhepuls", "BL_ku_sys", "BL_ku_dia", 
                 "BL_ku_map", "RR130", "RR140", "BL_creavalue", "BL_cysvalue",
                 "BL_gfr_mdrd", "gfr_mdrd_cat", "BL_uacr", "uacr_cat", "BL_med_raas_ace", "BL_med_raas_at1", "BL_med_raas_double", 
                 "BL_med_raas_single", "BL_med_diuretic", "BL_med_diuretic_thiazid", "BL_med_diuretic_aldost", 
                 "BL_med_diuretic_loop", "BL_med_caanta", "BL_med_bblocker", "biopsy")
GCKD_df1_fem <- GCKD_df1 %>% subset(dem_sex == "Female")
theme_gtsummary_language("en", big.mark = "")
GCKD_df1_fem_table_1a <-
  GCKD_df1_fem %>%
  select(all_of(var_table_1)) %>%
  tbl_summary(
    by = diabetes,
    statistic = list(all_continuous() ~ "{mean}",
                     BL_uacr ~ "{median}", 
                     all_categorical() ~ "{n}"),
    digits = all_continuous() ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_1 = "n_mean_female_d", stat_2 = "n_mean_female_nd") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_fem_table_1b <-
  GCKD_df1_fem %>%
  select(all_of(var_table_1)) %>%
  tbl_summary(
    by = diabetes,
    statistic = list(all_continuous() ~ "{sd}",
                     BL_uacr ~ "{p25}-{p75}",
                     all_categorical() ~ "{p}"),
    digits = ~ 1,
    missing_text = "(Missing)") %>%
  add_ci(statistic=list(all_categorical() ~ "{conf.low}-{conf.high}", all_continuous() ~ "{conf.low}-{conf.high}"),
         style_fun=list(all_categorical() ~ purrr::partial(style_sigfig, digits=3, scale = 100), 
                        all_continuous() ~ purrr::partial(style_sigfig, digits=3))) %>%
  modify_header(label = "Variable", stat_1 = "perc_female_d", ci_stat_1 = "CI_female_d", stat_2 = "perc_female_nd", ci_stat_2 = "CI_female_nd") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_fem_table_1 <-
  tbl_merge(tbls = list(GCKD_df1_fem_table_1a, GCKD_df1_fem_table_1b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_fem_table_1 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_kanonymity_usecase_11_table_1_fem.html", 
             path = path_results)
## UACR median CI calculation
GCKD_df1_fem_d <- GCKD_df1_fem %>% subset(diabetes == "diabetes_yes")
GCKD_df1_fem_nd <- GCKD_df1_fem %>% subset(diabetes == "diabetes_no")
UACR_MedCI_fem_d <- MedianCI(GCKD_df1_fem_d$BL_uacr, na.rm=TRUE)
UACR_CI_low_fem_d <- round(UACR_MedCI_fem_d[2], 1)
UACR_CI_high_fem_d <-round(UACR_MedCI_fem_d[3], 1)
UACR_CI_fem_d <- paste(UACR_CI_low_fem_d, UACR_CI_high_fem_d, sep="-")
UACR_MedCI_fem_nd <- MedianCI(GCKD_df1_fem_nd$BL_uacr, na.rm=TRUE)
UACR_CI_low_fem_nd <- round(UACR_MedCI_fem_nd[2], 1)
UACR_CI_high_fem_nd <-round(UACR_MedCI_fem_nd[3], 1)
UACR_CI_fem_nd <- paste(UACR_CI_low_fem_nd, UACR_CI_high_fem_nd, sep="-")
### clean and combine data
GCKD_df1_fem_table_1 <- as_tibble(GCKD_df1_fem_table_1)
GCKD_df1_fem_table_1$CI_female_d[GCKD_df1_fem_table_1$Variable == "BL_uacr"] <- UACR_CI_fem_d
GCKD_df1_fem_table_1$CI_female_nd[GCKD_df1_fem_table_1$Variable == "BL_uacr"] <- UACR_CI_fem_nd
setwd(path_tbl)
write.xlsx(GCKD_df1_fem_table_1, "tbl1_female_strictaverage_usecase_11.xlsx")

### Table 1 subset male
GCKD_df1_male <- GCKD_df1 %>% subset(dem_sex == "Male")
theme_gtsummary_language("en", big.mark = "")
GCKD_df1_male_table_1a <-
  GCKD_df1_male %>%
  select(all_of(var_table_1)) %>%
  tbl_summary(
    by = diabetes,    
    statistic = list(all_continuous() ~ "{mean}",
                     BL_uacr ~ "{median}",
                     all_categorical() ~ "{n}"),
    digits = all_continuous() ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_1 = "n_mean_male_d", stat_2 = "n_mean_male_nd") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_male_table_1b <-
  GCKD_df1_male %>%
  select(all_of(var_table_1)) %>%
  tbl_summary(
    by = diabetes,
    statistic = list(all_continuous() ~ "{sd}",
                     BL_uacr ~ "{p25}-{p75}",
                     all_categorical() ~ "{p}"),
    digits = ~ 1,
    missing_text = "(Missing)") %>%
  add_ci(statistic=list(all_categorical() ~ "{conf.low}-{conf.high}", all_continuous() ~ "{conf.low}-{conf.high}"),
         style_fun=list(all_categorical() ~ purrr::partial(style_sigfig, digits=3, scale = 100), 
                        all_continuous() ~ purrr::partial(style_sigfig, digits=3))) %>%
  modify_header(label = "Variable", stat_1 = "perc_male_d", ci_stat_1 = "CI_male_d", stat_2 = "perc_male_nd", ci_stat_2 = "CI_male_nd") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_male_table_1 <-
  tbl_merge(tbls = list(GCKD_df1_male_table_1a, GCKD_df1_male_table_1b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_male_table_1 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_kanonymity_usecase_11_table_1_male.html", 
             path = path_results)
## UACR median CI calculation
GCKD_df1_male_d <- GCKD_df1_male %>% subset(diabetes == "diabetes_yes")
GCKD_df1_male_nd <- GCKD_df1_male %>% subset(diabetes == "diabetes_no")
UACR_MedCI_male_d <- MedianCI(GCKD_df1_male_d$BL_uacr, na.rm=TRUE)
UACR_CI_low_male_d <- round(UACR_MedCI_male_d[2], 1)
UACR_CI_high_male_d <-round(UACR_MedCI_male_d[3], 1)
UACR_CI_male_d <- paste(UACR_CI_low_male_d, UACR_CI_high_male_d, sep="-")
UACR_MedCI_male_nd <- MedianCI(GCKD_df1_male_nd$BL_uacr, na.rm=TRUE)
UACR_CI_low_male_nd <- round(UACR_MedCI_male_nd[2], 1)
UACR_CI_high_male_nd <-round(UACR_MedCI_male_nd[3], 1)
UACR_CI_male_nd <- paste(UACR_CI_low_male_nd, UACR_CI_high_male_nd, sep="-")
### clean and combine data
GCKD_df1_male_table_1 <- as_tibble(GCKD_df1_male_table_1)
GCKD_df1_male_table_1$CI_male_d[GCKD_df1_male_table_1$Variable == "BL_uacr"] <- UACR_CI_male_d
GCKD_df1_male_table_1$CI_male_nd[GCKD_df1_male_table_1$Variable == "BL_uacr"] <- UACR_CI_male_nd
setwd(path_tbl)
write.xlsx(GCKD_df1_fem_table_1, "tbl1_male_strictaverage_usecase_11.xlsx")

## Table 1 merge subsets
tbl1_female_male_anonym <- full_join(x = GCKD_df1_male_table_1, y = GCKD_df1_fem_table_1, by = "Variable")
tbl1_total_female_male_anonym <- full_join(x = tbl1_female_male_anonym, y = GCKD_df1_table_1, by = "Variable")
tbl1_anonym <- subset(tbl1_total_female_male_anonym, !(endsWith(tbl1_total_female_male_anonym$Variable, "_no")) & 
                        tbl1_total_female_male_anonym$Variable != "(Missing)" &
                        tbl1_total_female_male_anonym$Variable != "Female" &
                        (!is.na(tbl1_total_female_male_anonym$n_mean_female_d) | !is.na(tbl1_total_female_male_anonym$n_mean_male_d)))
setwd(path_tbl)
write.xlsx(tbl1_anonym, "tbl1_strictaverage_usecase_11.xlsx")

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

### subset female illustration of parameters with change in datatype: age, height, weight
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

### subset male illustration of parameters with change in datatype: age, height, weight
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

## Table 2: Characteristics grouped by inclusion criteria: apart from QI identical to original 
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
  modify_header(label = "Variable", stat_1 = "n_mean_egfr", stat_2 = "n_mean_prot") %>%
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
  modify_header(label = "Variable", stat_1 = "perc_egfr", ci_stat_1 = "CI_egfr", stat_2 = "perc_prot", ci_stat_2 = "CI_prot") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_2 <-
  tbl_merge(tbls = list(GCKD_df1_table_2a, GCKD_df1_table_2b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_table_2 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_strictaverage_k11k2_table_2.html", 
             path = path_results)

## Cardiovasc - age calculations
#### k11
GCKD_df1_less60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[50, 60[" | GCKD_df1$BL_age == "[40, 50[" | GCKD_df1$BL_age == "[30, 40[" | GCKD_df1$BL_age == "[20, 30[")
GCKD_df1_more60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[60, 70[" | GCKD_df1$BL_age == "[70, 80[")
#### k11k2
GCKD_df1_less60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[20, 25[" | GCKD_df1$BL_age == "[25, 30[" | GCKD_df1$BL_age == "[30, 35[" | GCKD_df1$BL_age == "[35, 40[" | GCKD_df1$BL_age == "[40, 45[" | GCKD_df1$BL_age == "[45, 50[" | GCKD_df1$BL_age == "[50, 55[" | GCKD_df1$BL_age == "[55, 60[")
GCKD_df1_more60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[60, 65[" | GCKD_df1$BL_age == "[65, 70[" | GCKD_df1$BL_age == "[70, 75[" | GCKD_df1$BL_age == "[75, 80[")
#### all
cardiovasc_less60_yes_egfr <- sum(!is.na(GCKD_df1_less60$cardiovasc) & GCKD_df1_less60$cardiovasc == "cardiovasc_yes" & GCKD_df1_less60$incl_egfr == "egfr")
cardiovasc_less60_yes_egfr_BinomCI <- BinomCI(x=sum(GCKD_df1_less60$incl_egfr == "egfr" & GCKD_df1_less60$cardiovasc == "cardiovasc_yes", na.rm=TRUE), 
                                              n=sum(GCKD_df1_less60$incl_egfr == "egfr" &!is.na(GCKD_df1_less60$cardiovasc)),method="wilson") %>% '*'(100) %>% round(1)
cardiovasc_less60_yes_perc_egfr <-round(cardiovasc_less60_yes_egfr_BinomCI[1], 1)
cardiovasc_less60_yes_CI_low_egfr <- round(cardiovasc_less60_yes_egfr_BinomCI[2], 1)
cardiovasc_less60_yes_CI_high_egfr <-round(cardiovasc_less60_yes_egfr_BinomCI[3], 1)
cardiovasc_less60_yes_CI_egfr <- paste(cardiovasc_less60_yes_CI_low_egfr, cardiovasc_less60_yes_CI_high_egfr, sep="-")
cardiovasc_less60_yes_prot <- sum(!is.na(GCKD_df1_less60$cardiovasc) & GCKD_df1_less60$cardiovasc == "cardiovasc_yes" & GCKD_df1_less60$incl_egfr == "prot")
cardiovasc_less60_yes_prot_BinomCI <- BinomCI(x=sum(GCKD_df1_less60$incl_egfr == "prot" & GCKD_df1_less60$cardiovasc == "cardiovasc_yes", na.rm=TRUE), 
                                              n=sum(GCKD_df1_less60$incl_egfr == "prot" & !is.na(GCKD_df1_less60$cardiovasc)),method="wilson") %>% '*'(100) %>% round(1)
cardiovasc_less60_yes_perc_prot <-round(cardiovasc_less60_yes_prot_BinomCI[1], 1)
cardiovasc_less60_yes_CI_low_prot <- round(cardiovasc_less60_yes_prot_BinomCI[2], 1)
cardiovasc_less60_yes_CI_high_prot <-round(cardiovasc_less60_yes_prot_BinomCI[3], 1)
cardiovasc_less60_yes_CI_prot <- paste(cardiovasc_less60_yes_CI_low_prot, cardiovasc_less60_yes_CI_high_prot, sep="-")
cardiovasc_more60_yes_egfr <- sum(!is.na(GCKD_df1_more60$cardiovasc) & GCKD_df1_more60$cardiovasc == "cardiovasc_yes" & GCKD_df1_more60$incl_egfr == "egfr")
cardiovasc_more60_yes_egfr_BinomCI <- BinomCI(x=sum(GCKD_df1_more60$incl_egfr == "egfr" & GCKD_df1_more60$cardiovasc == "cardiovasc_yes", na.rm=TRUE), 
                                              n=sum(GCKD_df1_more60$incl_egfr == "egfr" &!is.na(GCKD_df1_more60$cardiovasc)),method="wilson") %>% '*'(100) %>% round(1)
cardiovasc_more60_yes_perc_egfr <-round(cardiovasc_more60_yes_egfr_BinomCI[1], 1)
cardiovasc_more60_yes_CI_low_egfr <- round(cardiovasc_more60_yes_egfr_BinomCI[2], 1)
cardiovasc_more60_yes_CI_high_egfr <-round(cardiovasc_more60_yes_egfr_BinomCI[3], 1)
cardiovasc_more60_yes_CI_egfr <- paste(cardiovasc_more60_yes_CI_low_egfr, cardiovasc_more60_yes_CI_high_egfr, sep="-")
cardiovasc_more60_yes_prot <- sum(!is.na(GCKD_df1_more60$cardiovasc) & GCKD_df1_more60$cardiovasc == "cardiovasc_yes" & GCKD_df1_more60$incl_egfr == "prot")
cardiovasc_more60_yes_prot_BinomCI <- BinomCI(x=sum(GCKD_df1_more60$incl_egfr == "prot" & GCKD_df1_more60$cardiovasc == "cardiovasc_yes", na.rm=TRUE), 
                                              n=sum(GCKD_df1_more60$incl_egfr == "prot" & !is.na(GCKD_df1_more60$cardiovasc)),method="wilson") %>% '*'(100) %>% round(1)
cardiovasc_more60_yes_perc_prot <-round(cardiovasc_more60_yes_prot_BinomCI[1], 1)
cardiovasc_more60_yes_CI_low_prot <- round(cardiovasc_more60_yes_prot_BinomCI[2], 1)
cardiovasc_more60_yes_CI_high_prot <-round(cardiovasc_more60_yes_prot_BinomCI[3], 1)
cardiovasc_more60_yes_CI_prot <- paste(cardiovasc_more60_yes_CI_low_prot, cardiovasc_more60_yes_CI_high_prot, sep="-")
### clean and combine data
GCKD_df1_table_2 <- as_tibble(GCKD_df1_table_2)
Cardiovasc_less60_yes <- c("cardiovasc_less60_yes", cardiovasc_less60_yes_egfr, cardiovasc_less60_yes_prot, cardiovasc_less60_yes_perc_egfr, cardiovasc_less60_yes_CI_egfr, 
                           cardiovasc_less60_yes_perc_prot, cardiovasc_less60_yes_CI_prot)
Cardiovasc_more60_yes <- c("cardiovasc_more60_yes", cardiovasc_more60_yes_egfr, cardiovasc_more60_yes_prot, cardiovasc_more60_yes_perc_egfr, cardiovasc_more60_yes_CI_egfr, 
                           cardiovasc_more60_yes_perc_prot, cardiovasc_more60_yes_CI_prot)
tbl2_cardiovasc_less60 <- rbind(GCKD_df1_table_2, Cardiovasc_less60_yes)
tbl2_cardiovasc_60 <- rbind(tbl2_cardiovasc_less60, Cardiovasc_more60_yes)
tbl2_anonym <- subset(tbl2_cardiovasc_60, !(endsWith(tbl2_cardiovasc_60$Variable, "_no")) & 
                        tbl2_cardiovasc_60$Variable != "(Missing)" &
                        tbl2_cardiovasc_60$Variable != "Female" &
                        (!is.na(tbl2_cardiovasc_60$n_mean_egfr) | !is.na(tbl2_cardiovasc_60$n_mean_prot)))
setwd(path_tbl)
write.xlsx(tbl2_anonym, "tbl2_strictaverage_usecase_11.xlsx")

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
  modify_header(
    label = "Variable",
    stat_1 = "perc_ckd_diab",
    ci_stat_1 = "CI_ckd_diab",
    stat_10 = "perc_ckd_oth",
    ci_stat_10 = "CI_ckd_oth",
    stat_11 = "perc_ckd_lead_uk",
    ci_stat_11 = "CI_ckd_lead_uk",
    stat_2 = "perc_ckd_vask",
    ci_stat_2 = "CI_ckd_vask",
    stat_3 = "perc_ckd_syst",
    ci_stat_3 = "CI_ckd_syst",
    stat_4 = "perc_ckd_glom_prim",
    ci_stat_4 = "CI_ckd_glom_prim",
    stat_5 = "perc_ckd_interst",
    ci_stat_5 = "CI_ckd_interst",
    stat_6 = "perc_ckd_aki",
    ci_stat_6 = "CI_ckd_aki",
    stat_7 = "perc_ckd_single",
    ci_stat_7 = "CI_ckd_single",
    stat_8 = "perc_ckd_heredit",
    ci_stat_8 = "CI_ckd_heredit",
    stat_9 = "perc_ckd_obstr",
    ci_stat_9 = "CI_ckd_obstr") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_3 <-
  tbl_merge(tbls = list(GCKD_df1_table_3a, GCKD_df1_table_3b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_table_3 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_specific_strictaverage_table_3.html", 
             path = path_results)
GCKD_df1_table_3b <- as_tibble(GCKD_df1_table_3b)
setwd(path_tbl)
write.xlsx(GCKD_df1_table_3b, "tbl3_strictaverage_usecase_11.xlsx")


## Figure 3: Patient awareness and treatment
### identical to original data

## Table 4: Cardiovascular disease: identical to original data
### Cardiovasc - age calculations
#### k11
GCKD_df1_less60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[50, 60[" | GCKD_df1$BL_age == "[40, 50[" | GCKD_df1$BL_age == "[30, 40[" | GCKD_df1$BL_age == "[20, 30[")
GCKD_df1_more60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[60, 70[" | GCKD_df1$BL_age == "[70, 80[")
#### k11k2
GCKD_df1_less60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[20, 25[" | GCKD_df1$BL_age == "[25, 30[" | GCKD_df1$BL_age == "[30, 35[" | GCKD_df1$BL_age == "[35, 40[" | GCKD_df1$BL_age == "[40, 45[" | GCKD_df1$BL_age == "[45, 50[" | GCKD_df1$BL_age == "[50, 55[" | GCKD_df1$BL_age == "[55, 60[")
GCKD_df1_more60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[60, 65[" | GCKD_df1$BL_age == "[65, 70[" | GCKD_df1$BL_age == "[70, 75[" | GCKD_df1$BL_age == "[75, 80[")
#### all
cardiovasc_less60_yes <- sum(!is.na(GCKD_df1_less60$cardiovasc) & GCKD_df1_less60$cardiovasc == "cardiovasc_yes")
cardiovasc_less60_yes_BinomCI <- BinomCI(x=sum(GCKD_df1_less60$cardiovasc == "cardiovasc_yes", na.rm=TRUE), 
                                         n=sum(!is.na(GCKD_df1_less60$cardiovasc)),method="wilson") %>% '*'(100) %>% round(1)
cardiovasc_less60_yes_perc <-round(cardiovasc_less60_yes_BinomCI[1], 1)
cardiovasc_less60_yes_CI_low <- round(cardiovasc_less60_yes_BinomCI[2], 1)
cardiovasc_less60_yes_CI_high <-round(cardiovasc_less60_yes_BinomCI[3], 1)
cardiovasc_less60_yes_CI <- paste(cardiovasc_less60_yes_CI_low, cardiovasc_less60_yes_CI_high, sep="-")
cardiovasc_more60_yes <- sum(!is.na(GCKD_df1_more60$cardiovasc) & GCKD_df1_more60$cardiovasc == "cardiovasc_yes")
cardiovasc_more60_yes_BinomCI <- BinomCI(x=sum(GCKD_df1_more60$cardiovasc == "cardiovasc_yes", na.rm=TRUE), 
                                         n=sum(!is.na(GCKD_df1_more60$cardiovasc)),method="wilson") %>% '*'(100) %>% round(1)
cardiovasc_more60_yes_perc <-round(cardiovasc_more60_yes_BinomCI[1], 1)
cardiovasc_more60_yes_CI_low <- round(cardiovasc_more60_yes_BinomCI[2], 1)
cardiovasc_more60_yes_CI_high <-round(cardiovasc_more60_yes_BinomCI[3], 1)
cardiovasc_more60_yes_CI <- paste(cardiovasc_more60_yes_CI_low, cardiovasc_more60_yes_CI_high, sep="-")
#### build dataframe
Variable <- c("cardiovasc_less60_yes", "cardiovasc_more60_yes")
n_total <- c(cardiovasc_less60_yes, cardiovasc_more60_yes)
perc_total <- c(cardiovasc_less60_yes_perc, cardiovasc_more60_yes_perc)
CI_total <- c(cardiovasc_less60_yes_CI, cardiovasc_more60_yes_CI)
tbl4_cardiovasc_60 <- data.frame(Variable, n_total, perc_total, CI_total)
setwd(path_tbl)
write.xlsx(tbl4_cardiovasc_60, "tbl4_total_strictaverage_usecase_11.xlsx")

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
  modify_header(label = "Variable", stat_1 = "n_female_d", stat_2 = "n_female_nd") %>%
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
  modify_header(label = "Variable", stat_1 = "perc_female_d", ci_stat_1 = "CI_female_d", stat_2 = "perc_female_nd", ci_stat_2 = "CI_female_nd") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_fem_table_4 <-
  tbl_merge(tbls = list(GCKD_df1_fem_table_4a, GCKD_df1_fem_table_4b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_fem_table_4 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_specific_strictaverage_table_4_fem.html", 
             path = path_results)
### Cardiovasc - age calculation
#### k11
GCKD_df1_fem_less60 <- GCKD_df1_fem %>% subset(GCKD_df1_fem$BL_age == "[50, 60[" | GCKD_df1_fem$BL_age == "[40, 50[" | GCKD_df1_fem$BL_age == "[30, 40[" | GCKD_df1_fem$BL_age == "[20, 30[")
GCKD_df1_fem_more60 <- GCKD_df1_fem %>% subset(GCKD_df1_fem$BL_age == "[60, 70[" | GCKD_df1_fem$BL_age == "[70, 80[")
#### k11k2
GCKD_df1_fem_less60 <- GCKD_df1_fem %>% subset(GCKD_df1_fem$BL_age == "[20, 25[" | GCKD_df1_fem$BL_age == "[25, 30[" | GCKD_df1_fem$BL_age == "[30, 35[" | GCKD_df1_fem$BL_age == "[35, 40[" | GCKD_df1_fem$BL_age == "[40, 45[" | GCKD_df1_fem$BL_age == "[45, 50[" | GCKD_df1_fem$BL_age == "[50, 55[" | GCKD_df1_fem$BL_age == "[55, 60[")
GCKD_df1_fem_more60 <- GCKD_df1_fem %>% subset(GCKD_df1_fem$BL_age == "[60, 65[" | GCKD_df1_fem$BL_age == "[65, 70[" | GCKD_df1_fem$BL_age == "[70, 75[" | GCKD_df1_fem$BL_age == "[75, 80[")
#### all
cardiovasc_less60_yes_d <- sum(!is.na(GCKD_df1_fem_less60$cardiovasc) & GCKD_df1_fem_less60$cardiovasc == "cardiovasc_yes" & GCKD_df1_fem_less60$diabetes == "diabetes_yes")
cardiovasc_less60_yes_d_BinomCI <- BinomCI(x=sum(GCKD_df1_fem_less60$diabetes == "diabetes_yes" & GCKD_df1_fem_less60$cardiovasc == "cardiovasc_yes", na.rm=TRUE), 
                                           n=sum(GCKD_df1_fem_less60$diabetes == "diabetes_yes" &!is.na(GCKD_df1_fem_less60$cardiovasc)),method="wilson") %>% '*'(100) %>% round(1)
cardiovasc_less60_yes_perc_d <-round(cardiovasc_less60_yes_d_BinomCI[1], 1)
cardiovasc_less60_yes_CI_low_d <- round(cardiovasc_less60_yes_d_BinomCI[2], 1)
cardiovasc_less60_yes_CI_high_d <-round(cardiovasc_less60_yes_d_BinomCI[3], 1)
cardiovasc_less60_yes_CI_d <- paste(cardiovasc_less60_yes_CI_low_d, cardiovasc_less60_yes_CI_high_d, sep="-")
cardiovasc_less60_yes_nd <- sum(!is.na(GCKD_df1_fem_less60$cardiovasc) & GCKD_df1_fem_less60$cardiovasc == "cardiovasc_yes" & GCKD_df1_fem_less60$diabetes == "diabetes_no")
cardiovasc_less60_yes_nd_BinomCI <- BinomCI(x=sum(GCKD_df1_fem_less60$diabetes == "diabetes_no" & GCKD_df1_fem_less60$cardiovasc == "cardiovasc_yes", na.rm=TRUE), 
                                            n=sum(GCKD_df1_fem_less60$diabetes == "diabetes_no" & !is.na(GCKD_df1_fem_less60$cardiovasc)),method="wilson") %>% '*'(100) %>% round(1)
cardiovasc_less60_yes_perc_nd <-round(cardiovasc_less60_yes_nd_BinomCI[1], 1)
cardiovasc_less60_yes_CI_low_nd <- round(cardiovasc_less60_yes_nd_BinomCI[2], 1)
cardiovasc_less60_yes_CI_high_nd <-round(cardiovasc_less60_yes_nd_BinomCI[3], 1)
cardiovasc_less60_yes_CI_nd <- paste(cardiovasc_less60_yes_CI_low_nd, cardiovasc_less60_yes_CI_high_nd, sep="-")
cardiovasc_more60_yes_d <- sum(!is.na(GCKD_df1_fem_more60$cardiovasc) & GCKD_df1_fem_more60$cardiovasc == "cardiovasc_yes" & GCKD_df1_fem_more60$diabetes == "diabetes_yes")
cardiovasc_more60_yes_d_BinomCI <- BinomCI(x=sum(GCKD_df1_fem_more60$diabetes == "diabetes_yes" & GCKD_df1_fem_more60$cardiovasc == "cardiovasc_yes", na.rm=TRUE), 
                                           n=sum(GCKD_df1_fem_more60$diabetes == "diabetes_yes" &!is.na(GCKD_df1_fem_more60$cardiovasc)),method="wilson") %>% '*'(100) %>% round(1)
cardiovasc_more60_yes_perc_d <-round(cardiovasc_more60_yes_d_BinomCI[1], 1)
cardiovasc_more60_yes_CI_low_d <- round(cardiovasc_more60_yes_d_BinomCI[2], 1)
cardiovasc_more60_yes_CI_high_d <-round(cardiovasc_more60_yes_d_BinomCI[3], 1)
cardiovasc_more60_yes_CI_d <- paste(cardiovasc_more60_yes_CI_low_d, cardiovasc_more60_yes_CI_high_d, sep="-")
cardiovasc_more60_yes_nd <- sum(!is.na(GCKD_df1_fem_more60$cardiovasc) & GCKD_df1_fem_more60$cardiovasc == "cardiovasc_yes" & GCKD_df1_fem_more60$diabetes == "diabetes_no")
cardiovasc_more60_yes_nd_BinomCI <- BinomCI(x=sum(GCKD_df1_fem_more60$diabetes == "diabetes_no" & GCKD_df1_fem_more60$cardiovasc == "cardiovasc_yes", na.rm=TRUE), 
                                            n=sum(GCKD_df1_fem_more60$diabetes == "diabetes_no" & !is.na(GCKD_df1_fem_more60$cardiovasc)),method="wilson") %>% '*'(100) %>% round(1)
cardiovasc_more60_yes_perc_nd <-round(cardiovasc_more60_yes_nd_BinomCI[1], 1)
cardiovasc_more60_yes_CI_low_nd <- round(cardiovasc_more60_yes_nd_BinomCI[2], 1)
cardiovasc_more60_yes_CI_high_nd <-round(cardiovasc_more60_yes_nd_BinomCI[3], 1)
cardiovasc_more60_yes_CI_nd <- paste(cardiovasc_more60_yes_CI_low_nd, cardiovasc_more60_yes_CI_high_nd, sep="-")
### clean and combine data
GCKD_df1_fem_table_4 <- as_tibble(GCKD_df1_fem_table_4)
Cardiovasc_less60_yes <- c("cardiovasc_less60_yes", cardiovasc_less60_yes_d, cardiovasc_less60_yes_nd, cardiovasc_less60_yes_perc_d, cardiovasc_less60_yes_CI_d, 
                           cardiovasc_less60_yes_perc_nd, cardiovasc_less60_yes_CI_nd)
Cardiovasc_more60_yes <- c("cardiovasc_more60_yes", cardiovasc_more60_yes_d, cardiovasc_more60_yes_nd, cardiovasc_more60_yes_perc_d, cardiovasc_more60_yes_CI_d, 
                           cardiovasc_more60_yes_perc_nd, cardiovasc_more60_yes_CI_nd)
tbl4_cardiovasc_less60_fem <- rbind(GCKD_df1_fem_table_4, Cardiovasc_less60_yes)
tbl4_cardiovasc_60_fem <- rbind(tbl4_cardiovasc_less60_fem, Cardiovasc_more60_yes)
setwd(path_tbl)
write.xlsx(GCKD_df1_fem_table_4, "tbl4_female_strictaverage_usecase_11.xlsx")

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
  modify_header(label = "Variable", stat_1 = "n_male_d", stat_2 = "n_male_nd") %>%
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
  modify_header(label = "Variable", stat_1 = "perc_male_d", ci_stat_1 = "CI_male_d", stat_2 = "perc_male_nd", ci_stat_2 = "CI_male_nd") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_male_table_4 <-
  tbl_merge(tbls = list(GCKD_df1_male_table_4a, GCKD_df1_male_table_4b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_male_table_4 %>%
  as_gt() %>%
  gt::gtsave(filename = "tbl4_male_strictaverage_usecase_11.html", 
             path = path_results)
### Cardiovasc - age calculations
#### k11
GCKD_df1_male_less60 <- GCKD_df1_male %>% subset(GCKD_df1_male$BL_age == "[50, 60[" | GCKD_df1_male$BL_age == "[40, 50[" | GCKD_df1_male$BL_age == "[30, 40[" | GCKD_df1_male$BL_age == "[20, 30[")
GCKD_df1_male_more60 <- GCKD_df1_male %>% subset(GCKD_df1_male$BL_age == "[60, 70[" | GCKD_df1_male$BL_age == "[70, 80[")
#### k11k2
GCKD_df1_male_less60 <- GCKD_df1_male %>% subset(GCKD_df1_male$BL_age == "[20, 25[" | GCKD_df1_male$BL_age == "[25, 30[" | GCKD_df1_male$BL_age == "[30, 35[" | GCKD_df1_male$BL_age == "[35, 40[" | GCKD_df1_male$BL_age == "[40, 45[" | GCKD_df1_male$BL_age == "[45, 50[" | GCKD_df1_male$BL_age == "[50, 55[" | GCKD_df1_male$BL_age == "[55, 60[")
GCKD_df1_male_more60 <- GCKD_df1_male %>% subset(GCKD_df1_male$BL_age == "[60, 65[" | GCKD_df1_male$BL_age == "[65, 70[" | GCKD_df1_male$BL_age == "[70, 75[" | GCKD_df1_male$BL_age == "[75, 80[")
#### all
cardiovasc_less60_yes_d <- sum(!is.na(GCKD_df1_male_less60$cardiovasc) & GCKD_df1_male_less60$cardiovasc == "cardiovasc_yes" & GCKD_df1_male_less60$diabetes == "diabetes_yes")
cardiovasc_less60_yes_d_BinomCI <- BinomCI(x=sum(GCKD_df1_male_less60$diabetes == "diabetes_yes" & GCKD_df1_male_less60$cardiovasc == "cardiovasc_yes", na.rm=TRUE), 
                                           n=sum(GCKD_df1_male_less60$diabetes == "diabetes_yes" &!is.na(GCKD_df1_male_less60$cardiovasc)),method="wilson") %>% '*'(100) %>% round(1)
cardiovasc_less60_yes_perc_d <-round(cardiovasc_less60_yes_d_BinomCI[1], 1)
cardiovasc_less60_yes_CI_low_d <- round(cardiovasc_less60_yes_d_BinomCI[2], 1)
cardiovasc_less60_yes_CI_high_d <-round(cardiovasc_less60_yes_d_BinomCI[3], 1)
cardiovasc_less60_yes_CI_d <- paste(cardiovasc_less60_yes_CI_low_d, cardiovasc_less60_yes_CI_high_d, sep="-")
cardiovasc_less60_yes_nd <- sum(!is.na(GCKD_df1_male_less60$cardiovasc) & GCKD_df1_male_less60$cardiovasc == "cardiovasc_yes" & GCKD_df1_male_less60$diabetes == "diabetes_no")
cardiovasc_less60_yes_nd_BinomCI <- BinomCI(x=sum(GCKD_df1_male_less60$diabetes == "diabetes_no" & GCKD_df1_male_less60$cardiovasc == "cardiovasc_yes", na.rm=TRUE), 
                                            n=sum(GCKD_df1_male_less60$diabetes == "diabetes_no" & !is.na(GCKD_df1_male_less60$cardiovasc)),method="wilson") %>% '*'(100) %>% round(1)
cardiovasc_less60_yes_perc_nd <-round(cardiovasc_less60_yes_nd_BinomCI[1], 1)
cardiovasc_less60_yes_CI_low_nd <- round(cardiovasc_less60_yes_nd_BinomCI[2], 1)
cardiovasc_less60_yes_CI_high_nd <-round(cardiovasc_less60_yes_nd_BinomCI[3], 1)
cardiovasc_less60_yes_CI_nd <- paste(cardiovasc_less60_yes_CI_low_nd, cardiovasc_less60_yes_CI_high_nd, sep="-")
cardiovasc_more60_yes_d <- sum(!is.na(GCKD_df1_male_more60$cardiovasc) & GCKD_df1_male_more60$cardiovasc == "cardiovasc_yes" & GCKD_df1_male_more60$diabetes == "diabetes_yes")
cardiovasc_more60_yes_d_BinomCI <- BinomCI(x=sum(GCKD_df1_male_more60$diabetes == "diabetes_yes" & GCKD_df1_male_more60$cardiovasc == "cardiovasc_yes", na.rm=TRUE), 
                                           n=sum(GCKD_df1_male_more60$diabetes == "diabetes_yes" &!is.na(GCKD_df1_male_more60$cardiovasc)),method="wilson") %>% '*'(100) %>% round(1)
cardiovasc_more60_yes_perc_d <-round(cardiovasc_more60_yes_d_BinomCI[1], 1)
cardiovasc_more60_yes_CI_low_d <- round(cardiovasc_more60_yes_d_BinomCI[2], 1)
cardiovasc_more60_yes_CI_high_d <-round(cardiovasc_more60_yes_d_BinomCI[3], 1)
cardiovasc_more60_yes_CI_d <- paste(cardiovasc_more60_yes_CI_low_d, cardiovasc_more60_yes_CI_high_d, sep="-")
cardiovasc_more60_yes_nd <- sum(!is.na(GCKD_df1_male_more60$cardiovasc) & GCKD_df1_male_more60$cardiovasc == "cardiovasc_yes" & GCKD_df1_male_more60$diabetes == "diabetes_no")
cardiovasc_more60_yes_nd_BinomCI <- BinomCI(x=sum(GCKD_df1_male_more60$diabetes == "diabetes_no" & GCKD_df1_male_more60$cardiovasc == "cardiovasc_yes", na.rm=TRUE), 
                                            n=sum(GCKD_df1_male_more60$diabetes == "diabetes_no" & !is.na(GCKD_df1_male_more60$cardiovasc)),method="wilson") %>% '*'(100) %>% round(1)
cardiovasc_more60_yes_perc_nd <-round(cardiovasc_more60_yes_nd_BinomCI[1], 1)
cardiovasc_more60_yes_CI_low_nd <- round(cardiovasc_more60_yes_nd_BinomCI[2], 1)
cardiovasc_more60_yes_CI_high_nd <-round(cardiovasc_more60_yes_nd_BinomCI[3], 1)
cardiovasc_more60_yes_CI_nd <- paste(cardiovasc_more60_yes_CI_low_nd, cardiovasc_more60_yes_CI_high_nd, sep="-")
### clean and combine data
GCKD_df1_male_table_4 <- as_tibble(GCKD_df1_male_table_4)
Cardiovasc_less60_yes <- c("cardiovasc_less60_yes", cardiovasc_less60_yes_d, cardiovasc_less60_yes_nd, cardiovasc_less60_yes_perc_d, cardiovasc_less60_yes_CI_d, 
                           cardiovasc_less60_yes_perc_nd, cardiovasc_less60_yes_CI_nd)
Cardiovasc_more60_yes <- c("cardiovasc_more60_yes", cardiovasc_more60_yes_d, cardiovasc_more60_yes_nd, cardiovasc_more60_yes_perc_d, cardiovasc_more60_yes_CI_d, 
                           cardiovasc_more60_yes_perc_nd, cardiovasc_more60_yes_CI_nd)
tbl4_cardiovasc_less60_male <- rbind(GCKD_df1_male_table_4, Cardiovasc_less60_yes)
tbl4_cardiovasc_60_male <- rbind(tbl4_cardiovasc_less60_male, Cardiovasc_more60_yes)
setwd(path_tbl)
write.xlsx(tbl4_cardiovasc_60_male, "tbl4_male_strictaverage_usecase_11.xlsx")

## Table 4 merge subsets
tbl4_female_male_origin <- full_join(x = tbl4_cardiovasc_60_male, y = tbl4_cardiovasc_60_fem, by = "Variable")
tbl4_total_female_male_origin <- full_join(x = tbl4_female_male_origin, y = tbl4_cardiovasc_60, by = "Variable")
tbl4_origin <- subset(tbl4_total_female_male_origin, !(endsWith(tbl4_total_female_male_origin$Variable, "_no")) & 
                        tbl4_total_female_male_origin$Variable != "(Missing)" &
                        tbl4_total_female_male_origin$Variable != "Female" &
                        tbl4_total_female_male_origin$Variable != "Male" &
                        (!is.na(tbl4_total_female_male_origin$n_female_d) | !is.na(tbl4_total_female_male_origin$n_male_d)))
setwd(path_tbl)
write.xlsx(tbl4_origin, "tbl4_strictaverage_usecase_11.xlsx")

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
    statistic = list(all_categorical() ~ "{n}"),
    digits = all_continuous() ~ 1,
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_1 = "n_mean_DMwDN", stat_2 = "n_mean_DMwoDN", stat_3 = "n_mean_NoDM") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_s3b <-
  GCKD_df1 %>%
  select(all_of(var_table_s3)) %>%
  tbl_summary(
    by = DM,
    statistic = list(all_categorical() ~ "{p}"),
    digits = ~ 1,
    missing_text = "(Missing)") %>%
  add_ci(statistic=list(all_categorical() ~ "{conf.low}-{conf.high}", all_continuous() ~ "{conf.low}-{conf.high}"),
         style_fun=list(all_categorical() ~ purrr::partial(style_sigfig, digits=3, scale = 100), 
                        all_continuous() ~ purrr::partial(style_sigfig, digits=3))) %>%
  modify_header(label = "Variable", stat_1 = "perc_DMwDN", ci_stat_1 = "CI_DMwDN", stat_2 = "perc_DMwoDN", ci_stat_2 = "CI_DMwoDN", stat_3 = "perc_NoDM", ci_stat_3 = "CI_NoDM") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_s3 <-
  tbl_merge(tbls = list(GCKD_df1_table_s3a, GCKD_df1_table_s3b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_table_s3 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_specific_strictaverage_table_s3.html", 
             path = path_results)
### calculated values in Table S3: identical to original
GCKD_df1_table_s3 <- as_tibble(GCKD_df1_table_s3)
tbls3_anonym <- subset(GCKD_df1_table_s3, !(endsWith(GCKD_df1_table_s3$Variable, "_no")) & 
                         GCKD_df1_table_s3$Variable != "(Missing)" &
                         (!is.na(GCKD_df1_table_s3$n_mean_DMwDN) | !is.na(GCKD_df1_table_s3$n_mean_DMwoDN)))
setwd(path_tbl)
write.xlsx(tbls3_anonym, "tbls3_strictaverage_usecase_11.xlsx")

### calculated values in Table S3: BMI groups
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
