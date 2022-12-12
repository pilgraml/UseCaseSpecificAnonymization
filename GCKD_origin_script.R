# Packages 
pacman::p_load(tidyr, stringr, dplyr, openxlsx, naniar, emmeans, multcomp, 
               plyr, finalfit, ggplot2, tibble, lmtest, sandwich,
               tidyverse, tidyselect, summarytools, scales, gridExtra, 
               lubridate, eeptools, gtsummary, flextable, boot, mosaic, patchwork, rms, coxed, DescTools, PropCIs)

# Dataset
path_data = "D:/BackUp GCKD"
setwd(path_data)
GCKD_df1 <- as_tibble(read.xlsx("GCKD_df1_origin.xlsx", sep = ";"))
dim(GCKD_df1)

# Data Cleansing
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
col_num <- c("BL_ku_sys", "BL_ku_dia", "BL_ku_map", "BL_ku_ruhepuls", "BL_creavalue", "BL_cysvalue", 
             "BL_gfr_mdrd", "BL_gfr_ckdepi", "BL_gfr_ckdepi_cys", "BL_gfr_ckdepi_creacys", "BL_uacr", "BL_ku_bmi", "BL_age", "BL_ku_height_cm", "BL_ku_weight")
GCKD_df1 <- GCKD_df1 %>% mutate(across(all_of(col_num), as.numeric))
GCKD_df1 <- GCKD_df1 %>% mutate(DM = ifelse(GCKD_df1$diabetes == 2, "No DM", ifelse(GCKD_df1$ckd_lead == "ckd_diab", "DMwDN", "DMwoDN")))

# Titze et al: Disease burden and risk profile in referred patients with moderate chronic kidney disease: composition of the German Chronic Kidney Disease (GCKD) cohort
## Table 1 including 95% CI for distribution or mean, BL_uacr not included
## perc for continous SD
var_table_1 <- c("BL_age", "dem_sex", "aa_stroke", "aa_myocard", "aa_hypertens", "aa_diabetes", "aa_renal", 
                 "aa_renal_stones", "aa_dialyse", "aa_ntx", "smoking", "hospital", "BL_ku_height_cm","BL_ku_weight", 
                 "BL_ku_bmi", "BL_ku_sys", "BL_ku_dia", "BL_ku_map", "BL_ku_ruhepuls", "BL_creavalue", "BL_cysvalue",
                 "BL_gfr_mdrd", "BL_med_raas_ace", "BL_med_raas_at1", "BL_med_raas_double", 
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
  modify_header(label = "Variable", stat_0 = "n_mean_total") %>%
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
  modify_header(label = "Variable", stat_0 = "perc_total", ci_stat_0 = "CI_total") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_1 <-
  tbl_merge(tbls = list(GCKD_df1_table_1a, GCKD_df1_table_1b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_table_1 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_origin_table_1.html", 
             path = path_results)
GCKD_df1_table_1 <- as_tibble(GCKD_df1_table_1)
setwd(path_results)
write.xlsx(GCKD_df1_table_1, "tbl1_total_origin.xlsx")

### calculated values in Table 1
table(GCKD_df1$BL_ku_bmi > 30,  useNA = "always")
BinomCI(x=sum(GCKD_df1$BL_ku_bmi > 30, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$BL_ku_bmi <= 30 & GCKD_df1$BL_ku_bmi > 25,  useNA = "always")
BinomCI(x=sum(GCKD_df1$BL_ku_bmi <= 30 & GCKD_df1$BL_ku_bmi > 25, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$BL_ku_bmi <= 25,  useNA = "always")
BinomCI(x=sum(GCKD_df1$BL_ku_bmi <= 25, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$BL_ku_sys < 130 & GCKD_df1$BL_ku_dia < 80,  useNA = "always")
BinomCI(x=sum(GCKD_df1$BL_ku_sys < 130 & GCKD_df1$BL_ku_dia < 80, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_ku_sys)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$BL_ku_sys < 140 & GCKD_df1$BL_ku_dia < 90,  useNA = "always")
BinomCI(x=sum(GCKD_df1$BL_ku_sys < 140 & GCKD_df1$BL_ku_dia < 90, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_ku_sys)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$BL_gfr_mdrd >= 60,  useNA = "always")
BinomCI(x=sum(GCKD_df1$BL_gfr_mdrd >= 60, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60,  useNA = "always")
BinomCI(x=sum(GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45,  useNA = "always")
BinomCI(x=sum(GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$BL_gfr_mdrd < 30,  useNA = "always")
BinomCI(x=sum(GCKD_df1$BL_gfr_mdrd < 30, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
summary(GCKD_df1$BL_uacr)
MedianCI(GCKD_df1$BL_uacr, na.rm=TRUE)
table(GCKD_df1$BL_uacr < 30,  useNA = "always")
BinomCI(x=sum(GCKD_df1$BL_uacr < 30, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300,  useNA = "always")
BinomCI(x=sum(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$BL_uacr > 300,  useNA = "always")
BinomCI(x=sum(GCKD_df1$BL_uacr > 300, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)

## Table 1 subset female
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
  gt::gtsave(filename = "GCKD_origin_table_1_fem.html", 
             path = path_results)
### calculated values in Table 1 subset female
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi > 30,  useNA = "always")
BinomCI(x=sum(GCKD_df1_fem$diabetes =="1" & GCKD_df1_fem$BL_ku_bmi > 30, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes == "1" & !is.na(GCKD_df1_fem$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem$diabetes =="2" & GCKD_df1_fem$BL_ku_bmi > 30, na.rm=TRUE), sum(GCKD_df1_fem$diabetes == "2" & !is.na(GCKD_df1_fem$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi <= 30 & GCKD_df1_fem$BL_ku_bmi > 25,  useNA = "always")
BinomCI(x=sum(GCKD_df1_fem$diabetes =="1" & GCKD_df1_fem$BL_ku_bmi <= 30 & GCKD_df1_fem$BL_ku_bmi > 25, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes == "1" & !is.na(GCKD_df1_fem$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem$diabetes =="2" & GCKD_df1_fem$BL_ku_bmi <= 30 & GCKD_df1_fem$BL_ku_bmi > 25, na.rm=TRUE), sum(GCKD_df1_fem$diabetes == "2" & !is.na(GCKD_df1_fem$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi <= 25,  useNA = "always")
BinomCI(x=sum(GCKD_df1_fem$diabetes =="1" & GCKD_df1_fem$BL_ku_bmi <= 25, na.rm=TRUE), n=sum(GCKD_df1_fem$diabetes == "1" & !is.na(GCKD_df1_fem$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem$diabetes =="2" & GCKD_df1_fem$BL_ku_bmi <= 25, na.rm=TRUE), sum(GCKD_df1_fem$diabetes == "2" & !is.na(GCKD_df1_fem$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
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

## Table 1 subset male
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
  gt::gtsave(filename = "GCKD_origin_table_1_male.html", 
             path = path_results)
### calculated values in Table 1 subset female
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi > 30,  useNA = "always")
BinomCI(x=sum(GCKD_df1_male$diabetes =="1" & GCKD_df1_male$BL_ku_bmi > 30, na.rm=TRUE), sum(GCKD_df1_male$diabetes == "1" & !is.na(GCKD_df1_male$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male$diabetes =="2" & GCKD_df1_male$BL_ku_bmi > 30, na.rm=TRUE), sum(GCKD_df1_male$diabetes == "2" & !is.na(GCKD_df1_male$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi <= 30 & GCKD_df1_male$BL_ku_bmi > 25,  useNA = "always")
BinomCI(x=sum(GCKD_df1_male$diabetes =="1" & GCKD_df1_male$BL_ku_bmi <= 30 & GCKD_df1_male$BL_ku_bmi > 25, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes == "1" & !is.na(GCKD_df1_male$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male$diabetes =="2" & GCKD_df1_male$BL_ku_bmi <= 30 & GCKD_df1_male$BL_ku_bmi > 25, na.rm=TRUE), sum(GCKD_df1_male$diabetes == "2" & !is.na(GCKD_df1_male$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi <= 25,  useNA = "always")
BinomCI(x=sum(GCKD_df1_male$diabetes =="1" & GCKD_df1_male$BL_ku_bmi <= 25, na.rm=TRUE), n=sum(GCKD_df1_male$diabetes == "1" & !is.na(GCKD_df1_male$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male$diabetes =="2" & GCKD_df1_male$BL_ku_bmi <= 25, na.rm=TRUE), sum(GCKD_df1_male$diabetes == "2" & !is.na(GCKD_df1_male$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
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

## Figure 1: KDIGO categories
BinomCI(x=sum(GCKD_df1$BL_uacr < 30 & GCKD_df1$BL_gfr_mdrd >= 90, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300 & GCKD_df1$BL_gfr_mdrd >= 90, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_uacr > 300 & GCKD_df1$BL_gfr_mdrd >= 90, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_uacr < 30 & GCKD_df1$BL_gfr_mdrd >= 60 & GCKD_df1$BL_gfr_mdrd < 90, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300 & GCKD_df1$BL_gfr_mdrd >= 60 & GCKD_df1$BL_gfr_mdrd < 90, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_uacr > 300 & GCKD_df1$BL_gfr_mdrd >= 60 & GCKD_df1$BL_gfr_mdrd < 90, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_uacr < 30 & GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300 & GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_uacr > 300 & GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_uacr < 30 & GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300 & GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_uacr > 300 & GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_uacr < 30 & GCKD_df1$BL_gfr_mdrd >= 15 & GCKD_df1$BL_gfr_mdrd < 30, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300 & GCKD_df1$BL_gfr_mdrd >= 15 & GCKD_df1$BL_gfr_mdrd < 30, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_uacr > 300 & GCKD_df1$BL_gfr_mdrd >= 15 & GCKD_df1$BL_gfr_mdrd < 30, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_uacr < 30 & GCKD_df1$BL_gfr_mdrd < 15, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300 & GCKD_df1$BL_gfr_mdrd < 15, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$BL_uacr > 300 & GCKD_df1$BL_gfr_mdrd < 15, na.rm=TRUE), n=sum(!is.na(GCKD_df1$BL_uacr)), method="wilson")

## Table 2: Characteristics grouped by inclusion criteria 
table(GCKD_df1$incl_egfr)
var_table_2 <- c("BL_age", "dem_sex", "aa_stroke", "aa_myocard", "aa_hypertens", "aa_diabetes", "aa_renal", 
                 "aa_renal_stones", "aa_dialyse", "aa_ntx", "smoking", "diabetes", "BL_creavalue", "BL_cysvalue",
                 "BL_gfr_mdrd", "biopsy", "ckd_lead", "incl_egfr")
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
  gt::gtsave(filename = "GCKD_origin_table_2.html", 
             path = path_results)
### calculated values in Table 2
GCKD_df1_less60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age <= 60)
table(GCKD_df1_less60$incl_egfr, GCKD_df1_less60$cardiovasc == "1",  useNA = "always")
BinomCI(x=sum(GCKD_df1_less60$incl_egfr == "1" & GCKD_df1_less60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_less60$incl_egfr == "1" & !is.na(GCKD_df1_less60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_less60$incl_egfr == "2" & GCKD_df1_less60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_less60$incl_egfr == "2" & !is.na(GCKD_df1_less60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
GCKD_df1_more60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age > 60)
table(GCKD_df1_more60$incl_egfr, GCKD_df1_more60$cardiovasc == "1",  useNA = "always") 
BinomCI(x=sum(GCKD_df1_more60$incl_egfr == "1" & GCKD_df1_more60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_more60$incl_egfr == "1" & !is.na(GCKD_df1_more60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_more60$incl_egfr == "2" & GCKD_df1_more60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_more60$incl_egfr == "2" & !is.na(GCKD_df1_more60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
GCKD_df1 %>% group_by(incl_egfr) %>%
  summarise_at(vars(BL_uacr), list(~ round(quantile(., na.rm=TRUE), 1)))
GCKD_df1_gfr <- GCKD_df1 %>% subset(incl_egfr == "1")
GCKD_df1_protein <- GCKD_df1 %>% subset(incl_egfr == "2")
MedianCI(GCKD_df1_gfr$BL_uacr, na.rm=TRUE)
MedianCI(GCKD_df1_protein$BL_uacr, na.rm=TRUE)

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
BinomCI(x=sum(GCKD_df1$diabetes == "1" & GCKD_df1$ckd_lead == "ckd_diab", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_diab" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$diabetes == "1" & GCKD_df1$ckd_lead == "ckd_vask", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_vask" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$diabetes == "1" & GCKD_df1$ckd_lead == "ckd_syst", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_syst" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$diabetes == "1" & GCKD_df1$ckd_lead == "ckd_glom_prim", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_glom_prim" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$diabetes == "1" & GCKD_df1$ckd_lead == "ckd_interst", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_interst" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$diabetes == "1" & GCKD_df1$ckd_lead == "ckd_aki", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_aki" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$diabetes == "1" & GCKD_df1$ckd_lead == "ckd_single", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_single" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$diabetes == "1" & GCKD_df1$ckd_lead == "ckd_heredit", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_heredit" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$diabetes == "1" & GCKD_df1$ckd_lead == "ckd_obstr", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_obstr" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$diabetes == "1" & GCKD_df1$ckd_lead == "ckd_oth", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_oth" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$diabetes == "1" & GCKD_df1$ckd_lead == "ckd_lead_uk", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_lead_uk" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
prop.table(table(GCKD_df1$hypertension, GCKD_df1$ckd_lead), 2)
BinomCI(x=sum(GCKD_df1$hypertension == "1" & GCKD_df1$ckd_lead == "ckd_diab", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_diab" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$hypertension == "1" & GCKD_df1$ckd_lead == "ckd_vask", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_vask" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$hypertension == "1" & GCKD_df1$ckd_lead == "ckd_syst", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_syst" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$hypertension == "1" & GCKD_df1$ckd_lead == "ckd_glom_prim", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_glom_prim" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$hypertension == "1" & GCKD_df1$ckd_lead == "ckd_interst", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_interst" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$hypertension == "1" & GCKD_df1$ckd_lead == "ckd_aki", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_aki" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$hypertension == "1" & GCKD_df1$ckd_lead == "ckd_single", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_single" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$hypertension == "1" & GCKD_df1$ckd_lead == "ckd_heredit", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_heredit" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$hypertension == "1" & GCKD_df1$ckd_lead == "ckd_obstr", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_obstr" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$hypertension == "1" & GCKD_df1$ckd_lead == "ckd_oth", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_oth" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$hypertension == "1" & GCKD_df1$ckd_lead == "ckd_lead_uk", na.rm=TRUE), n=sum(GCKD_df1$ckd_lead == "ckd_lead_uk" & !is.na(GCKD_df1$ckd_lead)), method="wilson") %>% '*'(100) %>% round(1)

## Table 3: Causes of CKD leading & additional
GCKD_df1 <- GCKD_df1 %>% mutate(ckd_add_diab = ifelse(GCKD_df1$ckd_lead == "ckd_diab", "2", ifelse(GCKD_df1$ckd_diab == "1", "1", "2")))
GCKD_df1 <- GCKD_df1 %>% mutate(ckd_add_vask = ifelse(GCKD_df1$ckd_lead == "ckd_vask", "2", ifelse(GCKD_df1$ckd_vasc == "1", "1", "2")))
GCKD_df1 <- GCKD_df1 %>% mutate(ckd_add_syst = ifelse(GCKD_df1$ckd_lead == "ckd_syst", "2", ifelse(GCKD_df1$ckd_syst == "1", "1", "2")))
GCKD_df1 <- GCKD_df1 %>% mutate(ckd_add_glom_prim = ifelse(GCKD_df1$ckd_lead == "ckd_glom_prim", "2", ifelse(GCKD_df1$ckd_glom_prim == "1", "1", "2")))
GCKD_df1 <- GCKD_df1 %>% mutate(ckd_add_interst = ifelse(GCKD_df1$ckd_lead == "ckd_interst", "2", ifelse(GCKD_df1$ckd_interst == "1", "1", "2")))
GCKD_df1 <- GCKD_df1 %>% mutate(ckd_add_aki = ifelse(GCKD_df1$ckd_lead == "ckd_aki", "2", ifelse(GCKD_df1$ckd_aki == "1", "1", "2")))
GCKD_df1 <- GCKD_df1 %>% mutate(ckd_add_single = ifelse(GCKD_df1$ckd_lead == "ckd_single", "2", ifelse(GCKD_df1$ckd_single == "1", "1", "2")))
GCKD_df1 <- GCKD_df1 %>% mutate(ckd_add_heredit = ifelse(GCKD_df1$ckd_lead == "ckd_heredit", "2", ifelse(GCKD_df1$ckd_heredit == "1", "1", "2")))
GCKD_df1 <- GCKD_df1 %>% mutate(ckd_add_obstr = ifelse(GCKD_df1$ckd_lead == "ckd_obstr", "2", ifelse(GCKD_df1$ckd_obstr == "1", "1", "2")))
GCKD_df1 <- GCKD_df1 %>% mutate(ckd_add_oth = ifelse(GCKD_df1$ckd_lead == "ckd_oth", "2", ifelse(GCKD_df1$ckd_oth == "1", "1", "2")))
GCKD_df1 <- GCKD_df1 %>% mutate(ckd_add = ifelse(ckd_add_diab == "1" | ckd_add_vask == "1" | ckd_add_syst == "1" | ckd_add_glom_prim == "1" | ckd_add_interst == "1" | ckd_add_aki == "1" | 
                                                   ckd_add_single == "1" | ckd_add_heredit == "1" | ckd_add_obstr == "1" | ckd_add_oth == "1", "1", 
                                                 ifelse(ckd_add_diab == "2" & ckd_add_vask == "2" & ckd_add_syst == "2" & ckd_add_glom_prim == "2" & ckd_add_interst == "2" & 
                                                          ckd_add_aki == "2" & ckd_add_single == "2" & ckd_add_heredit == "2" & ckd_add_obstr == "2" & ckd_add_oth == "2", "2", "ckd_add_uk")))
table(GCKD_df1$ckd_lead, useNA = "always")
prop.table(table(GCKD_df1$ckd_lead)) %>% '*'(100) %>% round(1)
MultinomCI(table(GCKD_df1$ckd_lead), method="wilson") %>% '*'(100) %>% round(1)
var_table_3 <- c("ckd_lead", "ckd_add_diab", "ckd_add_vask", "ckd_add_syst", "ckd_add_glom_prim", "ckd_add_interst", 
                 "ckd_add_aki", "ckd_add_single", "ckd_add_heredit", "ckd_add_obstr", "ckd_add_oth", "ckd_add", "ckd_uk", "biopsy")
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
  gt::gtsave(filename = "GCKD_origin_table_3.html", 
             path = path_results)

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
var_table_4 <- c("dem_sex", "diabetes", "cardiovasc", "hypertension", "valve", "coronary", "myocard", "bypass", "ptca", "cerebrovasc", 
             "stroke", "carotic_surg", "carotic_interv", "pavk", "amput", "ygraft", "pavk_surgery", "pta")
theme_gtsummary_language("en", big.mark = "")
GCKD_df1_table_4a <-
  GCKD_df1 %>%
  select(all_of(var_table_4)) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n}"),
    missing_text = "(Missing)") %>%
  modify_header(label = "Variable", stat_0 = "n") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_4b <-
  GCKD_df1 %>%
  select(all_of(var_table_4)) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{p}"),
    digits = ~ 1,
    missing_text = "(Missing)") %>%
  add_ci(statistic=list(all_categorical() ~ "{conf.low}-{conf.high}", all_continuous() ~ "{conf.low}-{conf.high}"),
         style_fun=list(all_categorical() ~ purrr::partial(style_sigfig, digits=3, scale = 100), 
                        all_continuous() ~ purrr::partial(style_sigfig, digits=3))) %>%
  modify_header(label = "Variable", stat_0 = "%") %>%
  modify_footnote(update = everything() ~ NA)
GCKD_df1_table_4 <-
  tbl_merge(tbls = list(GCKD_df1_table_4a, GCKD_df1_table_4b)) %>%
  modify_spanning_header(everything() ~ NA_character_)
GCKD_df1_table_4 %>%
  as_gt() %>%
  gt::gtsave(filename = "GCKD_origin_table_4.html", 
             path = path_results)
### calculated values in Table 4
GCKD_df1_less60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age <= 60)
table(GCKD_df1_less60$cardiovasc == "1",  useNA = "always") 
BinomCI(x=sum(GCKD_df1_less60$cardiovasc == "1", na.rm=TRUE), n=sum(!is.na(GCKD_df1_less60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
GCKD_df1_more60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age > 60)
table(GCKD_df1_more60$cardiovasc == "1",  useNA = "always")
BinomCI(x=sum(GCKD_df1_more60$cardiovasc == "1", na.rm=TRUE), n=sum(!is.na(GCKD_df1_more60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)

## Table 4 subset female
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
  gt::gtsave(filename = "GCKD_origin_table_4_fem.html", 
             path = path_results)
### calculated values in Table 4 subset female
GCKD_df1_fem_less60 <- GCKD_df1_fem %>% subset(GCKD_df1_fem$BL_age <= 60)
table(GCKD_df1_fem_less60$diabetes, GCKD_df1_fem_less60$cardiovasc == 1,  useNA = "always")  
BinomCI(x=sum(GCKD_df1_fem_less60$diabetes == "1" & GCKD_df1_fem_less60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_fem_less60$diabetes == "1" & !is.na(GCKD_df1_fem_less60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_fem_less60$diabetes == "2" & GCKD_df1_fem_less60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_fem_less60$diabetes == "2" & !is.na(GCKD_df1_fem_less60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
GCKD_df1_fem_more60 <- GCKD_df1_fem %>% subset(GCKD_df1_fem$BL_age > 60)
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
  gt::gtsave(filename = "GCKD_origin_table_4_male.html", 
             path = path_results)
### calculated values in Table 4 subset male
GCKD_df1_male_less60 <- GCKD_df1_male %>% subset(GCKD_df1_male$BL_age <= 60)
table(GCKD_df1_male_less60$diabetes, GCKD_df1_male_less60$cardiovasc == 1,  useNA = "always")  
BinomCI(x=sum(GCKD_df1_male_less60$diabetes == "1" & GCKD_df1_male_less60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_male_less60$diabetes == "1" & !is.na(GCKD_df1_male_less60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male_less60$diabetes == "2" & GCKD_df1_male_less60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_male_less60$diabetes == "2" & !is.na(GCKD_df1_male_less60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
GCKD_df1_male_more60 <- GCKD_df1_male %>% subset(GCKD_df1_male$BL_age > 60)
table(GCKD_df1_male_more60$diabetes, GCKD_df1_male_more60$cardiovasc == 1,  useNA = "always")  
BinomCI(x=sum(GCKD_df1_male_more60$diabetes == "1" & GCKD_df1_male_more60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_male_more60$diabetes == "1" & !is.na(GCKD_df1_male_more60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1_male_more60$diabetes == "2" & GCKD_df1_male_more60$cardiovasc == "1", na.rm=TRUE), n=sum(GCKD_df1_male_more60$diabetes == "2" & !is.na(GCKD_df1_male_more60$cardiovasc)), method="wilson") %>% '*'(100) %>% round(1)

## Suppl. Table 2: GFR categories
GCKD_df1_stbl2 <- c("BL_gfr_mdrd", "BL_gfr_ckdepi", "BL_gfr_ckdepi_cys","BL_gfr_ckdepi_creacys")
for(i in GCKD_df1_stbl2) {
  no_90 <- sum(GCKD_df1[[i]] >= 90, na.rm=TRUE)
  p95ci_90 <- BinomCI(x=sum(GCKD_df1[[i]] >= 90, na.rm=TRUE), n=sum(!is.na(GCKD_df1[[i]])), method="wilson") %>% '*'(100) %>% round(1)
  no_6090 <- sum(GCKD_df1[[i]] >=60 & GCKD_df1[[i]] < 90, na.rm=TRUE)
  p95ci_6090 <- BinomCI(x=sum(GCKD_df1[[i]] >=60 & GCKD_df1[[i]] < 90, na.rm=TRUE), n=sum(!is.na(GCKD_df1[[i]])), method="wilson") %>% '*'(100) %>% round(1)
  no_4560 <- sum(GCKD_df1[[i]] >=45 & GCKD_df1[[i]] < 60, na.rm=TRUE)
  p95ci_4560 <- BinomCI(x=sum(GCKD_df1[[i]] >=45 & GCKD_df1[[i]] < 60, na.rm=TRUE), n=sum(!is.na(GCKD_df1[[i]])), method="wilson") %>% '*'(100) %>% round(1)
  no_3045 <- sum(GCKD_df1[[i]] >=30 & GCKD_df1[[i]] < 45, na.rm=TRUE)
  p95ci_3045 <- BinomCI(x=sum(GCKD_df1[[i]] >=30 & GCKD_df1[[i]] < 45, na.rm=TRUE), n=sum(!is.na(GCKD_df1[[i]])), method="wilson") %>% '*'(100) %>% round(1)
  no_30 <- sum(GCKD_df1[[i]] < 30, na.rm=TRUE)
  p95ci_30 <- BinomCI(x=sum(GCKD_df1[[i]] < 30, na.rm=TRUE), n=sum(!is.na(GCKD_df1[[i]])), method="wilson") %>% '*'(100) %>% round(1)
  meansd <- sd(GCKD_df1[[i]], na.rm=TRUE)
  mean95ci <- MeanCI(GCKD_df1[[i]], na.rm=TRUE) %>% round(1)
  cat(i, "\nNo 15-29\n", no_30, "\nPerc 95% CI 15-29\n", p95ci_30, "\nNo 30-44\n", no_3045, "\nPerc 95% CI 30-44\n", p95ci_3045, 
      "\nNo 45-59\n", no_4560, "\nPerc 95% CI 45-59\n", p95ci_4560, "\nNo 60-89\n", no_6090, "\nPerc 95% CI 60-89\n", p95ci_6090, 
      "\nNo >= 90\n", no_90, "\nPerc 95% CI >= 90\n", p95ci_90, "\nSD\n", meansd, "\nMean & 95% CI\n", mean95ci, "\n")
}

## Suppl. Table 3: Diabetic nephropathy
var_table_s3 <- c("BL_age", "education", "aa_stroke", "aa_myocard", "aa_hypertens", "aa_diabetes", "aa_renal", 
                 "aa_renal_stones", "aa_dialyse", "aa_ntx", "smoking", "hospital", "BL_ku_height_cm","BL_ku_weight", 
                 "BL_ku_bmi", "BL_ku_sys", "BL_ku_dia", "BL_ku_map", "BL_ku_ruhepuls", "BL_creavalue", "BL_cysvalue",
                 "BL_gfr_mdrd", "BL_med_raas_ace", "BL_med_raas_at1", "BL_med_raas_double", 
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
  gt::gtsave(filename = "GCKD_origin_table_s3.html", 
             path = path_results)
### calculated values in Table S3
table(GCKD_df1$DM, GCKD_df1$BL_ku_bmi > 30,  useNA = "always")
BinomCI(x=sum(GCKD_df1$DM =="DMwDN" & GCKD_df1$BL_ku_bmi > 30, na.rm=TRUE), n=sum(GCKD_df1$DM == "DMwDN" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="DMwoDN" & GCKD_df1$BL_ku_bmi > 30, na.rm=TRUE), sum(GCKD_df1$DM == "DMwoDN" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="No DM" & GCKD_df1$BL_ku_bmi > 30, na.rm=TRUE), sum(GCKD_df1$DM == "No DM" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$DM, GCKD_df1$BL_ku_bmi <= 30 & GCKD_df1$BL_ku_bmi > 25,  useNA = "always")
BinomCI(x=sum(GCKD_df1$DM =="DMwDN" & GCKD_df1$BL_ku_bmi <= 30 & GCKD_df1$BL_ku_bmi > 25, na.rm=TRUE), n=sum(GCKD_df1$DM == "DMwDN" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="DMwoDN" & GCKD_df1$BL_ku_bmi <= 30 & GCKD_df1$BL_ku_bmi > 25, na.rm=TRUE), sum(GCKD_df1$DM == "DMwoDN" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="No DM" & GCKD_df1$BL_ku_bmi <= 30 & GCKD_df1$BL_ku_bmi > 25, na.rm=TRUE), sum(GCKD_df1$DM == "No DM" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$DM, GCKD_df1$BL_ku_bmi <= 25,  useNA = "always")
BinomCI(x=sum(GCKD_df1$DM =="DMwDN" & GCKD_df1$BL_ku_bmi <= 25, na.rm=TRUE), n=sum(GCKD_df1$DM == "DMwDN" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="DMwoDN" & GCKD_df1$BL_ku_bmi <= 25, na.rm=TRUE), sum(GCKD_df1$DM == "DMwoDN" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="No DM" & GCKD_df1$BL_ku_bmi <= 25, na.rm=TRUE), sum(GCKD_df1$DM == "No DM" & !is.na(GCKD_df1$BL_ku_bmi)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$DM, GCKD_df1$BL_ku_sys < 130 & GCKD_df1$BL_ku_dia < 80,  useNA = "always")
BinomCI(x=sum(GCKD_df1$DM =="DMwDN" & GCKD_df1$BL_ku_sys < 130 & GCKD_df1$BL_ku_dia < 80, na.rm=TRUE), n=sum(GCKD_df1$DM == "DMwDN" & !is.na(GCKD_df1$BL_ku_sys)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="DMwoDN" & GCKD_df1$BL_ku_sys < 130 & GCKD_df1$BL_ku_dia < 80, na.rm=TRUE), n=sum(GCKD_df1$DM == "DMwoDN" & !is.na(GCKD_df1$BL_ku_sys)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="No DM" & GCKD_df1$BL_ku_sys < 130 & GCKD_df1$BL_ku_dia < 80, na.rm=TRUE), sum(GCKD_df1$DM == "No DM" & !is.na(GCKD_df1$BL_ku_sys)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$DM, GCKD_df1$BL_ku_sys < 140 & GCKD_df1$BL_ku_dia < 90,  useNA = "always")
BinomCI(x=sum(GCKD_df1$DM =="DMwDN" & GCKD_df1$BL_ku_sys < 140 & GCKD_df1$BL_ku_dia < 90, na.rm=TRUE), n=sum(GCKD_df1$DM == "DMwDN" & !is.na(GCKD_df1$BL_ku_sys)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="DMwoDN" & GCKD_df1$BL_ku_sys < 140 & GCKD_df1$BL_ku_dia < 90, na.rm=TRUE), n=sum(GCKD_df1$DM == "DMwoDN" & !is.na(GCKD_df1$BL_ku_sys)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="No DM" & GCKD_df1$BL_ku_sys < 140 & GCKD_df1$BL_ku_dia < 90, na.rm=TRUE), sum(GCKD_df1$DM == "No DM" & !is.na(GCKD_df1$BL_ku_sys)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$DM, GCKD_df1$BL_gfr_mdrd >= 60,  useNA = "always")
BinomCI(x=sum(GCKD_df1$DM =="DMwDN" & GCKD_df1$BL_gfr_mdrd >= 60, na.rm=TRUE), n=sum(GCKD_df1$DM =="DMwDN" & !is.na(GCKD_df1$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="DMwoDN" & GCKD_df1$BL_gfr_mdrd >= 60, na.rm=TRUE), n=sum(GCKD_df1$DM =="DMwoDN" & !is.na(GCKD_df1$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="No DM" & GCKD_df1$BL_gfr_mdrd >= 60, na.rm=TRUE), sum(GCKD_df1$DM == "No DM" & !is.na(GCKD_df1$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$DM, GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60,  useNA = "always")
BinomCI(x=sum(GCKD_df1$DM =="DMwDN" & GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60, na.rm=TRUE), n=sum(GCKD_df1$DM =="DMwDN" & !is.na(GCKD_df1$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="DMwoDN" & GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60, na.rm=TRUE), n=sum(GCKD_df1$DM =="DMwoDN" & !is.na(GCKD_df1$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="No DM" & GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60, na.rm=TRUE), sum(GCKD_df1$DM == "No DM" & !is.na(GCKD_df1$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$DM, GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45,  useNA = "always")
BinomCI(x=sum(GCKD_df1$DM =="DMwDN" & GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45, na.rm=TRUE), n=sum(GCKD_df1$DM =="DMwDN" & !is.na(GCKD_df1$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="DMwoDN" & GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45, na.rm=TRUE), n=sum(GCKD_df1$DM =="DMwoDN" & !is.na(GCKD_df1$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="No DM" & GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45, na.rm=TRUE), sum(GCKD_df1$DM == "No DM" & !is.na(GCKD_df1$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$DM, GCKD_df1$BL_gfr_mdrd < 30,  useNA = "always")
BinomCI(x=sum(GCKD_df1$DM =="DMwDN" & GCKD_df1$BL_gfr_mdrd < 30, na.rm=TRUE), n=sum(GCKD_df1$DM =="DMwDN" & !is.na(GCKD_df1$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="DMwoDN" & GCKD_df1$BL_gfr_mdrd < 30, na.rm=TRUE), n=sum(GCKD_df1$DM =="DMwoDN" & !is.na(GCKD_df1$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="No DM" & GCKD_df1$BL_gfr_mdrd < 30, na.rm=TRUE), sum(GCKD_df1$DM == "No DM" & !is.na(GCKD_df1$BL_gfr_mdrd)), method="wilson") %>% '*'(100) %>% round(1)
GCKD_df1 %>% group_by(DM) %>%
  summarise_at(vars(BL_uacr), list(~ round(quantile(., na.rm=TRUE), 1)))
GCKD_df1_dmwdn <- GCKD_df1 %>% subset(DM == "DMwDN")
GCKD_df1_dmwodn <- GCKD_df1 %>% subset(DM == "DMwoDN")
GCKD_df1_nodm <- GCKD_df1 %>% subset(DM == "No DM")
MedianCI(GCKD_df1_dmwdn$BL_uacr, na.rm=TRUE)
MedianCI(GCKD_df1_dmwodn$BL_uacr, na.rm=TRUE)
MedianCI(GCKD_df1_nodm$BL_uacr, na.rm=TRUE)
table(GCKD_df1$DM, GCKD_df1$BL_uacr < 30,  useNA = "always")
BinomCI(x=sum(GCKD_df1$DM =="DMwDN" & GCKD_df1$BL_uacr < 30, na.rm=TRUE), n=sum(GCKD_df1$DM =="DMwDN" & !is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="DMwoDN" & GCKD_df1$BL_uacr < 30, na.rm=TRUE), n=sum(GCKD_df1$DM =="DMwoDN" & !is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="No DM" & GCKD_df1$BL_uacr < 30, na.rm=TRUE), n=sum(GCKD_df1$DM =="No DM" & !is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$DM, GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300,  useNA = "always")
BinomCI(x=sum(GCKD_df1$DM =="DMwDN" & GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300, na.rm=TRUE), n=sum(GCKD_df1$DM =="DMwDN" & !is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="DMwoDN" & GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300, na.rm=TRUE), n=sum(GCKD_df1$DM =="DMwoDN" & !is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="No DM" & GCKD_df1$BL_uacr < 30, na.rm=TRUE), n=sum(GCKD_df1$DM =="No DM" & !is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
table(GCKD_df1$DM, GCKD_df1$BL_uacr > 300,  useNA = "always")
BinomCI(x=sum(GCKD_df1$DM =="DMwDN" & GCKD_df1$BL_uacr > 300, na.rm=TRUE), n=sum(GCKD_df1$DM =="DMwDN" & !is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="DMwoDN" & GCKD_df1$BL_uacr > 300, na.rm=TRUE), n=sum(GCKD_df1$DM =="DMwoDN" & !is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)
BinomCI(x=sum(GCKD_df1$DM =="No DM" & GCKD_df1$BL_uacr < 30, na.rm=TRUE), n=sum(GCKD_df1$DM =="No DM" & !is.na(GCKD_df1$BL_uacr)), method="wilson") %>% '*'(100) %>% round(1)

## Suppl. Figure 1: Age distribution stratified
GCKD_df1_FigS1 <- GCKD_df1
GCKD_df1_FigS1 <- GCKD_df1_FigS1 %>% mutate(
  BL_age_cat = ifelse(GCKD_df1_FigS1$BL_age < 20, 1, ifelse(GCKD_df1_FigS1$BL_age >= 20 & GCKD_df1_FigS1$BL_age < 30, 2, ifelse(
    GCKD_df1_FigS1$BL_age >= 30 & GCKD_df1_FigS1$BL_age < 40, 3, ifelse(GCKD_df1_FigS1$BL_age >= 40 & GCKD_df1_FigS1$BL_age < 50, 4, ifelse(
      GCKD_df1_FigS1$BL_age >= 50 & GCKD_df1_FigS1$BL_age < 60, 5, ifelse(GCKD_df1_FigS1$BL_age >= 60 & GCKD_df1_FigS1$BL_age < 70, 6, 7)))))))
GCKD_df1_FigS1 %>%
  filter(dem_sex == "Male") %>%
  ggplot(aes(x=as.factor(BL_age_cat))) +
  geom_bar(color="white", fill="azure4", width=1.0)  +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
  coord_flip() +
  theme(aspect.ratio = 3/2)
GCKD_df1_FigS1 %>%
  filter(dem_sex == "Female") %>%
  ggplot(aes(x=as.factor(BL_age_cat))) +
  geom_bar(color="white", fill="azure4", alpha = 0.5, width=1.0) +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
  coord_flip() +
  theme(aspect.ratio = 3/2)
GCKD_df1_FigS1 %>%
  filter(diabetes == 1) %>%
  ggplot(aes(x=as.factor(BL_age_cat))) +
  geom_bar(color="white", fill="azure4", width=1.0)  +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
  coord_flip() +
  theme(aspect.ratio = 3/2)
GCKD_df1_FigS1 %>%
  filter(diabetes == 2) %>%
  ggplot(aes(x=as.factor(BL_age_cat))) +
  geom_bar(color="white", fill="azure4", alpha = 0.5, width=1.0) +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
  coord_flip() +
  theme(aspect.ratio = 3/2)
