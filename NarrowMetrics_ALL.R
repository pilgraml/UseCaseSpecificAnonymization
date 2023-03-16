#!/usr/bin/env Rscript --vanilla

# name: NarrowMetrics_ALL.R
#
# input: AnonymizedDataset
# output: tbl1

pacman::p_load(tidyr, stringr, dplyr, openxlsx, naniar, emmeans, multcomp, 
               plyr, tibble, lmtest, sandwich,
               tidyselect, scales, gridExtra, 
               lubridate, gtsummary, boot, patchwork, rms, DescTools, PropCIs)

CI_overlap <- function(Lor, Uor, Lan, Uan){
  if(is.na(Uor) == TRUE | is.na(Uan) == TRUE | is.na(Lor) == TRUE | is.na(Lan) == TRUE){
    CIo <- NA
  }
  else if(Uor < Lan | Uan < Lor){
    CIo <- 0
  }
  else if(Uor <= Uan & Lor >= Lan){
    CIo <- 0.5*((Uor-Lor)/(Uor-Lor)+(Uor-Lor)/(Uan-Lan))
  }
  else if(Uan <= Uor & Lan >= Lor){
    CIo <- 0.5*((Uan-Lan)/(Uor-Lor)+(Uan-Lan)/(Uan-Lan))
  }
  else if(Uan <= Uor & Lor >= Lan){
    CIo <- 0.5*((Uan-Lor)/(Uor-Lor)+(Uan-Lor)/(Uan-Lan))
  }
  else if(Uor <= Uan & Lan >= Lor){
    CIo <- 0.5*((Uor-Lan)/(Uor-Lor)+(Uor-Lan)/(Uan-Lan))
  }
  else (CIo <- NA)
  CIo <- round(CIo*100,1)
  print(CIo)
}

args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]
output_file_tbl1 <- args[2]
output_file_tbl1_cio <- args[3]
output_file_tbl2 <- args[4]
output_file_tbl2_cio <- args[5]
output_file_tbl3 <- args[6]
output_file_tbl3_cio <- args[7]
output_file_tbl4 <- args[8]
output_file_tbl4_cio <- args[9]
output_file_tbls3 <- args[10]
output_file_tbls3_cio <- args[11]
output_file_avg_cio <- args[12]

GCKD_df1 <- as_tibble(read.csv(input_file, sep = ";"))
GCKD_df1 <- GCKD_df1 %>% na_if("*")
GCKD_df1 <- GCKD_df1 %>% na_if("NULL")
GCKD_df1 <- GCKD_df1 %>% na_if("null")
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
             "pavk_surgery", "incl_egfr", "education", "BL_age")
GCKD_df1 <- GCKD_df1 %>% mutate(across(all_of(col_cat), as.character))
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


# table 1
print("tbl1")
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
GCKD_df1_table_1 <- as_tibble(GCKD_df1_table_1)

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
GCKD_df1_fem_table_1 <- as_tibble(GCKD_df1_fem_table_1)
GCKD_df1_fem_table_1$CI_female_d[GCKD_df1_fem_table_1$Variable == "BL_uacr"] <- UACR_CI_fem_d
GCKD_df1_fem_table_1$CI_female_nd[GCKD_df1_fem_table_1$Variable == "BL_uacr"] <- UACR_CI_fem_nd

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
GCKD_df1_male_table_1 <- as_tibble(GCKD_df1_male_table_1)
GCKD_df1_male_table_1$CI_male_d[GCKD_df1_male_table_1$Variable == "BL_uacr"] <- UACR_CI_male_d
GCKD_df1_male_table_1$CI_male_nd[GCKD_df1_male_table_1$Variable == "BL_uacr"] <- UACR_CI_male_nd

tbl1_female_male_anonym <- full_join(x = GCKD_df1_male_table_1, y = GCKD_df1_fem_table_1, by = "Variable")
tbl1_total_female_male_anonym <- full_join(x = tbl1_female_male_anonym, y = GCKD_df1_table_1, by = "Variable")
tbl1_anonym <- subset(tbl1_total_female_male_anonym, !(endsWith(tbl1_total_female_male_anonym$Variable, "_no")) & 
                        tbl1_total_female_male_anonym$Variable != "(Missing)" &
                        tbl1_total_female_male_anonym$Variable != "Female" &
                        (!is.na(tbl1_total_female_male_anonym$n_mean_female_d) | !is.na(tbl1_total_female_male_anonym$n_mean_male_d)))
write.xlsx(tbl1_anonym, output_file_tbl1)

print("tbl1 95%CI overlap")
tbl1_origin <- as_tibble(read.xlsx("tbl1_origin.xlsx"))
tbl1_origin <- subset(tbl1_origin,  
                      tbl1_origin$Variable != "BL_age" &
                        tbl1_origin$Variable != "BL_ku_height_cm" &
                        tbl1_origin$Variable != "BL_ku_weight" &
                        tbl1_origin$Variable != "BL_ku_bmi"&
                        tbl1_origin$Variable != ">30"&
                        tbl1_origin$Variable != "25.1-29.9"&
                        tbl1_origin$Variable != "<=25"&
                        tbl1_origin$Variable != "Male")
tbl1_anonym <- subset(tbl1_anonym,  
                      tbl1_anonym$Variable != "Male")
tbl1_origin[c("CI_total_low", "CI_total_up")] <- str_split_fixed(tbl1_origin$CI_total, "-", 2)
tbl1_origin$CI_total_low <- as.numeric(tbl1_origin$CI_total_low)
tbl1_origin$CI_total_up <- as.numeric(tbl1_origin$CI_total_up)
tbl1_anonym[c("CI_total_low", "CI_total_up")] <- str_split_fixed(tbl1_anonym$CI_total, "-", 2)
tbl1_anonym$CI_total_low <- as.numeric(tbl1_anonym$CI_total_low)
tbl1_anonym$CI_total_up <- as.numeric(tbl1_anonym$CI_total_up)
tbl1_origin[c("CI_male_d_low", "CI_male_d_up")] <- str_split_fixed(tbl1_origin$CI_male_d, "-", 2)
tbl1_origin$CI_male_d_low <- as.numeric(tbl1_origin$CI_male_d_low)
tbl1_origin$CI_male_d_up <- as.numeric(tbl1_origin$CI_male_d_up)
tbl1_anonym[c("CI_male_d_low", "CI_male_d_up")] <- str_split_fixed(tbl1_anonym$CI_male_d, "-", 2)
tbl1_anonym$CI_male_d_low <- as.numeric(tbl1_anonym$CI_male_d_low)
tbl1_anonym$CI_male_d_up <- as.numeric(tbl1_anonym$CI_male_d_up)
tbl1_origin[c("CI_male_nd_low", "CI_male_nd_up")] <- str_split_fixed(tbl1_origin$CI_male_nd, "-", 2)
tbl1_origin$CI_male_nd_low <- as.numeric(tbl1_origin$CI_male_nd_low)
tbl1_origin$CI_male_nd_up <- as.numeric(tbl1_origin$CI_male_nd_up)
tbl1_anonym[c("CI_male_nd_low", "CI_male_nd_up")] <- str_split_fixed(tbl1_anonym$CI_male_nd, "-", 2)
tbl1_anonym$CI_male_nd_low <- as.numeric(tbl1_anonym$CI_male_nd_low)
tbl1_anonym$CI_male_nd_up <- as.numeric(tbl1_anonym$CI_male_nd_up)
tbl1_origin[c("CI_female_d_low", "CI_female_d_up")] <- str_split_fixed(tbl1_origin$CI_female_d, "-", 2)
tbl1_origin$CI_female_d_low <- as.numeric(tbl1_origin$CI_female_d_low)
tbl1_origin$CI_female_d_up <- as.numeric(tbl1_origin$CI_female_d_up)
tbl1_anonym[c("CI_female_d_low", "CI_female_d_up")] <- str_split_fixed(tbl1_anonym$CI_female_d, "-", 2)
tbl1_anonym$CI_female_d_low <- as.numeric(tbl1_anonym$CI_female_d_low)
tbl1_anonym$CI_female_d_up <- as.numeric(tbl1_anonym$CI_female_d_up)
tbl1_origin[c("CI_female_nd_low", "CI_female_nd_up")] <- str_split_fixed(tbl1_origin$CI_female_nd, "-", 2)
tbl1_origin$CI_female_nd_low <- as.numeric(tbl1_origin$CI_female_nd_low)
tbl1_origin$CI_female_nd_up <- as.numeric(tbl1_origin$CI_female_nd_up)
tbl1_anonym[c("CI_female_nd_low", "CI_female_nd_up")] <- str_split_fixed(tbl1_anonym$CI_female_nd, "-", 2)
tbl1_anonym$CI_female_nd_low <- as.numeric(tbl1_anonym$CI_female_nd_low)
tbl1_anonym$CI_female_nd_up <- as.numeric(tbl1_anonym$CI_female_nd_up)
tbl1_origin <- tbl1_origin %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym <- tbl1_anonym %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_ci <- data.frame(NA_col = rep(NA, 40))
res_ci_male_d = numeric(40)
res_ci_male_nd = numeric(40)
res_ci_female_d = numeric(40)
res_ci_female_nd = numeric(40)
res_ci = numeric(40)
for(i in 1:nrow((tbl1_anonym))) {
  var = tbl1_origin$Variable[i]
  res_ci_male_d[i] <- CI_overlap(tbl1_origin$CI_male_d_low[i], tbl1_origin$CI_male_d_up[i], tbl1_anonym$CI_male_d_low[i], tbl1_anonym$CI_male_d_up[i])
  tbl1_ci[, 1] <- res_ci_male_d
  res_ci_male_nd[i] <- CI_overlap(tbl1_origin$CI_male_nd_low[i], tbl1_origin$CI_male_nd_up[i], tbl1_anonym$CI_male_nd_low[i], tbl1_anonym$CI_male_nd_up[i])
  tbl1_ci[, 2] <- res_ci_male_nd
  res_ci_female_d[i] <- CI_overlap(tbl1_origin$CI_female_d_low[i], tbl1_origin$CI_female_d_up[i], tbl1_anonym$CI_female_d_low[i], tbl1_anonym$CI_female_d_up[i])
  tbl1_ci[, 3] <- res_ci_female_d
  res_ci_female_nd[i] <- CI_overlap(tbl1_origin$CI_female_nd_low[i], tbl1_origin$CI_female_nd_up[i], tbl1_anonym$CI_female_nd_low[i], tbl1_anonym$CI_female_nd_up[i])
  tbl1_ci[, 4] <- res_ci_female_nd
  res_ci[i] <- CI_overlap(tbl1_origin$CI_total_low[i], tbl1_origin$CI_total_up[i], tbl1_anonym$CI_total_low[i], tbl1_anonym$CI_total_up[i]) 
  tbl1_ci[, 5] <- res_ci
  rownames(tbl1_ci)[i] <- paste0(var)
  colnames(tbl1_ci)[1] <- paste0("CI_overlap_male_d")
  colnames(tbl1_ci)[2] <- paste0("CI_overlap_male_nd")
  colnames(tbl1_ci)[3] <- paste0("CI_overlap_female_d")
  colnames(tbl1_ci)[4] <- paste0("CI_overlap_female_nd")
  colnames(tbl1_ci)[5] <- paste0("CI_overlap_total")
}

tbl1_ci$Variable <- row.names(tbl1_ci)
tbl1_ci <- tbl1_ci %>% replace(is.na(.), 100.0)
tbl1_ci$CI_overlap_avg <-apply(tbl1_ci[sapply(tbl1_ci, is.numeric)],1,mean)
tbl1_ci <- tbl1_ci %>% mutate(across(where(is.numeric), ~round(., 1)))
write.xlsx(tbl1_ci, output_file_tbl1_cio)

# table 2
print("tbl2")
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
GCKD_df1_table_2 <- as_tibble(GCKD_df1_table_2)
tbl2_anonym <- subset(GCKD_df1_table_2, !(endsWith(GCKD_df1_table_2$Variable, "_no")) & 
                        GCKD_df1_table_2$Variable != "(Missing)" &
                        GCKD_df1_table_2$Variable != "Female" &
                        (!is.na(GCKD_df1_table_2$n_mean_egfr) | !is.na(GCKD_df1_table_2$n_mean_prot)))
write.xlsx(tbl2_anonym, output_file_tbl2)

print("tbl2 95%CI overlap")
tbl2_total_origin <- as_tibble(read.xlsx("tbl2_origin.xlsx"))
tbl2_total_origin <- subset(tbl2_total_origin, tbl2_total_origin$Variable != "BL_age" &
                              tbl2_total_origin$Variable != "cardiovasc_less60_yes" &
                              tbl2_total_origin$Variable != "cardiovasc_more60_yes")
tbl2_origin <- subset(tbl2_total_origin, tbl2_total_origin$Variable == "Male" |
                        tbl2_total_origin$Variable == "biopsy_yes")
tbl2_origin[c("CI_egfr_low", "CI_egfr_up")] <- str_split_fixed(tbl2_origin$CI_egfr, "-", 2)
tbl2_origin$CI_egfr_low <- as.numeric(tbl2_origin$CI_egfr_low)
tbl2_origin$CI_egfr_up <- as.numeric(tbl2_origin$CI_egfr_up)
tbl2_anonym[c("CI_egfr_low", "CI_egfr_up")] <- str_split_fixed(tbl2_anonym$CI_egfr, "-", 2)
tbl2_anonym$CI_egfr_low <- as.numeric(tbl2_anonym$CI_egfr_low)
tbl2_anonym$CI_egfr_up <- as.numeric(tbl2_anonym$CI_egfr_up)
tbl2_origin[c("CI_prot_low", "CI_prot_up")] <- str_split_fixed(tbl2_origin$CI_prot, "-", 2)
tbl2_origin$CI_prot_low <- as.numeric(tbl2_origin$CI_prot_low)
tbl2_origin$CI_prot_up <- as.numeric(tbl2_origin$CI_prot_up)
tbl2_anonym[c("CI_prot_low", "CI_prot_up")] <- str_split_fixed(tbl2_anonym$CI_prot, "-", 2)
tbl2_anonym$CI_prot_low <- as.numeric(tbl2_anonym$CI_prot_low)
tbl2_anonym$CI_prot_up <- as.numeric(tbl2_anonym$CI_prot_up)
tbl2_origin <- tbl2_origin %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl2_anonym <- tbl2_anonym %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl2_ci <- data.frame(NA_col = rep(NA, 2))
res_ci_egfr = numeric(2)
res_ci_prot = numeric(2)
for(i in 1:nrow((tbl2_anonym))) {
  var = tbl2_origin$Variable[i]
  res_ci_egfr[i] <- CI_overlap(tbl2_origin$CI_egfr_low[i], tbl2_origin$CI_egfr_up[i], tbl2_anonym$CI_egfr_low[i], tbl2_anonym$CI_egfr_up[i])
  tbl2_ci[, 1] <- res_ci_egfr
  res_ci_prot[i] <- CI_overlap(tbl2_origin$CI_prot_low[i], tbl2_origin$CI_prot_up[i], tbl2_anonym$CI_prot_low[i], tbl2_anonym$CI_prot_up[i])
  tbl2_ci[, 2] <- res_ci_prot
  rownames(tbl2_ci)[i] <- paste0(var)
  colnames(tbl2_ci)[1] <- paste0("CI_overlap_egfr")
  colnames(tbl2_ci)[2] <- paste0("CI_overlap_prot")
}
tbl2_ci$Variable <- row.names(tbl2_ci)
tbl2_ci <- full_join(x = tbl2_total_origin, y = tbl2_ci, by = "Variable")
tbl2_ci <- tbl2_ci %>% replace(is.na(.), 100.0)
tbl2_ci <- subset(tbl2_ci, select = c(CI_overlap_egfr, CI_overlap_prot, Variable))
tbl2_ci$CI_overlap_avg <-apply(tbl2_ci[sapply(tbl2_ci, is.numeric)],1,mean)
tbl2_ci <- tbl2_ci %>% mutate(across(where(is.numeric), ~round(., 1)))
write.xlsx(tbl2_ci, output_file_tbl2_cio)

# table 3
print("tbl3")
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
GCKD_df1_table_3b <- as_tibble(GCKD_df1_table_3b)
write.xlsx(GCKD_df1_table_3b, output_file_tbl3)

print("tbl3 95%CI overlap")
tbl3_total_origin <- as_tibble(read.xlsx("tbl3_origin.xlsx"))
tbl3_total_origin_t <- data.frame(t(tbl3_total_origin[-1]))
colnames(tbl3_total_origin_t) <- c("biopsy", "biopsy_no", "CI_biopsy", "(Missing)")
tbl3_total_origin_t$Variable <- row.names(tbl3_total_origin_t)
tbl3_origin <- subset(tbl3_total_origin_t, select = c("Variable", "CI_biopsy"), !(startsWith(tbl3_total_origin_t$Variable, "perc_")))
tbl3_total_anonym <- GCKD_df1_table_3b
tbl3_total_anonym_t <- data.frame(t(tbl3_total_anonym[-1]))
colnames(tbl3_total_anonym_t) <- c("biopsy", "biopsy_no", "CI_biopsy", "(Missing)")
tbl3_total_anonym_t$Variable <- row.names(tbl3_total_anonym_t)
tbl3_anonym <- subset(tbl3_total_anonym_t, select = c("Variable", "CI_biopsy"), !(startsWith(tbl3_total_anonym_t$Variable, "perc_")))
tbl3_origin[c("CI_biopsy_low", "CI_biopsy_up")] <- str_split_fixed(tbl3_origin$CI_biopsy, "-", 2)
tbl3_origin$CI_biopsy_low <- as.numeric(tbl3_origin$CI_biopsy_low)
tbl3_origin$CI_biopsy_up <- as.numeric(tbl3_origin$CI_biopsy_up)
tbl3_anonym[c("CI_biopsy_low", "CI_biopsy_up")] <- str_split_fixed(tbl3_anonym$CI_biopsy, "-", 2)
tbl3_anonym$CI_biopsy_low <- as.numeric(tbl3_anonym$CI_biopsy_low)
tbl3_anonym$CI_biopsy_up <- as.numeric(tbl3_anonym$CI_biopsy_up)
tbl3_origin <- tbl3_origin %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl3_anonym <- tbl3_anonym %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl3_ci <- data.frame(NA_col = rep(NA, 11))
res_CI_biopsy = numeric(11)
for(i in 1:nrow((tbl3_anonym))) {
  var = tbl3_origin$Variable[i]
  res_CI_biopsy[i] <- CI_overlap(tbl3_origin$CI_biopsy_low[i], tbl3_origin$CI_biopsy_up[i], tbl3_anonym$CI_biopsy_low[i], tbl3_anonym$CI_biopsy_up[i])
  tbl3_ci[, 1] <- res_CI_biopsy
  rownames(tbl3_ci)[i] <- paste0(var)
  colnames(tbl3_ci)[1] <- paste0("CI_overlap_biopsy")
}
tbl3_ci$Variable <- row.names(tbl3_ci)
tbl3_ci$CI_overlap_ckd_diab <- NA
tbl3_ci$CI_overlap_ckd_oth <- NA
tbl3_ci$CI_overlap_ckd_lead_uk <- NA
tbl3_ci$CI_overlap_ckd_vask <- NA
tbl3_ci$CI_overlap_ckd_syst <- NA
tbl3_ci$CI_overlap_ckd_glom_prim <- NA
tbl3_ci$CI_overlap_ckd_interst <- NA
tbl3_ci$CI_overlap_ckd_aki <- NA
tbl3_ci$CI_overlap_ckd_single <- NA
tbl3_ci$CI_overlap_ckd_heredit <- NA
tbl3_ci$CI_overlap_ckd_obstr <- NA
tbl3_ci <- tbl3_ci %>% replace(is.na(.), 100.0)
tbl3_ci$CI_overlap_avg <-apply(tbl3_ci[sapply(tbl3_ci, is.numeric)],1,mean)
tbl3_ci <- tbl3_ci %>% mutate(across(where(is.numeric), ~round(., 1)))
write.xlsx(tbl3_ci, output_file_tbl3_cio)

# table 4
print("tbl4")
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
GCKD_df1_fem_table_4 <- as_tibble(GCKD_df1_fem_table_4)

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
GCKD_df1_male_table_4 <- as_tibble(GCKD_df1_male_table_4)

tbl4_female_male <- full_join(x = GCKD_df1_male_table_4, y = GCKD_df1_fem_table_4, by = "Variable")
tbl4_anonym <- subset(tbl4_female_male, !(endsWith(tbl4_female_male$Variable, "_no")) & 
                        tbl4_female_male$Variable != "(Missing)" &
                        tbl4_female_male$Variable != "Female" &
                        tbl4_female_male$Variable != "Male" &
                        (!is.na(tbl4_female_male$n_female_d) | !is.na(tbl4_female_male$n_male_d)))
write.xlsx(tbl4_anonym, output_file_tbl4)

print("tbl4 95%CI overlap")
tbl4_total_origin <- as_tibble(read.xlsx("tbl4_origin.xlsx"))
tbl4_origin <- subset(tbl4_total_origin, tbl4_total_origin$Variable != "cardiovasc_less60_yes" &
                        tbl4_total_origin$Variable != "cardiovasc_more60_yes")
tbl4_origin[c("CI_male_d_low", "CI_male_d_up")] <- str_split_fixed(tbl4_origin$CI_male_d, "-", 2)
tbl4_origin$CI_male_d_low <- as.numeric(tbl4_origin$CI_male_d_low)
tbl4_origin$CI_male_d_up <- as.numeric(tbl4_origin$CI_male_d_up)
tbl4_anonym[c("CI_male_d_low", "CI_male_d_up")] <- str_split_fixed(tbl4_anonym$CI_male_d, "-", 2)
tbl4_anonym$CI_male_d_low <- as.numeric(tbl4_anonym$CI_male_d_low)
tbl4_anonym$CI_male_d_up <- as.numeric(tbl4_anonym$CI_male_d_up)
tbl4_origin[c("CI_male_nd_low", "CI_male_nd_up")] <- str_split_fixed(tbl4_origin$CI_male_nd, "-", 2)
tbl4_origin$CI_male_nd_low <- as.numeric(tbl4_origin$CI_male_nd_low)
tbl4_origin$CI_male_nd_up <- as.numeric(tbl4_origin$CI_male_nd_up)
tbl4_anonym[c("CI_male_nd_low", "CI_male_nd_up")] <- str_split_fixed(tbl4_anonym$CI_male_nd, "-", 2)
tbl4_anonym$CI_male_nd_low <- as.numeric(tbl4_anonym$CI_male_nd_low)
tbl4_anonym$CI_male_nd_up <- as.numeric(tbl4_anonym$CI_male_nd_up)
tbl4_origin[c("CI_female_d_low", "CI_female_d_up")] <- str_split_fixed(tbl4_origin$CI_female_d, "-", 2)
tbl4_origin$CI_female_d_low <- as.numeric(tbl4_origin$CI_female_d_low)
tbl4_origin$CI_female_d_up <- as.numeric(tbl4_origin$CI_female_d_up)
tbl4_anonym[c("CI_female_d_low", "CI_female_d_up")] <- str_split_fixed(tbl4_anonym$CI_female_d, "-", 2)
tbl4_anonym$CI_female_d_low <- as.numeric(tbl4_anonym$CI_female_d_low)
tbl4_anonym$CI_female_d_up <- as.numeric(tbl4_anonym$CI_female_d_up)
tbl4_origin[c("CI_female_nd_low", "CI_female_nd_up")] <- str_split_fixed(tbl4_origin$CI_female_nd, "-", 2)
tbl4_origin$CI_female_nd_low <- as.numeric(tbl4_origin$CI_female_nd_low)
tbl4_origin$CI_female_nd_up <- as.numeric(tbl4_origin$CI_female_nd_up)
tbl4_anonym[c("CI_female_nd_low", "CI_female_nd_up")] <- str_split_fixed(tbl4_anonym$CI_female_nd, "-", 2)
tbl4_anonym$CI_female_nd_low <- as.numeric(tbl4_anonym$CI_female_nd_low)
tbl4_anonym$CI_female_nd_up <- as.numeric(tbl4_anonym$CI_female_nd_up)
tbl4_origin <- tbl4_origin %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl4_anonym <- tbl4_anonym %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl4_ci <- data.frame(NA_col = rep(NA, 16))
res_ci_male_d = numeric(16)
res_ci_male_nd = numeric(16)
res_ci_female_d = numeric(16)
res_ci_female_nd = numeric(16)
for(i in 1:nrow((tbl4_anonym))) {
  var = tbl4_origin$Variable[i]
  res_ci_male_d[i] <- CI_overlap(tbl4_origin$CI_male_d_low[i], tbl4_origin$CI_male_d_up[i], tbl4_anonym$CI_male_d_low[i], tbl4_anonym$CI_male_d_up[i])
  tbl4_ci[, 1] <- res_ci_male_d
  res_ci_male_nd[i] <- CI_overlap(tbl4_origin$CI_male_nd_low[i], tbl4_origin$CI_male_nd_up[i], tbl4_anonym$CI_male_nd_low[i], tbl4_anonym$CI_male_nd_up[i])
  tbl4_ci[, 2] <- res_ci_male_nd
  res_ci_female_d[i] <- CI_overlap(tbl4_origin$CI_female_d_low[i], tbl4_origin$CI_female_d_up[i], tbl4_anonym$CI_female_d_low[i], tbl4_anonym$CI_female_d_up[i])
  tbl4_ci[, 3] <- res_ci_female_d
  res_ci_female_nd[i] <- CI_overlap(tbl4_origin$CI_female_nd_low[i], tbl4_origin$CI_female_nd_up[i], tbl4_anonym$CI_female_nd_low[i], tbl4_anonym$CI_female_nd_up[i])
  tbl4_ci[, 4] <- res_ci_female_nd
  rownames(tbl4_ci)[i] <- paste0(var)
  colnames(tbl4_ci)[1] <- paste0("CI_overlap_male_d")
  colnames(tbl4_ci)[2] <- paste0("CI_overlap_male_nd")
  colnames(tbl4_ci)[3] <- paste0("CI_overlap_female_d")
  colnames(tbl4_ci)[4] <- paste0("CI_overlap_female_nd")
}
tbl4_ci$Variable <- row.names(tbl4_ci)
tbl4_ci$CI_overlap_total <- NA
tbl4_ci <- tbl4_ci %>% replace(is.na(.), 100.0)
tbl4_ci$CI_overlap_avg <-apply(tbl4_ci[sapply(tbl4_ci, is.numeric)],1,mean)
tbl4_ci <- tbl4_ci %>% mutate(across(where(is.numeric), ~round(., 1)))
write.xlsx(tbl4_ci, output_file_tbl4_cio)

# table s3
print("tbls3")
var_table_s3 <- c("biopsy", "DM")
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
GCKD_df1_table_s3 <- as_tibble(GCKD_df1_table_s3)
tbls3_anonym <- subset(GCKD_df1_table_s3, !(endsWith(GCKD_df1_table_s3$Variable, "_no")) & 
                         GCKD_df1_table_s3$Variable != "(Missing)" &
                         (!is.na(GCKD_df1_table_s3$n_mean_DMwDN) | !is.na(GCKD_df1_table_s3$n_mean_DMwoDN)))
write.xlsx(tbls3_anonym, output_file_tbls3)

print("tbls3 95%CI overlap")
tbls3_total_origin <- as_tibble(read.xlsx("tbls3_origin.xlsx"))
tbls3_total_origin <- subset(tbls3_total_origin, tbls3_total_origin$Variable != "BL_age" &
                               tbls3_total_origin$Variable != "BL_ku_bmi" &
                               tbls3_total_origin$Variable != ">30" &
                               tbls3_total_origin$Variable != "25.1-29.9" &
                               tbls3_total_origin$Variable != "<=25" &
                               tbls3_total_origin$Variable != "BL_ku_height_cm" &
                               tbls3_total_origin$Variable != "BL_ku_weight")
tbls3_origin <- subset(tbls3_total_origin, tbls3_total_origin$Variable == "biopsy_yes")
tbls3_origin[c("CI_DMwDN_low", "CI_DMwDN_up")] <- str_split_fixed(tbls3_origin$CI_DMwDN, "-", 2)
tbls3_origin$CI_DMwDN_low <- as.numeric(tbls3_origin$CI_DMwDN_low)
tbls3_origin$CI_DMwDN_up <- as.numeric(tbls3_origin$CI_DMwDN_up)
tbls3_anonym[c("CI_DMwDN_low", "CI_DMwDN_up")] <- str_split_fixed(tbls3_anonym$CI_DMwDN, "-", 2)
tbls3_anonym$CI_DMwDN_low <- as.numeric(tbls3_anonym$CI_DMwDN_low)
tbls3_anonym$CI_DMwDN_up <- as.numeric(tbls3_anonym$CI_DMwDN_up)
tbls3_origin[c("CI_DMwoDN_low", "CI_DMwoDN_up")] <- str_split_fixed(tbls3_origin$CI_DMwoDN, "-", 2)
tbls3_origin$CI_DMwoDN_low <- as.numeric(tbls3_origin$CI_DMwoDN_low)
tbls3_origin$CI_DMwoDN_up <- as.numeric(tbls3_origin$CI_DMwoDN_up)
tbls3_anonym[c("CI_DMwoDN_low", "CI_DMwoDN_up")] <- str_split_fixed(tbls3_anonym$CI_DMwoDN, "-", 2)
tbls3_anonym$CI_DMwoDN_low <- as.numeric(tbls3_anonym$CI_DMwoDN_low)
tbls3_anonym$CI_DMwoDN_up <- as.numeric(tbls3_anonym$CI_DMwoDN_up)
tbls3_origin[c("CI_NoDM_low", "CI_NoDM_up")] <- str_split_fixed(tbls3_origin$CI_NoDM, "-", 2)
tbls3_origin$CI_NoDM_low <- as.numeric(tbls3_origin$CI_NoDM_low)
tbls3_origin$CI_NoDM_up <- as.numeric(tbls3_origin$CI_NoDM_up)
tbls3_anonym[c("CI_NoDM_low", "CI_NoDM_up")] <- str_split_fixed(tbls3_anonym$CI_NoDM, "-", 2)
tbls3_anonym$CI_NoDM_low <- as.numeric(tbls3_anonym$CI_NoDM_low)
tbls3_anonym$CI_NoDM_up <- as.numeric(tbls3_anonym$CI_NoDM_up)
tbls3_origin <- tbls3_origin %>% mutate(across(where(is.numeric), ~round(., 1)))
tbls3_anonym <- tbls3_anonym %>% mutate(across(where(is.numeric), ~round(., 1)))
tbls3_ci <- data.frame(NA_col = rep(NA, 1))
res_ci_DMwoDN = numeric(1)
res_ci_NoDM = numeric(1)
res_ci = numeric(1)
for(i in 1:nrow((tbls3_anonym))) {
  var = tbls3_origin$Variable[i]
  res_ci_DMwoDN[i] <- CI_overlap(tbls3_origin$CI_DMwoDN_low[i], tbls3_origin$CI_DMwoDN_up[i], tbls3_anonym$CI_DMwoDN_low[i], tbls3_anonym$CI_DMwoDN_up[i])
  tbls3_ci[, 1] <- res_ci_DMwoDN
  res_ci_NoDM[i] <- CI_overlap(tbls3_origin$CI_NoDM_low[i], tbls3_origin$CI_NoDM_up[i], tbls3_anonym$CI_NoDM_low[i], tbls3_anonym$CI_NoDM_up[i])
  tbls3_ci[, 2] <- res_ci_NoDM
  res_ci[i] <- CI_overlap(tbls3_origin$CI_DMwDN_low[i], tbls3_origin$CI_DMwDN_up[i], tbls3_anonym$CI_DMwDN_low[i], tbls3_anonym$CI_DMwDN_up[i]) 
  tbls3_ci[, 3] <- res_ci
  rownames(tbls3_ci)[i] <- paste0(var)
  colnames(tbls3_ci)[1] <- paste0("CI_overlap_DMwoDN")
  colnames(tbls3_ci)[2] <- paste0("CI_overlap_NoDM")
  colnames(tbls3_ci)[3] <- paste0("CI_overlap_DMwDN")
}
tbls3_ci$Variable <- row.names(tbls3_ci)
tbls3_ci <- full_join(x = tbls3_total_origin, y = tbls3_ci, by = "Variable")
tbls3_ci <- tbls3_ci %>% replace(is.na(.), 100.0)
tbls3_ci <- subset(tbls3_ci, select = c(CI_overlap_DMwDN, CI_overlap_DMwoDN, CI_overlap_NoDM, Variable))
tbls3_ci$CI_overlap_avg <-apply(tbls3_ci[sapply(tbls3_ci, is.numeric)],1,mean)
tbls3_ci <- tbls3_ci %>% mutate(across(where(is.numeric), ~round(., 1)))
write.xlsx(tbls3_ci, output_file_tbls3_cio)

# average 95%CI overlap per table
print("95%CI overlap metrics")
avg_cio <- data.frame(tbl1_ci_avg = rep(NA, 1))
avg_cio$tbl1_ci_avg <- mean(tbl1_ci$CI_overlap_avg)
avg_cio$tbl2_ci_avg <- mean(tbl2_ci$CI_overlap_avg)
avg_cio$tbl3_ci_avg <- mean(tbl3_ci$CI_overlap_avg)
avg_cio$tbl4_ci_avg <- mean(tbl4_ci$CI_overlap_avg)
avg_cio$tbls3_ci_avg <- mean(tbls3_ci$CI_overlap_avg)
avg_cio$all_affected_ci_avg <- rowMeans(subset(avg_cio, select = c(tbl1_ci_avg, tbl2_ci_avg, tbl3_ci_avg, tbl4_ci_avg, tbls3_ci_avg)))
avg_cio <- avg_cio %>% mutate(fig2_ci_avg = 100.0)
avg_cio <- avg_cio %>% mutate(tbls2_ci_avg = 100.0)
avg_cio$all_ci_avg <- rowMeans(subset(avg_cio, select = c(tbl1_ci_avg, tbl2_ci_avg, tbl3_ci_avg, tbl4_ci_avg, tbls3_ci_avg, fig2_ci_avg, tbls2_ci_avg)))
avg_cio <- avg_cio %>% mutate(across(where(is.numeric), ~round(., 1)))
write.xlsx(avg_cio, output_file_avg_cio)