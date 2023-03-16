# Packages 
pacman::p_load(tidyr, stringr, dplyr, openxlsx, naniar, emmeans, multcomp, 
               plyr, finalfit, ggplot2, tibble, lmtest, sandwich,
               tidyverse, tidyselect, summarytools, scales, gridExtra, 
               lubridate, eeptools, gtsummary, flextable, boot, mosaic, 
               patchwork, rms, coxed, DescTools, PropCIs)

# Working directory
setwd(path_tbl)

# Datasets
tbl1_origin <- as_tibble(read.xlsx("tbl1_origin.xlsx", sep = ";"))
tbl1_anonym_GENERIC_strictaverage_2 <- as_tibble(read.xlsx("AnonymizedDataset_GENERIC_strictaverage_2.csv_tbl1.xlsx", sep = ";"))
tbl1_anonym_GENERIC_strictaverage_20 <- as_tibble(read.xlsx("AnonymizedDataset_GENERIC_strictaverage_20.csv_tbl1.xlsx", sep = ";"))
tbl1_anonym_GENERIC_strictaverage_33 <- as_tibble(read.xlsx("AnonymizedDataset_GENERIC_strictaverage_33.csv_tbl1.xlsx", sep = ";"))
tbl1_anonym_GENERIC_kanonymity_11 <- as_tibble(read.xlsx("AnonymizedDataset_GENERIC_kanonymity_11.csv_tbl1.xlsx", sep = ";"))
tbl1_anonym_GENERIC_kanonymity_20 <- as_tibble(read.xlsx("AnonymizedDataset_GENERIC_kanonymity_20.csv_tbl1.xlsx", sep = ";"))
tbl1_anonym_GENERIC_kanonymity_33 <- as_tibble(read.xlsx("AnonymizedDataset_GENERIC_kanonymity_33.csv_tbl1.xlsx", sep = ";"))
tbl1_anonym_USECASE_4QI_strictaverage_2 <- as_tibble(read.xlsx("AnonymizedDataset_USECASE_4QI_strictaverage_2.csv_tbl1.xlsx", sep = ";"))
tbl1_anonym_USECASE_4QI_strictaverage_20 <- as_tibble(read.xlsx("AnonymizedDataset_USECASE_4QI_strictaverage_20.csv_tbl1.xlsx", sep = ";"))
tbl1_anonym_USECASE_4QI_strictaverage_33 <- as_tibble(read.xlsx("AnonymizedDataset_USECASE_4QI_strictaverage_33.csv_tbl1.xlsx", sep = ";"))
tbl1_anonym_USECASE_4QI_kanonymity_11 <- as_tibble(read.xlsx("AnonymizedDataset_USECASE_4QI_kanonymity_11.csv_tbl1.xlsx", sep = ";"))
tbl1_anonym_USECASE_4QI_kanonymity_20 <- as_tibble(read.xlsx("AnonymizedDataset_USECASE_4QI_kanonymity_20.csv_tbl1.xlsx", sep = ";"))
tbl1_anonym_USECASE_4QI_kanonymity_33 <- as_tibble(read.xlsx("AnonymizedDataset_USECASE_4QI_kanonymity_33.csv_tbl1.xlsx", sep = ";"))

# Preprocessing
## Separating 95% CI bounds
tbl1_origin[c("CI_female_d_low", "CI_female_d_up")] <- str_split_fixed(tbl1_origin$CI_female_d, "-", 2)
tbl1_origin$CI_female_d_low <- as.numeric(tbl1_origin$CI_female_d_low)
tbl1_origin$CI_female_d_up <- as.numeric(tbl1_origin$CI_female_d_up)
tbl1_anonym_GENERIC_strictaverage_2[c("CI_female_d_low", "CI_female_d_up")] <- str_split_fixed(tbl1_anonym_GENERIC_strictaverage_2$CI_female_d, "-", 2)
tbl1_anonym_GENERIC_strictaverage_2$CI_female_d_low <- as.numeric(tbl1_anonym_GENERIC_strictaverage_2$CI_female_d_low)
tbl1_anonym_GENERIC_strictaverage_2$CI_female_d_up <- as.numeric(tbl1_anonym_GENERIC_strictaverage_2$CI_female_d_up)
tbl1_anonym_GENERIC_strictaverage_20[c("CI_female_d_low", "CI_female_d_up")] <- str_split_fixed(tbl1_anonym_GENERIC_strictaverage_20$CI_female_d, "-", 2)
tbl1_anonym_GENERIC_strictaverage_20$CI_female_d_low <- as.numeric(tbl1_anonym_GENERIC_strictaverage_20$CI_female_d_low)
tbl1_anonym_GENERIC_strictaverage_20$CI_female_d_up <- as.numeric(tbl1_anonym_GENERIC_strictaverage_20$CI_female_d_up)
tbl1_anonym_GENERIC_strictaverage_33[c("CI_female_d_low", "CI_female_d_up")] <- str_split_fixed(tbl1_anonym_GENERIC_strictaverage_33$CI_female_d, "-", 2)
tbl1_anonym_GENERIC_strictaverage_33$CI_female_d_low <- as.numeric(tbl1_anonym_GENERIC_strictaverage_33$CI_female_d_low)
tbl1_anonym_GENERIC_strictaverage_33$CI_female_d_up <- as.numeric(tbl1_anonym_GENERIC_strictaverage_33$CI_female_d_up)
tbl1_anonym_GENERIC_kanonymity_11[c("CI_female_d_low", "CI_female_d_up")] <- str_split_fixed(tbl1_anonym_GENERIC_kanonymity_11$CI_female_d, "-", 2)
tbl1_anonym_GENERIC_kanonymity_11$CI_female_d_low <- as.numeric(tbl1_anonym_GENERIC_kanonymity_11$CI_female_d_low)
tbl1_anonym_GENERIC_kanonymity_11$CI_female_d_up <- as.numeric(tbl1_anonym_GENERIC_kanonymity_11$CI_female_d_up)
tbl1_anonym_GENERIC_kanonymity_20[c("CI_female_d_low", "CI_female_d_up")] <- str_split_fixed(tbl1_anonym_GENERIC_kanonymity_20$CI_female_d, "-", 2)
tbl1_anonym_GENERIC_kanonymity_20$CI_female_d_low <- as.numeric(tbl1_anonym_GENERIC_kanonymity_20$CI_female_d_low)
tbl1_anonym_GENERIC_kanonymity_20$CI_female_d_up <- as.numeric(tbl1_anonym_GENERIC_kanonymity_20$CI_female_d_up)
tbl1_anonym_GENERIC_kanonymity_33[c("CI_female_d_low", "CI_female_d_up")] <- str_split_fixed(tbl1_anonym_GENERIC_kanonymity_33$CI_female_d, "-", 2)
tbl1_anonym_GENERIC_kanonymity_33$CI_female_d_low <- as.numeric(tbl1_anonym_GENERIC_kanonymity_33$CI_female_d_low)
tbl1_anonym_GENERIC_kanonymity_33$CI_female_d_up <- as.numeric(tbl1_anonym_GENERIC_kanonymity_33$CI_female_d_up)
tbl1_anonym_USECASE_4QI_strictaverage_2[c("CI_female_d_low", "CI_female_d_up")] <- str_split_fixed(tbl1_anonym_USECASE_4QI_strictaverage_2$CI_female_d, "-", 2)
tbl1_anonym_USECASE_4QI_strictaverage_2$CI_female_d_low <- as.numeric(tbl1_anonym_USECASE_4QI_strictaverage_2$CI_female_d_low)
tbl1_anonym_USECASE_4QI_strictaverage_2$CI_female_d_up <- as.numeric(tbl1_anonym_USECASE_4QI_strictaverage_2$CI_female_d_up)
tbl1_anonym_USECASE_4QI_strictaverage_20[c("CI_female_d_low", "CI_female_d_up")] <- str_split_fixed(tbl1_anonym_USECASE_4QI_strictaverage_20$CI_female_d, "-", 2)
tbl1_anonym_USECASE_4QI_strictaverage_20$CI_female_d_low <- as.numeric(tbl1_anonym_USECASE_4QI_strictaverage_20$CI_female_d_low)
tbl1_anonym_USECASE_4QI_strictaverage_20$CI_female_d_up <- as.numeric(tbl1_anonym_USECASE_4QI_strictaverage_20$CI_female_d_up)
tbl1_anonym_USECASE_4QI_strictaverage_33[c("CI_female_d_low", "CI_female_d_up")] <- str_split_fixed(tbl1_anonym_USECASE_4QI_strictaverage_33$CI_female_d, "-", 2)
tbl1_anonym_USECASE_4QI_strictaverage_33$CI_female_d_low <- as.numeric(tbl1_anonym_USECASE_4QI_strictaverage_33$CI_female_d_low)
tbl1_anonym_USECASE_4QI_strictaverage_33$CI_female_d_up <- as.numeric(tbl1_anonym_USECASE_4QI_strictaverage_33$CI_female_d_up)
tbl1_anonym_USECASE_4QI_kanonymity_11[c("CI_female_d_low", "CI_female_d_up")] <- str_split_fixed(tbl1_anonym_USECASE_4QI_kanonymity_11$CI_female_d, "-", 2)
tbl1_anonym_USECASE_4QI_kanonymity_11$CI_female_d_low <- as.numeric(tbl1_anonym_USECASE_4QI_kanonymity_11$CI_female_d_low)
tbl1_anonym_USECASE_4QI_kanonymity_11$CI_female_d_up <- as.numeric(tbl1_anonym_USECASE_4QI_kanonymity_11$CI_female_d_up)
tbl1_anonym_USECASE_4QI_kanonymity_20[c("CI_female_d_low", "CI_female_d_up")] <- str_split_fixed(tbl1_anonym_USECASE_4QI_kanonymity_20$CI_female_d, "-", 2)
tbl1_anonym_USECASE_4QI_kanonymity_20$CI_female_d_low <- as.numeric(tbl1_anonym_USECASE_4QI_kanonymity_20$CI_female_d_low)
tbl1_anonym_USECASE_4QI_kanonymity_20$CI_female_d_up <- as.numeric(tbl1_anonym_USECASE_4QI_kanonymity_20$CI_female_d_up)
tbl1_anonym_USECASE_4QI_kanonymity_33[c("CI_female_d_low", "CI_female_d_up")] <- str_split_fixed(tbl1_anonym_USECASE_4QI_kanonymity_33$CI_female_d, "-", 2)
tbl1_anonym_USECASE_4QI_kanonymity_33$CI_female_d_low <- as.numeric(tbl1_anonym_USECASE_4QI_kanonymity_33$CI_female_d_low)
tbl1_anonym_USECASE_4QI_kanonymity_33$CI_female_d_up <- as.numeric(tbl1_anonym_USECASE_4QI_kanonymity_33$CI_female_d_up)
## Converting relevant variables to numerical datatype: n & perc
col_num <- c("perc_female_d")
tbl1_origin <- tbl1_origin %>% mutate(across(all_of(col_num), as.numeric))
tbl1_anonym_GENERIC_strictaverage_2 <- tbl1_anonym_GENERIC_strictaverage_2 %>% mutate(across(all_of(col_num), as.numeric))
tbl1_anonym_GENERIC_strictaverage_20 <- tbl1_anonym_GENERIC_strictaverage_20 %>% mutate(across(all_of(col_num), as.numeric))
tbl1_anonym_GENERIC_strictaverage_33 <- tbl1_anonym_GENERIC_strictaverage_33 %>% mutate(across(all_of(col_num), as.numeric))
tbl1_anonym_GENERIC_kanonymity_11 <- tbl1_anonym_GENERIC_kanonymity_11 %>% mutate(across(all_of(col_num), as.numeric))
tbl1_anonym_GENERIC_kanonymity_20 <- tbl1_anonym_GENERIC_kanonymity_20 %>% mutate(across(all_of(col_num), as.numeric))
tbl1_anonym_GENERIC_kanonymity_33 <- tbl1_anonym_GENERIC_kanonymity_33 %>% mutate(across(all_of(col_num), as.numeric))
tbl1_anonym_USECASE_4QI_strictaverage_2 <- tbl1_anonym_USECASE_4QI_strictaverage_2 %>% mutate(across(all_of(col_num), as.numeric))
tbl1_anonym_USECASE_4QI_strictaverage_20 <- tbl1_anonym_USECASE_4QI_strictaverage_20 %>% mutate(across(all_of(col_num), as.numeric))
tbl1_anonym_USECASE_4QI_strictaverage_33 <- tbl1_anonym_USECASE_4QI_strictaverage_33 %>% mutate(across(all_of(col_num), as.numeric))
tbl1_anonym_USECASE_4QI_kanonymity_11 <- tbl1_anonym_USECASE_4QI_kanonymity_11 %>% mutate(across(all_of(col_num), as.numeric))
tbl1_anonym_USECASE_4QI_kanonymity_20 <- tbl1_anonym_USECASE_4QI_kanonymity_20 %>% mutate(across(all_of(col_num), as.numeric))
tbl1_anonym_USECASE_4QI_kanonymity_33 <- tbl1_anonym_USECASE_4QI_kanonymity_33 %>% mutate(across(all_of(col_num), as.numeric))
## Rounding 1 decimal
tbl1_origin <- tbl1_origin %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym_GENERIC_strictaverage_2 <- tbl1_anonym_GENERIC_strictaverage_2 %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym_GENERIC_strictaverage_20 <- tbl1_anonym_GENERIC_strictaverage_20 %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym_GENERIC_strictaverage_33 <- tbl1_anonym_GENERIC_strictaverage_33 %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym_GENERIC_kanonymity_11 <- tbl1_anonym_GENERIC_kanonymity_11 %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym_GENERIC_kanonymity_20 <- tbl1_anonym_GENERIC_kanonymity_20 %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym_GENERIC_kanonymity_33 <- tbl1_anonym_GENERIC_kanonymity_33 %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym_USECASE_4QI_strictaverage_2 <- tbl1_anonym_USECASE_4QI_strictaverage_2 %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym_USECASE_4QI_strictaverage_20 <- tbl1_anonym_USECASE_4QI_strictaverage_20 %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym_USECASE_4QI_strictaverage_33 <- tbl1_anonym_USECASE_4QI_strictaverage_33 %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym_USECASE_4QI_kanonymity_11 <- tbl1_anonym_USECASE_4QI_kanonymity_11 %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym_USECASE_4QI_kanonymity_20 <- tbl1_anonym_USECASE_4QI_kanonymity_20 %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym_USECASE_4QI_kanonymity_33 <- tbl1_anonym_USECASE_4QI_kanonymity_33 %>% mutate(across(where(is.numeric), ~round(., 1)))
## Dropping not needed variables
tbl1_origin <- tbl1_origin %>% subset(Variable != "Male" & Variable != "BL_age" & Variable != "<=25" & 
                                        Variable != "25.1-29.9" & Variable != ">30" & 
                                        Variable != "BL_ku_bmi" & Variable != "BL_ku_height_cm" & Variable != "BL_ku_weight" &
                                        Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & Variable != "BL_ku_map" & 
                                        Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
                                        Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "BL_uacr")
tbl1_anonym_GENERIC_strictaverage_2 <- tbl1_anonym_GENERIC_strictaverage_2 %>% 
  subset(Variable != "Male" & Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & 
           Variable != "BL_ku_map" & Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
           Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "BL_uacr")
tbl1_anonym_GENERIC_strictaverage_20 <- tbl1_anonym_GENERIC_strictaverage_20 %>% 
  subset(Variable != "Male" & Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & 
           Variable != "BL_ku_map" & Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
           Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "BL_uacr")
tbl1_anonym_GENERIC_strictaverage_33 <- tbl1_anonym_GENERIC_strictaverage_33 %>% 
  subset(Variable != "Male" & Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & 
           Variable != "BL_ku_map" & Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
           Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "BL_uacr")
tbl1_anonym_GENERIC_kanonymity_11 <- tbl1_anonym_GENERIC_kanonymity_11 %>% 
  subset(Variable != "Male" & Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & 
           Variable != "BL_ku_map" & Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
           Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "BL_uacr")
tbl1_anonym_GENERIC_kanonymity_20 <- tbl1_anonym_GENERIC_kanonymity_20 %>% 
  subset(Variable != "Male" & Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & 
           Variable != "BL_ku_map" & Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
           Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "BL_uacr")
tbl1_anonym_GENERIC_kanonymity_33 <- tbl1_anonym_GENERIC_kanonymity_33 %>% 
  subset(Variable != "Male" & Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & 
           Variable != "BL_ku_map" & Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
           Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "BL_uacr")
tbl1_anonym_USECASE_4QI_strictaverage_2 <- tbl1_anonym_USECASE_4QI_strictaverage_2 %>% 
  subset(Variable != "Male" & Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & 
           Variable != "BL_ku_map" & Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
           Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "BL_uacr")
tbl1_anonym_USECASE_4QI_strictaverage_20 <- tbl1_anonym_USECASE_4QI_strictaverage_20 %>% 
  subset(Variable != "Male" & Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & 
           Variable != "BL_ku_map" & Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
           Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "BL_uacr")
tbl1_anonym_USECASE_4QI_strictaverage_33 <- tbl1_anonym_USECASE_4QI_strictaverage_33 %>% 
  subset(Variable != "Male" & Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & 
           Variable != "BL_ku_map" & Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
           Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "BL_uacr")
tbl1_anonym_USECASE_4QI_kanonymity_11 <- tbl1_anonym_USECASE_4QI_kanonymity_11 %>% 
  subset(Variable != "Male" & Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & 
           Variable != "BL_ku_map" & Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
           Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "BL_uacr")
tbl1_anonym_USECASE_4QI_kanonymity_20 <- tbl1_anonym_USECASE_4QI_kanonymity_20 %>% 
  subset(Variable != "Male" & Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & 
           Variable != "BL_ku_map" & Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
           Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "BL_uacr")
tbl1_anonym_USECASE_4QI_kanonymity_33 <- tbl1_anonym_USECASE_4QI_kanonymity_33 %>% 
  subset(Variable != "Male" & Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & 
           Variable != "BL_ku_map" & Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
           Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "BL_uacr")
## Factor levels
tbl1_origin$Variable <- factor(tbl1_origin$Variable, levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                "BL_med_raas_ace_yes", "UACR300", "UACR30300", "UACR30",
                                                                "GFR30", "GFR3045", "GFR4560", "GFR60", "RR140_yes", "RR130_yes", 
                                                                "hospital_yes", "smoking_never", 
                                                                "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                "aa_stroke_yes"))  
tbl1_anonym_GENERIC_strictaverage_2$Variable <- factor(tbl1_anonym_GENERIC_strictaverage_2$Variable, 
                                                       levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                  "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                  "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                  "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                  "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                  "BL_med_raas_ace_yes", "UACR300", "UACR30300", "UACR30",
                                                                  "GFR30", "GFR3045", "GFR4560", "GFR60","RR140_yes", "RR130_yes", 
                                                                  "hospital_yes", "smoking_never", 
                                                                  "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                  "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                  "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                  "aa_stroke_yes"))
tbl1_anonym_GENERIC_strictaverage_20$Variable <- factor(tbl1_anonym_GENERIC_strictaverage_20$Variable, 
                                                        levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                   "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                   "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                   "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                   "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                   "BL_med_raas_ace_yes", "UACR300", "UACR30300", "UACR30",
                                                                   "GFR30", "GFR3045", "GFR4560", "GFR60","RR140_yes", "RR130_yes", 
                                                                   "hospital_yes", "smoking_never", 
                                                                   "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                   "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                   "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                   "aa_stroke_yes")) 
tbl1_anonym_GENERIC_strictaverage_33$Variable <- factor(tbl1_anonym_GENERIC_strictaverage_33$Variable, 
                                                        levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                   "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                   "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                   "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                   "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                   "BL_med_raas_ace_yes", "UACR300", "UACR30300", "UACR30",
                                                                   "GFR30", "GFR3045", "GFR4560", "GFR60","RR140_yes", "RR130_yes", 
                                                                   "hospital_yes", "smoking_never", 
                                                                   "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                   "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                   "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                   "aa_stroke_yes"))                                                                    
tbl1_anonym_GENERIC_kanonymity_11$Variable <- factor(tbl1_anonym_GENERIC_kanonymity_11$Variable, 
                                                     levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                "BL_med_raas_ace_yes", "UACR300", "UACR30300", "UACR30",
                                                                "GFR30", "GFR3045", "GFR4560", "GFR60", "RR140_yes", "RR130_yes",
                                                                "hospital_yes", "smoking_never", 
                                                                "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                "aa_stroke_yes"))  
tbl1_anonym_GENERIC_kanonymity_20$Variable <- factor(tbl1_anonym_GENERIC_kanonymity_20$Variable, 
                                                     levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                "BL_med_raas_ace_yes", "UACR300", "UACR30300", "UACR30",
                                                                "GFR30", "GFR3045", "GFR4560", "GFR60", "RR140_yes", "RR130_yes",
                                                                "hospital_yes", "smoking_never", 
                                                                "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                "aa_stroke_yes")) 
tbl1_anonym_GENERIC_kanonymity_33$Variable <- factor(tbl1_anonym_GENERIC_kanonymity_33$Variable, 
                                                     levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                "BL_med_raas_ace_yes", "UACR300", "UACR30300", "UACR30",
                                                                "GFR30", "GFR3045", "GFR4560", "GFR60", "RR140_yes", "RR130_yes",
                                                                "hospital_yes", "smoking_never", 
                                                                "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                "aa_stroke_yes"))                                                                 
tbl1_anonym_USECASE_4QI_strictaverage_2$Variable <- factor(tbl1_anonym_USECASE_4QI_strictaverage_2$Variable, 
                                                           levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                      "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                      "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                      "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                      "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                      "BL_med_raas_ace_yes", "UACR300", "UACR30300", "UACR30",
                                                                      "GFR30", "GFR3045", "GFR4560", "GFR60", "RR140_yes", "RR130_yes",
                                                                      "hospital_yes", "smoking_never", 
                                                                      "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                      "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                      "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                      "aa_stroke_yes"))
tbl1_anonym_USECASE_4QI_strictaverage_20$Variable <- factor(tbl1_anonym_USECASE_4QI_strictaverage_20$Variable, 
                                                            levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                       "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                       "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                       "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                       "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                       "BL_med_raas_ace_yes", "UACR300", "UACR30300", "UACR30",
                                                                       "GFR30", "GFR3045", "GFR4560", "GFR60", "RR140_yes", "RR130_yes",
                                                                       "hospital_yes", "smoking_never", 
                                                                       "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                       "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                       "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                       "aa_stroke_yes")) 
tbl1_anonym_USECASE_4QI_strictaverage_33$Variable <- factor(tbl1_anonym_USECASE_4QI_strictaverage_33$Variable, 
                                                            levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                       "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                       "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                       "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                       "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                       "BL_med_raas_ace_yes", "UACR300", "UACR30300", "UACR30",
                                                                       "GFR30", "GFR3045", "GFR4560", "GFR60", "RR140_yes", "RR130_yes",
                                                                       "hospital_yes", "smoking_never", 
                                                                       "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                       "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                       "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                       "aa_stroke_yes")) 
tbl1_anonym_USECASE_4QI_kanonymity_11$Variable <- factor(tbl1_anonym_USECASE_4QI_kanonymity_11$Variable, 
                                                         levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                    "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                    "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                    "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                    "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                    "BL_med_raas_ace_yes", "UACR300", "UACR30300", "UACR30",
                                                                    "GFR30", "GFR3045", "GFR4560", "GFR60", "RR140_yes", "RR130_yes",
                                                                    "hospital_yes", "smoking_never", 
                                                                    "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                    "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                    "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                    "aa_stroke_yes"))  
tbl1_anonym_USECASE_4QI_kanonymity_20$Variable <- factor(tbl1_anonym_USECASE_4QI_kanonymity_20$Variable, 
                                                         levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                    "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                    "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                    "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                    "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                    "BL_med_raas_ace_yes", "UACR300", "UACR30300", "UACR30",
                                                                    "GFR30", "GFR3045", "GFR4560", "GFR60","RR140_yes", "RR130_yes", 
                                                                    "hospital_yes", "smoking_never", 
                                                                    "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                    "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                    "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                    "aa_stroke_yes"))
tbl1_anonym_USECASE_4QI_kanonymity_33$Variable <- factor(tbl1_anonym_USECASE_4QI_kanonymity_33$Variable, 
                                                         levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                    "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                    "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                    "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                    "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                    "BL_med_raas_ace_yes", "UACR300", "UACR30300", "UACR30",
                                                                    "GFR30", "GFR3045", "GFR4560", "GFR60","RR140_yes", "RR130_yes", 
                                                                    "hospital_yes", "smoking_never", 
                                                                    "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                    "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                    "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                    "aa_stroke_yes"))                                                                                                                                        
## Separate datasets
tbl1_origin_low <- tbl1_origin[,c("CI_female_d_low", "Variable")]
tbl1_origin_low$bound = "low"
names(tbl1_origin_low)[names(tbl1_origin_low) == "CI_female_d_low"] <- "CI_bound_female_d"
tbl1_origin_up <- tbl1_origin[,c("CI_female_d_up", "Variable")]
tbl1_origin_up$bound = "up"
names(tbl1_origin_up)[names(tbl1_origin_up) == "CI_female_d_up"] <- "CI_bound_female_d"
tbl1_origin_female_d <- rbind(tbl1_origin_low, tbl1_origin_up)
tbl1_anonym_GENERIC_strictaverage_2_low <- tbl1_anonym_GENERIC_strictaverage_2[,c("CI_female_d_low", "Variable")]
tbl1_anonym_GENERIC_strictaverage_2_low$bound = "low"
names(tbl1_anonym_GENERIC_strictaverage_2_low)[names(tbl1_anonym_GENERIC_strictaverage_2_low) == "CI_female_d_low"] <- "CI_bound_female_d"
tbl1_anonym_GENERIC_strictaverage_2_up <- tbl1_anonym_GENERIC_strictaverage_2[,c("CI_female_d_up", "Variable")]
tbl1_anonym_GENERIC_strictaverage_2_up$bound = "up"
names(tbl1_anonym_GENERIC_strictaverage_2_up)[names(tbl1_anonym_GENERIC_strictaverage_2_up) == "CI_female_d_up"] <- "CI_bound_female_d"
tbl1_anonym_GENERIC_strictaverage_2_female_d <- rbind(tbl1_anonym_GENERIC_strictaverage_2_low, tbl1_anonym_GENERIC_strictaverage_2_up)
tbl1_anonym_GENERIC_strictaverage_20_low <- tbl1_anonym_GENERIC_strictaverage_20[,c("CI_female_d_low", "Variable")]
tbl1_anonym_GENERIC_strictaverage_20_low$bound = "low"
names(tbl1_anonym_GENERIC_strictaverage_20_low)[names(tbl1_anonym_GENERIC_strictaverage_20_low) == "CI_female_d_low"] <- "CI_bound_female_d"
tbl1_anonym_GENERIC_strictaverage_20_up <- tbl1_anonym_GENERIC_strictaverage_20[,c("CI_female_d_up", "Variable")]
tbl1_anonym_GENERIC_strictaverage_20_up$bound = "up"
names(tbl1_anonym_GENERIC_strictaverage_20_up)[names(tbl1_anonym_GENERIC_strictaverage_20_up) == "CI_female_d_up"] <- "CI_bound_female_d"
tbl1_anonym_GENERIC_strictaverage_20_female_d <- rbind(tbl1_anonym_GENERIC_strictaverage_20_low, tbl1_anonym_GENERIC_strictaverage_20_up)
tbl1_anonym_GENERIC_strictaverage_33_low <- tbl1_anonym_GENERIC_strictaverage_33[,c("CI_female_d_low", "Variable")]
tbl1_anonym_GENERIC_strictaverage_33_low$bound = "low"
names(tbl1_anonym_GENERIC_strictaverage_33_low)[names(tbl1_anonym_GENERIC_strictaverage_33_low) == "CI_female_d_low"] <- "CI_bound_female_d"
tbl1_anonym_GENERIC_strictaverage_33_up <- tbl1_anonym_GENERIC_strictaverage_33[,c("CI_female_d_up", "Variable")]
tbl1_anonym_GENERIC_strictaverage_33_up$bound = "up"
names(tbl1_anonym_GENERIC_strictaverage_33_up)[names(tbl1_anonym_GENERIC_strictaverage_33_up) == "CI_female_d_up"] <- "CI_bound_female_d"
tbl1_anonym_GENERIC_strictaverage_33_female_d <- rbind(tbl1_anonym_GENERIC_strictaverage_33_low, tbl1_anonym_GENERIC_strictaverage_33_up)
tbl1_anonym_GENERIC_kanonymity_11_low <- tbl1_anonym_GENERIC_kanonymity_11[,c("CI_female_d_low", "Variable")]
tbl1_anonym_GENERIC_kanonymity_11_low$bound = "low"
names(tbl1_anonym_GENERIC_kanonymity_11_low)[names(tbl1_anonym_GENERIC_kanonymity_11_low) == "CI_female_d_low"] <- "CI_bound_female_d"
tbl1_anonym_GENERIC_kanonymity_11_up <- tbl1_anonym_GENERIC_kanonymity_11[,c("CI_female_d_up", "Variable")]
tbl1_anonym_GENERIC_kanonymity_11_up$bound = "up"
names(tbl1_anonym_GENERIC_kanonymity_11_up)[names(tbl1_anonym_GENERIC_kanonymity_11_up) == "CI_female_d_up"] <- "CI_bound_female_d"
tbl1_anonym_GENERIC_kanonymity_11_female_d <- rbind(tbl1_anonym_GENERIC_kanonymity_11_low, tbl1_anonym_GENERIC_kanonymity_11_up)
tbl1_anonym_GENERIC_kanonymity_20_low <- tbl1_anonym_GENERIC_kanonymity_20[,c("CI_female_d_low", "Variable")]
tbl1_anonym_GENERIC_kanonymity_20_low$bound = "low"
names(tbl1_anonym_GENERIC_kanonymity_20_low)[names(tbl1_anonym_GENERIC_kanonymity_20_low) == "CI_female_d_low"] <- "CI_bound_female_d"
tbl1_anonym_GENERIC_kanonymity_20_up <- tbl1_anonym_GENERIC_kanonymity_20[,c("CI_female_d_up", "Variable")]
tbl1_anonym_GENERIC_kanonymity_20_up$bound = "up"
names(tbl1_anonym_GENERIC_kanonymity_20_up)[names(tbl1_anonym_GENERIC_kanonymity_20_up) == "CI_female_d_up"] <- "CI_bound_female_d"
tbl1_anonym_GENERIC_kanonymity_20_female_d <- rbind(tbl1_anonym_GENERIC_kanonymity_20_low, tbl1_anonym_GENERIC_kanonymity_20_up)
tbl1_anonym_GENERIC_kanonymity_33_low <- tbl1_anonym_GENERIC_kanonymity_33[,c("CI_female_d_low", "Variable")]
tbl1_anonym_GENERIC_kanonymity_33_low$bound = "low"
names(tbl1_anonym_GENERIC_kanonymity_33_low)[names(tbl1_anonym_GENERIC_kanonymity_33_low) == "CI_female_d_low"] <- "CI_bound_female_d"
tbl1_anonym_GENERIC_kanonymity_33_up <- tbl1_anonym_GENERIC_kanonymity_33[,c("CI_female_d_up", "Variable")]
tbl1_anonym_GENERIC_kanonymity_33_up$bound = "up"
names(tbl1_anonym_GENERIC_kanonymity_33_up)[names(tbl1_anonym_GENERIC_kanonymity_33_up) == "CI_female_d_up"] <- "CI_bound_female_d"
tbl1_anonym_GENERIC_kanonymity_33_female_d <- rbind(tbl1_anonym_GENERIC_kanonymity_33_low, tbl1_anonym_GENERIC_kanonymity_33_up)
tbl1_anonym_USECASE_4QI_strictaverage_2_low <- tbl1_anonym_USECASE_4QI_strictaverage_2[,c("CI_female_d_low", "Variable")]
tbl1_anonym_USECASE_4QI_strictaverage_2_low$bound = "low"
names(tbl1_anonym_USECASE_4QI_strictaverage_2_low)[names(tbl1_anonym_USECASE_4QI_strictaverage_2_low) == "CI_female_d_low"] <- "CI_bound_female_d"
tbl1_anonym_USECASE_4QI_strictaverage_2_up <- tbl1_anonym_USECASE_4QI_strictaverage_2[,c("CI_female_d_up", "Variable")]
tbl1_anonym_USECASE_4QI_strictaverage_2_up$bound = "up"
names(tbl1_anonym_USECASE_4QI_strictaverage_2_up)[names(tbl1_anonym_USECASE_4QI_strictaverage_2_up) == "CI_female_d_up"] <- "CI_bound_female_d"
tbl1_anonym_USECASE_4QI_strictaverage_2_female_d <- rbind(tbl1_anonym_USECASE_4QI_strictaverage_2_low, tbl1_anonym_USECASE_4QI_strictaverage_2_up)
tbl1_anonym_USECASE_4QI_strictaverage_20_low <- tbl1_anonym_USECASE_4QI_strictaverage_20[,c("CI_female_d_low", "Variable")]
tbl1_anonym_USECASE_4QI_strictaverage_20_low$bound = "low"
names(tbl1_anonym_USECASE_4QI_strictaverage_20_low)[names(tbl1_anonym_USECASE_4QI_strictaverage_20_low) == "CI_female_d_low"] <- "CI_bound_female_d"
tbl1_anonym_USECASE_4QI_strictaverage_20_up <- tbl1_anonym_USECASE_4QI_strictaverage_20[,c("CI_female_d_up", "Variable")]
tbl1_anonym_USECASE_4QI_strictaverage_20_up$bound = "up"
names(tbl1_anonym_USECASE_4QI_strictaverage_20_up)[names(tbl1_anonym_USECASE_4QI_strictaverage_20_up) == "CI_female_d_up"] <- "CI_bound_female_d"
tbl1_anonym_USECASE_4QI_strictaverage_20_female_d <- rbind(tbl1_anonym_USECASE_4QI_strictaverage_20_low, tbl1_anonym_USECASE_4QI_strictaverage_20_up)
tbl1_anonym_USECASE_4QI_strictaverage_33_low <- tbl1_anonym_USECASE_4QI_strictaverage_33[,c("CI_female_d_low", "Variable")]
tbl1_anonym_USECASE_4QI_strictaverage_33_low$bound = "low"
names(tbl1_anonym_USECASE_4QI_strictaverage_33_low)[names(tbl1_anonym_USECASE_4QI_strictaverage_33_low) == "CI_female_d_low"] <- "CI_bound_female_d"
tbl1_anonym_USECASE_4QI_strictaverage_33_up <- tbl1_anonym_USECASE_4QI_strictaverage_33[,c("CI_female_d_up", "Variable")]
tbl1_anonym_USECASE_4QI_strictaverage_33_up$bound = "up"
names(tbl1_anonym_USECASE_4QI_strictaverage_33_up)[names(tbl1_anonym_USECASE_4QI_strictaverage_33_up) == "CI_female_d_up"] <- "CI_bound_female_d"
tbl1_anonym_USECASE_4QI_strictaverage_33_female_d <- rbind(tbl1_anonym_USECASE_4QI_strictaverage_33_low, tbl1_anonym_USECASE_4QI_strictaverage_33_up)
tbl1_anonym_USECASE_4QI_kanonymity_11_low <- tbl1_anonym_USECASE_4QI_kanonymity_11[,c("CI_female_d_low", "Variable")]
tbl1_anonym_USECASE_4QI_kanonymity_11_low$bound = "low"
names(tbl1_anonym_USECASE_4QI_kanonymity_11_low)[names(tbl1_anonym_USECASE_4QI_kanonymity_11_low) == "CI_female_d_low"] <- "CI_bound_female_d"
tbl1_anonym_USECASE_4QI_kanonymity_11_up <- tbl1_anonym_USECASE_4QI_kanonymity_11[,c("CI_female_d_up", "Variable")]
tbl1_anonym_USECASE_4QI_kanonymity_11_up$bound = "up"
names(tbl1_anonym_USECASE_4QI_kanonymity_11_up)[names(tbl1_anonym_USECASE_4QI_kanonymity_11_up) == "CI_female_d_up"] <- "CI_bound_female_d"
tbl1_anonym_USECASE_4QI_kanonymity_11_female_d <- rbind(tbl1_anonym_USECASE_4QI_kanonymity_11_low, tbl1_anonym_USECASE_4QI_kanonymity_11_up)
tbl1_anonym_USECASE_4QI_kanonymity_20_low <- tbl1_anonym_USECASE_4QI_kanonymity_20[,c("CI_female_d_low", "Variable")]
tbl1_anonym_USECASE_4QI_kanonymity_20_low$bound = "low"
names(tbl1_anonym_USECASE_4QI_kanonymity_20_low)[names(tbl1_anonym_USECASE_4QI_kanonymity_20_low) == "CI_female_d_low"] <- "CI_bound_female_d"
tbl1_anonym_USECASE_4QI_kanonymity_20_up <- tbl1_anonym_USECASE_4QI_kanonymity_20[,c("CI_female_d_up", "Variable")]
tbl1_anonym_USECASE_4QI_kanonymity_20_up$bound = "up"
names(tbl1_anonym_USECASE_4QI_kanonymity_20_up)[names(tbl1_anonym_USECASE_4QI_kanonymity_20_up) == "CI_female_d_up"] <- "CI_bound_female_d"
tbl1_anonym_USECASE_4QI_kanonymity_20_female_d <- rbind(tbl1_anonym_USECASE_4QI_kanonymity_20_low, tbl1_anonym_USECASE_4QI_kanonymity_20_up)
tbl1_anonym_USECASE_4QI_kanonymity_33_low <- tbl1_anonym_USECASE_4QI_kanonymity_33[,c("CI_female_d_low", "Variable")]
tbl1_anonym_USECASE_4QI_kanonymity_33_low$bound = "low"
names(tbl1_anonym_USECASE_4QI_kanonymity_33_low)[names(tbl1_anonym_USECASE_4QI_kanonymity_33_low) == "CI_female_d_low"] <- "CI_bound_female_d"
tbl1_anonym_USECASE_4QI_kanonymity_33_up <- tbl1_anonym_USECASE_4QI_kanonymity_33[,c("CI_female_d_up", "Variable")]
tbl1_anonym_USECASE_4QI_kanonymity_33_up$bound = "up"
names(tbl1_anonym_USECASE_4QI_kanonymity_33_up)[names(tbl1_anonym_USECASE_4QI_kanonymity_33_up) == "CI_female_d_up"] <- "CI_bound_female_d"
tbl1_anonym_USECASE_4QI_kanonymity_33_female_d <- rbind(tbl1_anonym_USECASE_4QI_kanonymity_33_low, tbl1_anonym_USECASE_4QI_kanonymity_33_up)

# Figure 95% CI overlap for categorical data in subset non diabetic females (CAVE d = non diabetic)
## including k=2, k=11, k=20
ggplot() + 
  geom_line(data=tbl1_anonym_GENERIC_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "indianred4", position = position_nudge(y = 0.1)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "indianred4", position = position_nudge(y = 0.1)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_2, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "indianred4", position = position_nudge(y = 0.1)) + 
  geom_line(data=tbl1_anonym_GENERIC_strictaverage_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "indianred", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "indianred", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_20, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "indianred", position = position_nudge(y = 0.2)) + 
  geom_line(data=tbl1_anonym_GENERIC_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "goldenrod4", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "goldenrod4", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_11, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "goldenrod4", position = position_nudge(y = 0.3)) + 
  geom_line(data=tbl1_anonym_GENERIC_kanonymity_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "goldenrod", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "goldenrod", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_20, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "goldenrod", position = position_nudge(y = 0.4)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "darkseagreen4", position = position_nudge(y = -0.1)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen4", position = position_nudge(y = -0.1)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_2, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "darkseagreen4", position = position_nudge(y = -0.1)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_strictaverage_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "darkseagreen", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_20, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "darkseagreen", position = position_nudge(y = -0.2)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "skyblue4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "skyblue4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_11, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "skyblue4", position = position_nudge(y = -0.3)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_kanonymity_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "skyblue", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "skyblue", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_20, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "skyblue", position = position_nudge(y = -0.4)) + 
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### GENERIC
ggplot() + 
  geom_line(data=tbl1_anonym_GENERIC_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "indianred4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "indianred4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_2, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "indianred4", position = position_nudge(y = 0.2)) + 
  geom_line(data=tbl1_anonym_GENERIC_strictaverage_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "indianred", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "indianred", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_20, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "indianred", position = position_nudge(y = 0.4)) + 
  geom_line(data=tbl1_anonym_GENERIC_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "goldenrod4", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "goldenrod4", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_11, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "goldenrod4", position = position_nudge(y = -0.2)) + 
  geom_line(data=tbl1_anonym_GENERIC_kanonymity_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "goldenrod", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "goldenrod", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_20, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "goldenrod", position = position_nudge(y = -0.4)) +
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100)  +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### USECASE
ggplot() + 
  geom_line(data=tbl1_anonym_USECASE_4QI_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "darkseagreen4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_2, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "darkseagreen4", position = position_nudge(y = 0.2)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_strictaverage_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "darkseagreen", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_20, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "darkseagreen", position = position_nudge(y = 0.4)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "skyblue4", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "skyblue4", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_11, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "skyblue4", position = position_nudge(y = -0.2)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_kanonymity_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "skyblue", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "skyblue", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_20, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "skyblue", position = position_nudge(y = -0.4)) +
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### strictaverage
ggplot() + 
  geom_line(data=tbl1_anonym_GENERIC_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "indianred4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "indianred4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_2, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "indianred4", position = position_nudge(y = 0.2)) + 
  geom_line(data=tbl1_anonym_GENERIC_strictaverage_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "indianred", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "indianred", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_20, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "indianred", position = position_nudge(y = 0.4)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "darkseagreen4", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen4", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_2, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "darkseagreen4", position = position_nudge(y = -0.2)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_strictaverage_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "darkseagreen", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_20, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "darkseagreen", position = position_nudge(y = -0.4)) + 
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### kanonymity
ggplot() + 
  geom_line(data=tbl1_anonym_GENERIC_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "goldenrod4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "goldenrod4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_11, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "goldenrod4", position = position_nudge(y = 0.2)) + 
  geom_line(data=tbl1_anonym_GENERIC_kanonymity_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "goldenrod", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "goldenrod", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_20, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "goldenrod", position = position_nudge(y = 0.4)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "skyblue4", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "skyblue4", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_11, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "skyblue4", position = position_nudge(y = -0.2)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_kanonymity_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "skyblue", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "skyblue", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_20, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "skyblue", position = position_nudge(y = -0.4)) + 
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### GENERIC + strictaverage
ggplot() + 
  geom_line(data=tbl1_anonym_GENERIC_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "indianred4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "indianred4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_2, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "indianred4", position = position_nudge(y = -0.3)) + 
  geom_line(data=tbl1_anonym_GENERIC_strictaverage_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "indianred", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "indianred", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_20, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "indianred", position = position_nudge(y = 0.3))  +
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100)  +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### GENERIC + kanonymity
ggplot() + 
  geom_line(data=tbl1_anonym_GENERIC_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "goldenrod4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "goldenrod4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_11, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "goldenrod4", position = position_nudge(y = -0.3)) + 
  geom_line(data=tbl1_anonym_GENERIC_kanonymity_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "goldenrod", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "goldenrod", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_20, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "goldenrod", position = position_nudge(y = 0.3)) +
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100)  +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### USECASE + strictaverage
ggplot() + 
  geom_line(data=tbl1_anonym_USECASE_4QI_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "darkseagreen4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_2, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "darkseagreen4", position = position_nudge(y = -0.3)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_strictaverage_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "darkseagreen", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_20, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "darkseagreen", position = position_nudge(y = 0.3)) + 
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### USECASE + kanonymity
ggplot() + 
  geom_line(data=tbl1_anonym_USECASE_4QI_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "skyblue4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "skyblue4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_11, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "skyblue4", position = position_nudge(y = -0.3)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_kanonymity_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "skyblue", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_20_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "skyblue", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_20, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "skyblue", position = position_nudge(y = 0.3)) +
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

## including k=2, k=11, k=33
ggplot() + 
  geom_line(data=tbl1_anonym_GENERIC_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "indianred4", position = position_nudge(y = 0.1)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "indianred4", position = position_nudge(y = 0.1)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_2, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "indianred4", position = position_nudge(y = 0.1)) + 
  geom_line(data=tbl1_anonym_GENERIC_strictaverage_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "indianred", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "indianred", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_33, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "indianred", position = position_nudge(y = 0.2)) + 
  geom_line(data=tbl1_anonym_GENERIC_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "goldenrod4", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "goldenrod4", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_11, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "goldenrod4", position = position_nudge(y = 0.3)) + 
  geom_line(data=tbl1_anonym_GENERIC_kanonymity_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "goldenrod", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "goldenrod", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_33, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "goldenrod", position = position_nudge(y = 0.4)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "darkseagreen4", position = position_nudge(y = -0.1)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen4", position = position_nudge(y = -0.1)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_2, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "darkseagreen4", position = position_nudge(y = -0.1)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_strictaverage_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "darkseagreen", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_33, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "darkseagreen", position = position_nudge(y = -0.2)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "skyblue4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "skyblue4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_11, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "skyblue4", position = position_nudge(y = -0.3)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_kanonymity_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "skyblue", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "skyblue", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_33, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "skyblue", position = position_nudge(y = -0.4)) + 
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100)
### GENERIC
ggplot() + 
  geom_line(data=tbl1_anonym_GENERIC_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "indianred4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "indianred4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_2, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "indianred4", position = position_nudge(y = 0.2)) + 
  geom_line(data=tbl1_anonym_GENERIC_strictaverage_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "indianred", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "indianred", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_33, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "indianred", position = position_nudge(y = 0.4)) + 
  geom_line(data=tbl1_anonym_GENERIC_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "goldenrod4", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "goldenrod4", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_11, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "goldenrod4", position = position_nudge(y = -0.2)) + 
  geom_line(data=tbl1_anonym_GENERIC_kanonymity_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "goldenrod", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "goldenrod", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_33, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "goldenrod", position = position_nudge(y = -0.4)) +
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100)  +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### USECASE
ggplot() + 
  geom_line(data=tbl1_anonym_USECASE_4QI_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "darkseagreen4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_2, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "darkseagreen4", position = position_nudge(y = 0.2)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_strictaverage_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "darkseagreen", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_33, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "darkseagreen", position = position_nudge(y = 0.4)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "skyblue4", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "skyblue4", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_11, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "skyblue4", position = position_nudge(y = -0.2)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_kanonymity_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "skyblue", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "skyblue", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_33, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "skyblue", position = position_nudge(y = -0.4)) +
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### strictaverage
ggplot() + 
  geom_line(data=tbl1_anonym_GENERIC_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "indianred4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "indianred4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_2, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "indianred4", position = position_nudge(y = 0.2)) + 
  geom_line(data=tbl1_anonym_GENERIC_strictaverage_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "indianred", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "indianred", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_33, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "indianred", position = position_nudge(y = 0.4)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "darkseagreen4", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen4", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_2, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "darkseagreen4", position = position_nudge(y = -0.2)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_strictaverage_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "darkseagreen", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_33, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "darkseagreen", position = position_nudge(y = -0.4)) + 
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### kanonymity
ggplot() + 
  geom_line(data=tbl1_anonym_GENERIC_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "goldenrod4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "goldenrod4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_11, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "goldenrod4", position = position_nudge(y = 0.2)) + 
  geom_line(data=tbl1_anonym_GENERIC_kanonymity_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "goldenrod", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "goldenrod", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_33, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "goldenrod", position = position_nudge(y = 0.4)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "skyblue4", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "skyblue4", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_11, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "skyblue4", position = position_nudge(y = -0.2)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_kanonymity_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "skyblue", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "skyblue", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_33, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "skyblue", position = position_nudge(y = -0.4)) + 
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### GENERIC + strictaverage
ggplot() + 
  geom_line(data=tbl1_anonym_GENERIC_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "indianred4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "indianred4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_2, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "indianred4", position = position_nudge(y = -0.3)) + 
  geom_line(data=tbl1_anonym_GENERIC_strictaverage_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "indianred", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "indianred", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_strictaverage_33, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "indianred", position = position_nudge(y = 0.3))  +
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100)  +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### GENERIC + kanonymity
ggplot() + 
  geom_line(data=tbl1_anonym_GENERIC_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "goldenrod4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "goldenrod4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_11, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "goldenrod4", position = position_nudge(y = -0.3)) + 
  geom_line(data=tbl1_anonym_GENERIC_kanonymity_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "goldenrod", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "goldenrod", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_GENERIC_kanonymity_33, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "goldenrod", position = position_nudge(y = 0.3)) +
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100)  +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### USECASE + strictaverage
ggplot() + 
  geom_line(data=tbl1_anonym_USECASE_4QI_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "darkseagreen4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_2_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_2, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "darkseagreen4", position = position_nudge(y = -0.3)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_strictaverage_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "darkseagreen", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_strictaverage_33, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "darkseagreen", position = position_nudge(y = 0.3)) + 
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### USECASE + kanonymity
ggplot() + 
  geom_line(data=tbl1_anonym_USECASE_4QI_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "skyblue4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_11_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "skyblue4", position = position_nudge(y = -0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_11, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "skyblue4", position = position_nudge(y = -0.3)) + 
  geom_line(data=tbl1_anonym_USECASE_4QI_kanonymity_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, color = "skyblue", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_33_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, color = "skyblue", position = position_nudge(y = 0.3)) +
  geom_point(data=tbl1_anonym_USECASE_4QI_kanonymity_33, aes(x = perc_female_d, y = Variable, group = Variable), size=2, color = "skyblue", position = position_nudge(y = 0.3)) +
  geom_line(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_d, aes(x = CI_bound_female_d, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin, aes(x = perc_female_d, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  xlim(0, 100) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
