# Packages 
pacman::p_load(tidyr, stringr, dplyr, openxlsx, naniar, emmeans, multcomp, 
               plyr, finalfit, ggplot2, tibble, lmtest, sandwich,
               tidyverse, tidyselect, summarytools, scales, gridExtra, 
               lubridate, eeptools, gtsummary, flextable, boot, mosaic, 
               patchwork, rms, coxed, DescTools, PropCIs)

# Dataset
path_data = "D:/BackUp GCKD"
path_results = "C:/Users/User/OneDrive/Documents/PRIVAT/Charite/Forschung/Projekt Computerbasierte Anonymisierung/Titzeetal/Ergebnisse"
setwd(path_results)

# similarity of results: replicability
## Defining function for weather estimates are within the confidence interval obtained from original data
estimate_CI <- function(Lor, Uor, Ean){
  if(is.na(Lor) == TRUE | is.na(Uor) == TRUE | is.na(Ean) == TRUE){
    return(NA)
  }
  else if(Ean >= Lor & Ean <= Uor){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}
## Defining function for overlap in the interval lengths 
### based on Karr et al
CI_overlap <- function(Lor, Uor, Lan, Uan){
  if(is.na(Uor) == TRUE | is.na(Uan) == TRUE | is.na(Lor) == TRUE | is.na(Lan) == TRUE){
    CIo <- NA
  }
  else if(Uor <= Uan & Lor >= Lan){
    CIo <- 0.5*((Uor-Lor)/(Uor-Lor)+(Uor-Lor)/(Uan-Lan))
  }
  else if(Uan <= Uor & Lan >= Lor){CI
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

### Package HTSSIP perc_overlap >> only percentage of coverage of original 95% CI

# Comparing tables
tbl1_origin <- as_tibble(read.xlsx("GCKD_results_tbl1_origin.xlsx", sep = ";"))
tbl1_anonym <- as_tibble(read.xlsx("GCKD_results_tbl1_generic_k11.xlsx", sep = ";"))
tbl1_anonym_2 <- as_tibble(read.xlsx("GCKD_results_tbl1_generic_k11k2.xlsx", sep = ";"))
tbl1_anonym_3 <- as_tibble(read.xlsx("GCKD_results_tbl1_specific_k11.xlsx", sep = ";"))
tbl1_anonym_4 <- as_tibble(read.xlsx("GCKD_results_tbl1_specific_k11k2.xlsx", sep = ";"))

# Tbl 1
# Preprocessing
## Separating 95% CI bounds
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
tbl1_anonym_2[c("CI_female_nd_low", "CI_female_nd_up")] <- str_split_fixed(tbl1_anonym_2$CI_female_nd, "-", 2)
tbl1_anonym_2$CI_female_nd_low <- as.numeric(tbl1_anonym_2$CI_female_nd_low)
tbl1_anonym_2$CI_female_nd_up <- as.numeric(tbl1_anonym_2$CI_female_nd_up)
tbl1_anonym_3[c("CI_female_nd_low", "CI_female_nd_up")] <- str_split_fixed(tbl1_anonym_3$CI_female_nd, "-", 2)
tbl1_anonym_3$CI_female_nd_low <- as.numeric(tbl1_anonym_3$CI_female_nd_low)
tbl1_anonym_3$CI_female_nd_up <- as.numeric(tbl1_anonym_3$CI_female_nd_up)
tbl1_anonym_4[c("CI_female_nd_low", "CI_female_nd_up")] <- str_split_fixed(tbl1_anonym_4$CI_female_nd, "-", 2)
tbl1_anonym_4$CI_female_nd_low <- as.numeric(tbl1_anonym_4$CI_female_nd_low)
tbl1_anonym_4$CI_female_nd_up <- as.numeric(tbl1_anonym_4$CI_female_nd_up)
## Rounding 1 decimal
tbl1_origin <- tbl1_origin %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym <- tbl1_anonym %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym_2 <- tbl1_anonym_2 %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym_3 <- tbl1_anonym_3 %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym_4 <- tbl1_anonym_4 %>% mutate(across(where(is.numeric), ~round(., 1)))
# Calculations
## estimate within 95% CI
tbl1_est <- data.frame(NA_col = rep(NA, 52))
res_male_d = numeric(52)
res_male_nd = numeric(52)
res_female_d = numeric(52)
res_female_nd = numeric(52)
res = numeric(52)
for(i in 1:nrow((tbl1_anonym))) {
  var = tbl1_origin$Variable[i]
  res_male_d[i] <- estimate_CI(tbl1_origin$CI_male_d_low[i], tbl1_origin$CI_male_d_up[i], tbl1_anonym$perc_male_d[i])
  tbl1_est[, 1] <- res_male_d
  res_male_nd[i] <- estimate_CI(tbl1_origin$CI_male_nd_low[i], tbl1_origin$CI_male_nd_up[i], tbl1_anonym$perc_male_nd[i])
  tbl1_est[, 2] <- res_male_nd
  res_female_d[i] <- estimate_CI(tbl1_origin$CI_female_d_low[i], tbl1_origin$CI_female_d_up[i], tbl1_anonym$perc_female_d[i])
  tbl1_est[, 3] <- res_female_d
  res_female_nd[i] <- estimate_CI(tbl1_origin$CI_female_nd_low[i], tbl1_origin$CI_female_nd_up[i], tbl1_anonym$perc_female_nd[i])
  tbl1_est[, 4] <- res_female_nd
  res[i] <- estimate_CI(tbl1_origin$CI_total_low[i], tbl1_origin$CI_total_up[i], tbl1_anonym$perc_total[i]) 
  tbl1_est[, 5] <- res
  rownames(tbl1_est)[i] <- paste0(var)
  colnames(tbl1_est)[1] <- paste0("Estimate_male_d")
  colnames(tbl1_est)[2] <- paste0("Estimate_male_nd")
  colnames(tbl1_est)[3] <- paste0("Estimate_female_d")
  colnames(tbl1_est)[4] <- paste0("Estimate_female_nd")
  colnames(tbl1_est)[5] <- paste0("Estimate_total")
}
tbl1_est$Variable <- row.names(tbl1_est)
write.xlsx(tbl1_est, "GCKD_results_est_tbl1_specific_k11.xlsx")
## overlapping 95% CI
tbl1_ci <- data.frame(NA_col = rep(NA, 52))
res_ci_male_d = numeric(52)
res_ci_male_nd = numeric(52)
res_ci_female_d = numeric(52)
res_ci_female_nd = numeric(52)
res_ci = numeric(52)
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
write.xlsx(tbl1_ci, "GCKD_results_ci_tbl1_specific_k11.xlsx")
## Graphical illustration only categorical variables for subset non diabetic females
tbl1_origin_cat <- tbl1_origin %>% subset(Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & Variable != "BL_ku_map" & 
                                            Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
                                            Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "male")
tbl1_origin_cat <- subset(tbl1_origin_cat, !(endsWith(Variable, '_no')))
tbl1_origin_cat$Variable <- factor(tbl1_origin_cat$Variable, levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                              "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                              "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                              "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                              "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                              "BL_med_raas_ace_yes", "hospital_yes", "smoking_never", 
                                                                              "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                              "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                              "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                              "aa_stroke_yes", "female"))  
tbl1_origin_low <- tbl1_origin_cat[,c("CI_female_nd_low", "Variable")]
tbl1_origin_low$bound = "low"
names(tbl1_origin_low)[names(tbl1_origin_low) == "CI_female_nd_low"] <- "CI_bound_female_nd"
tbl1_origin_up <- tbl1_origin_cat[,c("CI_female_nd_up", "Variable")]
tbl1_origin_up$bound = "up"
names(tbl1_origin_up)[names(tbl1_origin_up) == "CI_female_nd_up"] <- "CI_bound_female_nd"
tbl1_origin_female_nd <- rbind(tbl1_origin_low, tbl1_origin_up)
tbl1_anonym_cat <- tbl1_anonym %>% subset(Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & Variable != "BL_ku_map" & 
                                            Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
                                            Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "male")
tbl1_anonym_cat <- subset(tbl1_anonym_cat, !(endsWith(Variable, '_no')))
tbl1_anonym_cat$Variable <- factor(tbl1_anonym_cat$Variable, levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                              "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                              "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                              "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                              "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                              "BL_med_raas_ace_yes", "hospital_yes", "smoking_never", 
                                                                              "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                              "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                              "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                              "aa_stroke_yes", "female")) 
tbl1_anonym_low <- tbl1_anonym_cat[,c("CI_female_nd_low", "Variable")]
tbl1_anonym_low$bound = "low"
names(tbl1_anonym_low)[names(tbl1_anonym_low) == "CI_female_nd_low"] <- "CI_bound_female_nd"
tbl1_anonym_up <- tbl1_anonym_cat[,c("CI_female_nd_up", "Variable")]
tbl1_anonym_up$bound = "up"
names(tbl1_anonym_up)[names(tbl1_anonym_up) == "CI_female_nd_up"] <- "CI_bound_female_nd"
tbl1_anonym_female_nd <- rbind(tbl1_anonym_low, tbl1_anonym_up)
tbl1_anonym_2_cat <- tbl1_anonym_2 %>% subset(Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & Variable != "BL_ku_map" & 
                                                Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
                                                Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "male")
tbl1_anonym_2_cat <- subset(tbl1_anonym_2_cat, !(endsWith(Variable, '_no')))
tbl1_anonym_2_cat$Variable <- factor(tbl1_anonym_2_cat$Variable, levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                            "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                            "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                            "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                            "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                            "BL_med_raas_ace_yes", "hospital_yes", "smoking_never", 
                                                                            "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                            "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                            "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                            "aa_stroke_yes", "female")) 
tbl1_anonym_2_low <- tbl1_anonym_2_cat[,c("CI_female_nd_low", "Variable")]
tbl1_anonym_2_low$bound = "low"
names(tbl1_anonym_2_low)[names(tbl1_anonym_2_low) == "CI_female_nd_low"] <- "CI_bound_female_nd"
tbl1_anonym_2_up <- tbl1_anonym_2_cat[,c("CI_female_nd_up", "Variable")]
tbl1_anonym_2_up$bound = "up"
names(tbl1_anonym_2_up)[names(tbl1_anonym_2_up) == "CI_female_nd_up"] <- "CI_bound_female_nd"
tbl1_anonym_2_female_nd <- rbind(tbl1_anonym_2_low, tbl1_anonym_2_up)
tbl1_anonym_3_cat <- tbl1_anonym_3 %>% subset(Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & Variable != "BL_ku_map" & 
                                                Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
                                                Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "male")
tbl1_anonym_3_cat <- subset(tbl1_anonym_3_cat, !(endsWith(Variable, '_no')))
tbl1_anonym_3_cat$Variable <- factor(tbl1_anonym_3_cat$Variable, levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                            "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                            "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                            "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                            "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                            "BL_med_raas_ace_yes", "hospital_yes", "smoking_never", 
                                                                            "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                            "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                            "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                            "aa_stroke_yes", "female")) 
tbl1_anonym_3_low <- tbl1_anonym_3_cat[,c("CI_female_nd_low", "Variable")]
tbl1_anonym_3_low$bound = "low"
names(tbl1_anonym_3_low)[names(tbl1_anonym_3_low) == "CI_female_nd_low"] <- "CI_bound_female_nd"
tbl1_anonym_3_up <- tbl1_anonym_3_cat[,c("CI_female_nd_up", "Variable")]
tbl1_anonym_3_up$bound = "up"
names(tbl1_anonym_3_up)[names(tbl1_anonym_3_up) == "CI_female_nd_up"] <- "CI_bound_female_nd"
tbl1_anonym_3_female_nd <- rbind(tbl1_anonym_3_low, tbl1_anonym_3_up)
tbl1_anonym_4_cat <- tbl1_anonym_4 %>% subset(Variable != "BL_ku_sys" & Variable != "BL_ku_dia" & Variable != "BL_ku_map" & 
                                                Variable != "BL_ku_ruhepuls" & Variable != "BL_creavalue" & 
                                                Variable != "BL_cysvalue" & Variable != "BL_gfr_mdrd" & Variable != "male")
tbl1_anonym_4_cat <- subset(tbl1_anonym_4_cat, !(endsWith(Variable, '_no')))
tbl1_anonym_4_cat$Variable <- factor(tbl1_anonym_4_cat$Variable, levels = c("biopsy_yes", "BL_med_bblocker_yes", 
                                                                            "BL_med_caanta_yes", "BL_med_diuretic_loop_yes",
                                                                            "BL_med_diuretic_aldost_yes","BL_med_diuretic_thiazid_yes", 
                                                                            "BL_med_diuretic_yes", "BL_med_raas_single_yes", 
                                                                            "BL_med_raas_double_yes", "BL_med_raas_at1_yes", 
                                                                            "BL_med_raas_ace_yes", "hospital_yes", "smoking_never", 
                                                                            "smoking_former", "smoking_current", "aa_ntx_yes", 
                                                                            "aa_dialyse_yes", "aa_renal_stones_yes", "aa_renal_yes",
                                                                            "aa_diabetes_yes", "aa_hypertens_yes", "aa_myocard_yes", 
                                                                            "aa_stroke_yes", "female")) 
tbl1_anonym_4_low <- tbl1_anonym_4_cat[,c("CI_female_nd_low", "Variable")]
tbl1_anonym_4_low$bound = "low"
names(tbl1_anonym_4_low)[names(tbl1_anonym_4_low) == "CI_female_nd_low"] <- "CI_bound_female_nd"
tbl1_anonym_4_up <- tbl1_anonym_4_cat[,c("CI_female_nd_up", "Variable")]
tbl1_anonym_4_up$bound = "up"
names(tbl1_anonym_4_up)[names(tbl1_anonym_4_up) == "CI_female_nd_up"] <- "CI_bound_female_nd"
tbl1_anonym_4_female_nd <- rbind(tbl1_anonym_4_low, tbl1_anonym_4_up)
ggplot() + 
  geom_line(data=tbl1_anonym_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), size=0.5, color = "indianred4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), shape="|", size=3, color = "indianred4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_cat, aes(x = perc_female_nd, y = Variable, group = Variable), size=2, color = "indianred4", position = position_nudge(y = 0.2)) + 
  geom_line(data=tbl1_anonym_2_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), size=0.5, color = "gold", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_2_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), shape="|", size=3, color = "gold", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_2_cat, aes(x = perc_female_nd, y = Variable, group = Variable), size=2, color = "gold", position = position_nudge(y = 0.4)) + 
  geom_line(data=tbl1_anonym_3_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), size=0.5, color = "darkseagreen3", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_3_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen3", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_3_cat, aes(x = perc_female_nd, y = Variable, group = Variable), size=2, color = "darkseagreen3", position = position_nudge(y = -0.2)) + 
  geom_line(data=tbl1_anonym_4_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), size=0.5, color = "lightblue4", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_4_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), shape="|", size=3, color = "lightblue4", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_4_cat, aes(x = perc_female_nd, y = Variable, group = Variable), size=2, color = "lightblue4", position = position_nudge(y = -0.4)) +
  geom_line(data=tbl1_origin_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_cat, aes(x = perc_female_nd, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) 
ggplot() + 
  geom_line(data=tbl1_anonym_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), size=0.5, color = "indianred4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), shape="|", size=3, color = "indianred4", position = position_nudge(y = 0.2)) +
  geom_point(data=tbl1_anonym_cat, aes(x = perc_female_nd, y = Variable, group = Variable), size=2, color = "indianred4", position = position_nudge(y = 0.2)) + 
  geom_line(data=tbl1_anonym_2_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), size=0.5, color = "gold", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_2_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), shape="|", size=3, color = "gold", position = position_nudge(y = 0.4)) +
  geom_point(data=tbl1_anonym_2_cat, aes(x = perc_female_nd, y = Variable, group = Variable), size=2, color = "gold", position = position_nudge(y = 0.4)) + 
  geom_line(data=tbl1_anonym_3_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), size=0.5, color = "darkseagreen3", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_3_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), shape="|", size=3, color = "darkseagreen3", position = position_nudge(y = -0.2)) +
  geom_point(data=tbl1_anonym_3_cat, aes(x = perc_female_nd, y = Variable, group = Variable), size=2, color = "darkseagreen3", position = position_nudge(y = -0.2)) + 
  geom_line(data=tbl1_anonym_4_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), size=0.5, color = "lightblue4", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_4_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), shape="|", size=3, color = "lightblue4", position = position_nudge(y = -0.4)) +
  geom_point(data=tbl1_anonym_4_cat, aes(x = perc_female_nd, y = Variable, group = Variable), size=2, color = "lightblue4", position = position_nudge(y = -0.4)) +
  geom_line(data=tbl1_origin_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), size=0.5, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_female_nd, aes(x = CI_bound_female_nd, y = Variable, group = Variable), shape="|", size=3, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) +
  geom_point(data=tbl1_origin_cat, aes(x = perc_female_nd, y = Variable, group = Variable), size=2, alpha = 0.7, color = "azure4", position = position_nudge(y = 0)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# Tbl 2
# Preprocessing
## Separating 95% CI bounds
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
## Rounding 1 decimal
tbl2_origin <- tbl2_origin %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl2_anonym <- tbl2_anonym %>% mutate(across(where(is.numeric), ~round(., 1)))
# Calculations
## estimate within 95% CI
tbl2_est <- data.frame(NA_col = rep(NA, 52))
res_egfr = numeric(52)
res_prot = numeric(52)
for(i in 1:nrow((tbl2_anonym))) {
  var = tbl2_origin$Variable[i]
  res_egfr[i] <- estimate_CI(tbl2_origin$CI_egfr_low[i], tbl2_origin$CI_egfr_up[i], tbl2_anonym$perc_egfr[i])
  tbl2_est[, 1] <- res_egfr
  res_prot[i] <- estimate_CI(tbl2_origin$CI_prot_low[i], tbl2_origin$CI_prot_up[i], tbl2_anonym$perc_prot[i])
  tbl2_est[, 2] <- res_prot
  rownames(tbl2_est)[i] <- paste0(var)
  colnames(tbl2_est)[1] <- paste0("Estimate_egfr")
  colnames(tbl2_est)[2] <- paste0("Estimate_prot")
}
tbl2_est$Variable <- row.names(tbl2_est)
write.xlsx(tbl2_est, "GCKD_results_est_tbl2_specific_k11k2.xlsx")
## overlapping 95% CI
tbl2_ci <- data.frame(NA_col = rep(NA, 52))
res_ci_egfr = numeric(52)
res_ci_prot = numeric(52)
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
write.xlsx(tbl2_ci, "GCKD_results_ci_tbl2_specific_k11k2.xlsx")

# Tbl 3
# Preprocessing
## Separating 95% CI bounds
tbl3_origin[c("CI_biopsy_low", "CI_biopsy_up")] <- str_split_fixed(tbl3_origin$CI_biopsy, "-", 2)
tbl3_origin$CI_biopsy_low <- as.numeric(tbl3_origin$CI_biopsy_low)
tbl3_origin$CI_biopsy_up <- as.numeric(tbl3_origin$CI_biopsy_up)
tbl3_anonym[c("CI_biopsy_low", "CI_biopsy_up")] <- str_split_fixed(tbl3_anonym$CI_biopsy, "-", 2)
tbl3_anonym$CI_biopsy_low <- as.numeric(tbl3_anonym$CI_biopsy_low)
tbl3_anonym$CI_biopsy_up <- as.numeric(tbl3_anonym$CI_biopsy_up)
## Rounding 1 decimal
tbl3_origin <- tbl3_origin %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl3_anonym <- tbl3_anonym %>% mutate(across(where(is.numeric), ~round(., 1)))
# Calculations
## estimate within 95% CI
tbl3_est <- data.frame(NA_col = rep(NA, 52))
res_biopsy = numeric(52)
for(i in 1:nrow((tbl3_anonym))) {
  var = tbl3_origin$Variable[i]
  res_biopsy[i] <- estimate_CI(tbl3_origin$CI_biopsy_low[i], tbl3_origin$CI_biopsy_up[i], tbl3_anonym$perc_biopsy[i])
  tbl3_est[, 1] <- res_biopsy
  rownames(tbl3_est)[i] <- paste0(var)
  colnames(tbl3_est)[1] <- paste0("Estimate_biopsy")
}
tbl3_est$Variable <- row.names(tbl3_est)
write.xlsx(tbl3_est, "GCKD_results_est_tbl3_specific_k11k2.xlsx")
## overlapping 95% CI
tbl3_ci <- data.frame(NA_col = rep(NA, 52))
res_CI_biopsy = numeric(52)
for(i in 1:nrow((tbl3_anonym))) {
  var = tbl3_origin$Variable[i]
  res_CI_biopsy[i] <- CI_overlap(tbl3_origin$CI_biopsy_low[i], tbl3_origin$CI_biopsy_up[i], tbl3_anonym$CI_biopsy_low[i], tbl3_anonym$CI_biopsy_up[i])
  tbl3_ci[, 1] <- res_CI_biopsy
  rownames(tbl3_ci)[i] <- paste0(var)
  colnames(tbl3_ci)[1] <- paste0("CI_overlap_biopsy")
}
tbl3_ci$Variable <- row.names(tbl3_ci)
write.xlsx(tbl3_ci, "GCKD_results_ci_tbl3_specific_k11k2.xlsx")

# Tbl 4
# Preprocessing
## Separating 95% CI bounds
tbl4_origin[c("CI_total_low", "CI_total_up")] <- str_split_fixed(tbl4_origin$CI_total, "-", 2)
tbl4_origin$CI_total_low <- as.numeric(tbl4_origin$CI_total_low)
tbl4_origin$CI_total_up <- as.numeric(tbl4_origin$CI_total_up)
tbl4_anonym[c("CI_total_low", "CI_total_up")] <- str_split_fixed(tbl4_anonym$CI_total, "-", 2)
tbl4_anonym$CI_total_low <- as.numeric(tbl4_anonym$CI_total_low)
tbl4_anonym$CI_total_up <- as.numeric(tbl4_anonym$CI_total_up)
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
## Rounding 1 decimal
tbl4_origin <- tbl4_origin %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl4_anonym <- tbl4_anonym %>% mutate(across(where(is.numeric), ~round(., 1)))
# Calculations
## estimate within 95% CI
tbl4_est <- data.frame(NA_col = rep(NA, 52))
res_male_d = numeric(52)
res_male_nd = numeric(52)
res_female_d = numeric(52)
res_female_nd = numeric(52)
res = numeric(52)
for(i in 1:nrow((tbl4_anonym))) {
  var = tbl4_origin$Variable[i]
  res_male_d[i] <- estimate_CI(tbl4_origin$CI_male_d_low[i], tbl4_origin$CI_male_d_up[i], tbl4_anonym$perc_male_d[i])
  tbl4_est[, 1] <- res_male_d
  res_male_nd[i] <- estimate_CI(tbl4_origin$CI_male_nd_low[i], tbl4_origin$CI_male_nd_up[i], tbl4_anonym$perc_male_nd[i])
  tbl4_est[, 2] <- res_male_nd
  res_female_d[i] <- estimate_CI(tbl4_origin$CI_female_d_low[i], tbl4_origin$CI_female_d_up[i], tbl4_anonym$perc_female_d[i])
  tbl4_est[, 3] <- res_female_d
  res_female_nd[i] <- estimate_CI(tbl4_origin$CI_female_nd_low[i], tbl4_origin$CI_female_nd_up[i], tbl4_anonym$perc_female_nd[i])
  tbl4_est[, 4] <- res_female_nd
  res[i] <- estimate_CI(tbl4_origin$CI_total_low[i], tbl4_origin$CI_total_up[i], tbl4_anonym$perc_total[i]) 
  tbl4_est[, 5] <- res
  rownames(tbl4_est)[i] <- paste0(var)
  colnames(tbl4_est)[1] <- paste0("Estimate_male_d")
  colnames(tbl4_est)[2] <- paste0("Estimate_male_nd")
  colnames(tbl4_est)[3] <- paste0("Estimate_female_d")
  colnames(tbl4_est)[4] <- paste0("Estimate_female_nd")
  colnames(tbl4_est)[5] <- paste0("Estimate_total")
}
tbl4_est$Variable <- row.names(tbl4_est)
write.xlsx(tbl4_est, "GCKD_results_est_tbl4_specific_k11k2.xlsx")
## overlapping 95% CI
tbl4_ci <- data.frame(NA_col = rep(NA, 52))
res_ci_male_d = numeric(52)
res_ci_male_nd = numeric(52)
res_ci_female_d = numeric(52)
res_ci_female_nd = numeric(52)
res_ci = numeric(52)
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
  res_ci[i] <- CI_overlap(tbl4_origin$CI_total_low[i], tbl4_origin$CI_total_up[i], tbl4_anonym$CI_total_low[i], tbl4_anonym$CI_total_up[i]) 
  tbl4_ci[, 5] <- res_ci
  rownames(tbl4_ci)[i] <- paste0(var)
  colnames(tbl4_ci)[1] <- paste0("CI_overlap_male_d")
  colnames(tbl4_ci)[2] <- paste0("CI_overlap_male_nd")
  colnames(tbl4_ci)[3] <- paste0("CI_overlap_female_d")
  colnames(tbl4_ci)[4] <- paste0("CI_overlap_female_nd")
  colnames(tbl4_ci)[5] <- paste0("CI_overlap_total")
}
tbl4_ci$Variable <- row.names(tbl4_ci)
write.xlsx(tbl4_ci, "GCKD_results_ci_tbl4_specific_k11k2.xlsx")

# Tbl S3
# Preprocessing
## Separating 95% CI bounds
tbls3_origin[c("CI_dmwdn_low", "CI_dmwdn_up")] <- str_split_fixed(tbls3_origin$CI_dmwdn, "-", 2)
tbls3_origin$CI_dmwdn_low <- as.numeric(tbls3_origin$CI_dmwdn_low)
tbls3_origin$CI_dmwdn_up <- as.numeric(tbls3_origin$CI_dmwdn_up)
tbls3_anonym[c("CI_dmwdn_low", "CI_dmwdn_up")] <- str_split_fixed(tbls3_anonym$CI_dmwdn, "-", 2)
tbls3_anonym$CI_dmwdn_low <- as.numeric(tbls3_anonym$CI_dmwdn_low)
tbls3_anonym$CI_dmwdn_up <- as.numeric(tbls3_anonym$CI_dmwdn_up)
tbls3_origin[c("CI_dmwodn_low", "CI_dmwodn_up")] <- str_split_fixed(tbls3_origin$CI_dmwodn, "-", 2)
tbls3_origin$CI_dmwodn_low <- as.numeric(tbls3_origin$CI_dmwodn_low)
tbls3_origin$CI_dmwodn_up <- as.numeric(tbls3_origin$CI_dmwodn_up)
tbls3_anonym[c("CI_dmwodn_low", "CI_dmwodn_up")] <- str_split_fixed(tbls3_anonym$CI_dmwodn, "-", 2)
tbls3_anonym$CI_dmwodn_low <- as.numeric(tbls3_anonym$CI_dmwodn_low)
tbls3_anonym$CI_dmwodn_up <- as.numeric(tbls3_anonym$CI_dmwodn_up)
tbls3_origin[c("CI_nodm_low", "CI_nodm_up")] <- str_split_fixed(tbls3_origin$CI_nodm, "-", 2)
tbls3_origin$CI_nodm_low <- as.numeric(tbls3_origin$CI_nodm_low)
tbls3_origin$CI_nodm_up <- as.numeric(tbls3_origin$CI_nodm_up)
tbls3_anonym[c("CI_nodm_low", "CI_nodm_up")] <- str_split_fixed(tbls3_anonym$CI_nodm, "-", 2)
tbls3_anonym$CI_nodm_low <- as.numeric(tbls3_anonym$CI_nodm_low)
tbls3_anonym$CI_nodm_up <- as.numeric(tbls3_anonym$CI_nodm_up)
## Rounding 1 decimal
tbls3_origin <- tbls3_origin %>% mutate(across(where(is.numeric), ~round(., 1)))
tbls3_anonym <- tbls3_anonym %>% mutate(across(where(is.numeric), ~round(., 1)))
# Calculations
## estimate within 95% CI
tbls3_est <- data.frame(NA_col = rep(NA, 52))
res_dmwodn = numeric(52)
res_nodm = numeric(52)
res = numeric(52)
for(i in 1:nrow((tbls3_anonym))) {
  var = tbls3_origin$Variable[i]
  res_dmwodn[i] <- estimate_CI(tbls3_origin$CI_dmwodn_low[i], tbls3_origin$CI_dmwodn_up[i], tbls3_anonym$perc_dmwodn[i])
  tbls3_est[, 1] <- res_dmwodn
  res_nodm[i] <- estimate_CI(tbls3_origin$CI_nodm_low[i], tbls3_origin$CI_nodm_up[i], tbls3_anonym$perc_nodm[i])
  tbls3_est[, 2] <- res_nodm
  res[i] <- estimate_CI(tbls3_origin$CI_dmwdn_low[i], tbls3_origin$CI_dmwdn_up[i], tbls3_anonym$perc_dmwdn[i]) 
  tbls3_est[, 3] <- res
  rownames(tbls3_est)[i] <- paste0(var)
  colnames(tbls3_est)[1] <- paste0("Estimate_dmwodn")
  colnames(tbls3_est)[2] <- paste0("Estimate_nodm")
  colnames(tbls3_est)[3] <- paste0("Estimate_dmwdn")
}
tbls3_est$Variable <- row.names(tbls3_est)
write.xlsx(tbls3_est, "GCKD_results_est_tbls3_specific_k11k2.xlsx")
## overlapping 95% CI
tbls3_ci <- data.frame(NA_col = rep(NA, 52))
res_ci_dmwodn = numeric(52)
res_ci_nodm = numeric(52)
res_ci = numeric(52)
for(i in 1:nrow((tbls3_anonym))) {
  var = tbls3_origin$Variable[i]
  res_ci_dmwodn[i] <- CI_overlap(tbls3_origin$CI_dmwodn_low[i], tbls3_origin$CI_dmwodn_up[i], tbls3_anonym$CI_dmwodn_low[i], tbls3_anonym$CI_dmwodn_up[i])
  tbls3_ci[, 1] <- res_ci_dmwodn
  res_ci_nodm[i] <- CI_overlap(tbls3_origin$CI_nodm_low[i], tbls3_origin$CI_nodm_up[i], tbls3_anonym$CI_nodm_low[i], tbls3_anonym$CI_nodm_up[i])
  tbls3_ci[, 2] <- res_ci_nodm
  res_ci[i] <- CI_overlap(tbls3_origin$CI_dmwdn_low[i], tbls3_origin$CI_dmwdn_up[i], tbls3_anonym$CI_dmwdn_low[i], tbls3_anonym$CI_dmwdn_up[i]) 
  tbls3_ci[, 3] <- res_ci
  rownames(tbls3_ci)[i] <- paste0(var)
  colnames(tbls3_ci)[1] <- paste0("CI_overlap_dmwodn")
  colnames(tbls3_ci)[2] <- paste0("CI_overlap_nodm")
  colnames(tbls3_ci)[3] <- paste0("CI_overlap_dmwdn")
}
tbls3_ci$Variable <- row.names(tbls3_ci)
write.xlsx(tbls3_ci, "GCKD_results_ci_tbls3_specific_k11k2.xlsx")

# Tbl 1 
# Illustration of scale transformations
setwd(path_data)
GCKD_df1_o <- as_tibble(read.xlsx("GCKD_df1_origin.xlsx", sep = ";"))
GCKD_df1 <- as_tibble(read.xlsx("GCKD_generic_k11.xlsx", sep = ";"))
GCKD_df2 <- as_tibble(read.xlsx("GCKD_generic_k11k2.xlsx", sep = ";"))
GCKD_df3 <- as_tibble(read.xlsx("GCKD_specific_k11.xlsx", sep = ";"))
GCKD_df4 <- as_tibble(read.xlsx("GCKD_specific_k11k2.xlsx", sep = ";"))
# Preprocessing
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
GCKD_df4 <- GCKD_df4 %>% na_if("*")
GCKD_df4 <- GCKD_df4 %>% na_if("NULL")
GCKD_df4 <- GCKD_df4 %>% na_if("null")
## Data Type
col_cat <- c("aa_stroke", "aa_myocard", "aa_hypertens", "aa_diabetes", "aa_renal", "aa_renal_stones", "aa_dialyse", 
             "aa_ntx", "smoking", "hospital", "BL_med_raas_ace", "BL_med_raas_at1", "BL_med_raas_single", 
             "BL_med_raas_double", "BL_med_caanta", "BL_med_bblocker", "BL_med_diuretic", "BL_med_diuretic_loop", 
             "BL_med_diuretic_thiazid", "BL_med_diuretic_aldost", "biopsy", "ckd_diab", "ckd_vasc", "ckd_syst", 
             "ckd_glom_prim", "ckd_interst", "ckd_heredit", "ckd_aki", "ckd_single", "ckd_obstr", "ckd_oth", 
             "ckd_uk", "diabetes", "hypertension", "valve", "coronary", "myocard", "bypass", "ptca", "cerebrovasc", 
             "stroke", "carotic_surg", "carotic_interv", "pavk", "amput", "ygraft", "pta", "cardiovasc", 
             "pavk_surgery", "incl_egfr", "education")
col_num <- c("BL_ku_sys", "BL_ku_dia", "BL_ku_map", "BL_ku_ruhepuls", "BL_creavalue", "BL_cysvalue", 
             "BL_gfr_mdrd", "BL_gfr_ckdepi", "BL_gfr_ckdepi_cys", "BL_gfr_ckdepi_creacys", "BL_uacr")
GCKD_df1_o <- GCKD_df1_o %>% mutate(across(all_of(col_cat), as.character))
GCKD_df1_o <- GCKD_df1_o %>% mutate(across(all_of(col_num), as.numeric))
GCKD_df1 <- GCKD_df1 %>% mutate(across(all_of(col_cat), as.character))
GCKD_df1 <- GCKD_df1 %>% mutate(across(all_of(col_num), as.numeric))
GCKD_df2 <- GCKD_df2 %>% mutate(across(all_of(col_cat), as.character))
GCKD_df2 <- GCKD_df2 %>% mutate(across(all_of(col_num), as.numeric))
GCKD_df3 <- GCKD_df3 %>% mutate(across(all_of(col_cat), as.character))
GCKD_df3 <- GCKD_df3 %>% mutate(across(all_of(col_num), as.numeric))
GCKD_df4 <- GCKD_df4 %>% mutate(across(all_of(col_cat), as.character))
GCKD_df4 <- GCKD_df4 %>% mutate(across(all_of(col_num), as.numeric))
## Subset non diabetic females
GCKD_df1_o_femnd <- GCKD_df1_o %>% subset(dem_sex == "Female" & diabetes == "2")
GCKD_df1_femnd <- GCKD_df1 %>% subset(dem_sex == "Female" & diabetes == "2")
GCKD_df2_femnd <- GCKD_df2 %>% subset(dem_sex == "Female" & diabetes == "2")
GCKD_df3_femnd <- GCKD_df3 %>% subset(dem_sex == "Female" & diabetes == "2")
GCKD_df4_femnd <- GCKD_df4 %>% subset(dem_sex == "Female" & diabetes == "2")
## age
ggplot() + 
  geom_histogram(data = GCKD_df1_o_femnd, aes(x = BL_age), binwidth=1, colour="white", fill="azure4")
ggplot() + 
  geom_density(data = GCKD_df1_o_femnd, aes(x = BL_age), colour="white", alpha = 0.5, fill="azure4")
ggplot() +
  geom_bar(data = subset(GCKD_df1_femnd, !is.na(GCKD_df1_femnd$BL_age)), aes(x = BL_age), colour="white", fill="indianred4")
ggplot() +
  geom_bar(data = subset(GCKD_df2_femnd, !is.na(GCKD_df2_femnd$BL_age)), aes(x = BL_age), colour="white", fill="indianred")
ggplot() +
  geom_bar(data = subset(GCKD_df3_femnd, !is.na(GCKD_df3_femnd$BL_age)), aes(x = BL_age), colour="white", fill="lightblue4")
ggplot() +
  geom_bar(data = subset(GCKD_df4_femnd, !is.na(GCKD_df4_femnd$BL_age)), aes(x = BL_age), colour="white", fill= "lightblue3")
GCKD_df1_mut <- GCKD_df1_femnd
GCKD_df1_mut$BL_age <- mapvalues(GCKD_df1_mut$BL_age, from = c("[20, 30[", "[30, 40[", "[40, 50[", "[50, 60[", "[60, 70[", "[70, 80["), to = c("25", "35", "45", "55", "65", "75"))
GCKD_df1_mut$BL_age <- as.numeric(GCKD_df1_mut$BL_age)
GCKD_df2_mut <- GCKD_df2_femnd
GCKD_df2_mut$BL_age <- mapvalues(GCKD_df2_mut$BL_age, from = c("[20, 25[", "[25, 30[", "[30, 35[", "[35, 40[", "[40, 45[", "[45, 50[", "[50, 55[", "[55, 60[", "[60, 65[", "[65, 70[", "[70, 75[", "[75, 80["), to = c("22.5", "27.5", "32.5", "37.5", "42.5", "47.5", "52.5", "57.5", "62.5", "67.5", "72.5", "77.5"))
GCKD_df2_mut$BL_age <- as.numeric(GCKD_df2_mut$BL_age)
GCKD_df3_mut <- GCKD_df3_femnd
GCKD_df3_mut$BL_age <- mapvalues(GCKD_df3_mut$BL_age, from = c("[20, 30[", "[30, 40[", "[40, 50[", "[50, 60[", "[60, 70[", "[70, 80["), to = c("25", "35", "45", "55", "65", "75"))
GCKD_df3_mut$BL_age <- as.numeric(GCKD_df3_mut$BL_age)
GCKD_df4_mut <- GCKD_df4_femnd
GCKD_df4_mut$BL_age <- mapvalues(GCKD_df4_mut$BL_age, from = c("[20, 25[", "[25, 30[", "[30, 35[", "[35, 40[", "[40, 45[", "[45, 50[", "[50, 55[", "[55, 60[", "[60, 65[", "[65, 70[", "[70, 75["), to = c("22.5", "27.5", "32.5", "37.5", "42.5", "47.5", "52.5", "57.5", "62.5", "67.5", "72.5"))
GCKD_df4_mut$BL_age <- as.numeric(GCKD_df4_mut$BL_age)
ggplot() + 
  geom_density(data = GCKD_df1_o, aes(BL_age, y = ..density..*10000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(GCKD_df2_mut, !is.na(GCKD_df2_mut$BL_age)), aes(BL_age), colour="gold", width = 5, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, !is.na(GCKD_df3_mut$BL_age)), aes(BL_age), colour="darkseagreen3", width = 10, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_mut, !is.na(GCKD_df1_mut$BL_age)), aes(BL_age), colour="indianred4", width = 10, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_mut, !is.na(GCKD_df4_mut$BL_age)), aes(BL_age), colour="lightblue4", width = 5, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1)
ggplot() + 
  geom_density(data = GCKD_df1_o, aes(BL_age, y = ..density..*10000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(GCKD_df2_mut, !is.na(GCKD_df2_mut$BL_age)), aes(BL_age), colour="gold", width = 5, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, !is.na(GCKD_df3_mut$BL_age)), aes(BL_age), colour="darkseagreen3", width = 10, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_mut, !is.na(GCKD_df1_mut$BL_age)), aes(BL_age), colour="indianred4", width = 10, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_mut, !is.na(GCKD_df4_mut$BL_age)), aes(BL_age), colour="lightblue4", width = 5, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
## Weight
ggplot() + 
  geom_histogram(data = GCKD_df1_o_femnd, aes(x = BL_ku_weight), binwidth=1, colour="white", fill="azure4")
ggplot() + 
  geom_density(data = GCKD_df1_o_femnd, aes(x = BL_ku_weight), colour="white", alpha = 0.5, fill="azure4")
ggplot() +
  geom_bar(data = subset(GCKD_df1_femnd, !is.na(GCKD_df1_femnd$BL_ku_weight)), aes(x = BL_ku_weight), colour="white", fill="indianred4")
ggplot() +
  geom_bar(data = subset(GCKD_df2_femnd, !is.na(GCKD_df2_femnd$BL_ku_weight)), aes(x = BL_ku_weight), colour="white", fill="indianred")
ggplot() +
  geom_bar(data = subset(GCKD_df3_femnd, !is.na(GCKD_df3_femnd$BL_ku_weight)), aes(x = BL_ku_weight), colour="white", fill="darkseagreen4")
ggplot() +
  geom_bar(data = subset(GCKD_df4_femnd, !is.na(GCKD_df4_femnd$BL_ku_weight)), aes(x = BL_ku_weight), colour="white", fill= "darkseagreen")
GCKD_df1_mut <- GCKD_df1_femnd
GCKD_df1_mut$BL_ku_weight <- mapvalues(GCKD_df1_mut$BL_ku_weight, from = c("[40, 80[", "[80, 120[", "[120, 160["), to = c("60", "100", "140"))
GCKD_df1_mut$BL_ku_weight <- as.numeric(GCKD_df1_mut$BL_ku_weight)
GCKD_df2_mut <- GCKD_df2_femnd
GCKD_df2_mut$BL_ku_weight <- mapvalues(GCKD_df2_mut$BL_ku_weight, from = c("[40, 60[", "[60, 80[", "[80, 100[", "[100, 120[", "[120, 140["), to = c("50", "70", "90", "110", "130"))
GCKD_df2_mut$BL_ku_weight <- as.numeric(GCKD_df2_mut$BL_ku_weight)
GCKD_df3_mut <- GCKD_df3_femnd
GCKD_df3_mut$BL_ku_weight <- mapvalues(GCKD_df3_mut$BL_ku_weight, from = c("[40, 80[", "[80, 120[", "[120, 160["), to = c("60", "100", "140"))
GCKD_df3_mut$BL_ku_weight <- as.numeric(GCKD_df3_mut$BL_ku_weight)
GCKD_df4_mut <- GCKD_df4_femnd
GCKD_df4_mut$BL_ku_weight <- mapvalues(GCKD_df4_mut$BL_ku_weight, from = c("[40, 50[", "[50, 60[", "[60, 70[", "[70, 80[", "[80, 90[", "[90, 100[", "[100, 110[", "[110, 120[", "[120, 130[", "[130, 140["), to = c("45", "55", "65", "75", "85", "95", "105", "115", "125", "135"))
GCKD_df4_mut$BL_ku_weight <- as.numeric(GCKD_df4_mut$BL_ku_weight)
ggplot() + 
  geom_density(data = GCKD_df1_o, aes(BL_ku_weight, y = ..density..*50000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(GCKD_df2_mut, !is.na(GCKD_df2_mut$BL_ku_weight)), aes(BL_ku_weight), colour="gold", width = 20, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, !is.na(GCKD_df3_mut$BL_ku_weight)), aes(BL_ku_weight), colour="darkseagreen3", width = 40, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_mut, !is.na(GCKD_df1_mut$BL_ku_weight)), aes(BL_ku_weight), colour="indianred4", width = 40, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_mut, !is.na(GCKD_df4_mut$BL_ku_weight)), aes(BL_ku_weight), colour="lightblue4", width = 10, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(limits = c(40, 180), sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1)
ggplot() + 
  geom_density(data = GCKD_df1_o, aes(BL_ku_weight, y = ..density..*50000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(GCKD_df2_mut, !is.na(GCKD_df2_mut$BL_ku_weight)), aes(BL_ku_weight), colour="gold", width = 20, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, !is.na(GCKD_df3_mut$BL_ku_weight)), aes(BL_ku_weight), colour="darkseagreen3", width = 40, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_mut, !is.na(GCKD_df1_mut$BL_ku_weight)), aes(BL_ku_weight), colour="indianred4", width = 40, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_mut, !is.na(GCKD_df4_mut$BL_ku_weight)), aes(BL_ku_weight), colour="lightblue4", width = 10, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(limits = c(40, 180), sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
## Height (for generic health data scenario)
ggplot() + 
  geom_histogram(data = GCKD_df1_o_femnd, aes(x = BL_ku_height_cm), binwidth=1, colour="white", fill="azure4")
ggplot() + 
  geom_density(data = GCKD_df1_o_femnd, aes(x = BL_ku_height_cm), colour="white", alpha = 0.5, fill="azure4")
ggplot() +
  geom_bar(data = subset(GCKD_df1_femnd, !is.na(GCKD_df1_femnd$BL_ku_height_cm)), aes(x = BL_ku_height_cm), colour="white", fill="indianred4")
ggplot() +
  geom_bar(data = subset(GCKD_df2_femnd, !is.na(GCKD_df2_femnd$BL_ku_height_cm)), aes(x = BL_ku_height_cm), colour="white", fill="indianred")
GCKD_df1_mut <- GCKD_df1_femnd
GCKD_df1_mut$BL_ku_height_cm <- mapvalues(GCKD_df1_mut$BL_ku_height_cm, from = c("[140, 160[", "[160, 180["), to = c("150", "170"))
GCKD_df1_mut$BL_ku_height_cm <- as.numeric(GCKD_df1_mut$BL_ku_height_cm)
GCKD_df2_mut <- GCKD_df2_femnd
GCKD_df2_mut$BL_ku_height_cm <- mapvalues(GCKD_df2_mut$BL_ku_height_cm, from = c("[140, 150[", "[150, 160[", "[160, 170[", "[170, 180[", "[180, 190["), to = c("145", "155", "165", "175", "185"))
GCKD_df2_mut$BL_ku_height_cm <- as.numeric(GCKD_df2_mut$BL_ku_height_cm)
ggplot() + 
  geom_density(data = GCKD_df1_o, aes(BL_ku_height_cm, y = ..density..*30000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(GCKD_df2_mut, !is.na(GCKD_df2_mut$BL_ku_height_cm)), aes(BL_ku_height_cm), colour="indianred", size = 1, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_mut, !is.na(GCKD_df1_mut$BL_ku_height_cm)), aes(BL_ku_height_cm), colour="indianred4", size = 1, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(limits = c(140, 200), sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1)
ggplot()+ 
  geom_density(data = GCKD_df1_o, aes(BL_ku_height_cm, y = ..density..*30000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(GCKD_df2_mut, !is.na(GCKD_df2_mut$BL_ku_height_cm)), aes(BL_ku_height_cm), colour="indianred", size = 1, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_mut, !is.na(GCKD_df1_mut$BL_ku_height_cm)), aes(BL_ku_height_cm), colour="indianred4", size = 1, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(limits = c(140, 200), sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### combined with calculated height from specific use case scenario
GCKD_df3_femnd[c("BMI_low", "BMI_up")] <- str_split_fixed(GCKD_df3_femnd$BL_ku_bmi, ", ", 2)
GCKD_df3_femnd$BMI_low <- gsub("^.", "", as.character(GCKD_df3_femnd$BMI_low))
GCKD_df3_femnd$BMI_low <- as.numeric(GCKD_df3_femnd$BMI_low)
GCKD_df3_femnd$BMI_up <- gsub(".$", "", as.character(GCKD_df3_femnd$BMI_up))
GCKD_df3_femnd$BMI_up <- as.numeric(GCKD_df3_femnd$BMI_up)
GCKD_df3_femnd[c("weight_low", "weight_up")] <- str_split_fixed(GCKD_df3_femnd$BL_ku_weight, ", ", 2)
GCKD_df3_femnd$weight_low <- gsub("^.", "", as.character(GCKD_df3_femnd$weight_low))
GCKD_df3_femnd$weight_low <- as.numeric(GCKD_df3_femnd$weight_low)
GCKD_df3_femnd$weight_up <- gsub(".$", "", as.character(GCKD_df3_femnd$weight_up))
GCKD_df3_femnd$weight_up <- as.numeric(GCKD_df3_femnd$weight_up)
GCKD_df3_femnd$height_cm_low_calc <- as.numeric(sqrt(GCKD_df3_femnd$weight_low/GCKD_df3_femnd$BMI_up)*100)
GCKD_df3_femnd$height_cm_low_calc <- round(GCKD_df3_femnd$height_cm_low_calc ,digit=1)
GCKD_df3_femnd$height_cm_up_calc <- as.numeric(sqrt(GCKD_df3_femnd$weight_up/GCKD_df3_femnd$BMI_low)*100)
GCKD_df3_femnd$height_cm_up_calc <- round(GCKD_df3_femnd$height_cm_up_calc ,digit=1)
GCKD_df3_femnd <- GCKD_df3_femnd %>% 
  mutate(BL_ku_height_cm_calc = height_cm_low_calc + ((height_cm_up_calc-height_cm_low_calc)/2))
GCKD_df3_femnd$BL_ku_height_cm_calc <- round(GCKD_df3_femnd$BL_ku_height_cm_calc, digit=2)
table(GCKD_df3_femnd$height_cm_low_calc, GCKD_df3_femnd$height_cm_up_calc, useNA = "always")
table(GCKD_df3_femnd$height_cm_low_calc, GCKD_df3_femnd$BL_ku_height_cm_calc, useNA = "always")
GCKD_df4_femnd[c("BMI_low", "BMI_up")] <- str_split_fixed(GCKD_df4_femnd$BL_ku_bmi, ", ", 2)
GCKD_df4_femnd$BMI_low <- gsub("^.", "", as.character(GCKD_df4_femnd$BMI_low))
GCKD_df4_femnd$BMI_low <- as.numeric(GCKD_df4_femnd$BMI_low)
GCKD_df4_femnd$BMI_up <- gsub(".$", "", as.character(GCKD_df4_femnd$BMI_up))
GCKD_df4_femnd$BMI_up <- as.numeric(GCKD_df4_femnd$BMI_up)
GCKD_df4_femnd[c("weight_low", "weight_up")] <- str_split_fixed(GCKD_df4_femnd$BL_ku_weight, ", ", 2)
GCKD_df4_femnd$weight_low <- gsub("^.", "", as.character(GCKD_df4_femnd$weight_low))
GCKD_df4_femnd$weight_low <- as.numeric(GCKD_df4_femnd$weight_low)
GCKD_df4_femnd$weight_up <- gsub(".$", "", as.character(GCKD_df4_femnd$weight_up))
GCKD_df4_femnd$weight_up <- as.numeric(GCKD_df4_femnd$weight_up)
GCKD_df4_femnd$height_cm_low_calc <- as.numeric(sqrt(GCKD_df4_femnd$weight_low/GCKD_df4_femnd$BMI_up)*100)
GCKD_df4_femnd$height_cm_low_calc <- round(GCKD_df4_femnd$height_cm_low_calc ,digit=1)
GCKD_df4_femnd$height_cm_up_calc <- as.numeric(sqrt(GCKD_df4_femnd$weight_up/GCKD_df4_femnd$BMI_low)*100)
GCKD_df4_femnd$height_cm_up_calc <- round(GCKD_df4_femnd$height_cm_up_calc ,digit=1)
GCKD_df4_femnd <- GCKD_df4_femnd %>% 
  mutate(BL_ku_height_cm_calc = height_cm_low_calc + ((height_cm_up_calc-height_cm_low_calc)/2))
table(GCKD_df4_femnd$height_cm_low_calc, GCKD_df4_femnd$height_cm_up_calc, useNA = "always")
table(GCKD_df4_femnd$height_cm_low_calc, GCKD_df4_femnd$BL_ku_height_cm_calc, useNA = "always")
GCKD_df4_femnd$BL_ku_height_cm_calc <- round(GCKD_df4_femnd$BL_ku_height_cm_calc, digit=2)
ggplot() + 
  geom_density(data = GCKD_df1_o, aes(BL_ku_height_cm, y = ..density..*30000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(GCKD_df2_mut, !is.na(GCKD_df2_mut$BL_ku_height_cm)), aes(BL_ku_height_cm), colour="gold", width = 10, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_mut, !is.na(GCKD_df1_mut$BL_ku_height_cm)), aes(BL_ku_height_cm), colour="indianred4", width = 20, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_femnd, GCKD_df3_femnd$BL_ku_height_cm_calc == 135.1), aes(BL_ku_height_cm_calc), colour="darkseagreen3", width = 55, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_femnd, GCKD_df3_femnd$BL_ku_height_cm_calc == 140.05), aes(BL_ku_height_cm_calc), colour="darkseagreen3", width = 65, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_femnd, GCKD_df3_femnd$BL_ku_height_cm_calc == 147.2), aes(BL_ku_height_cm_calc), colour="darkseagreen3", width = 65, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_femnd, GCKD_df3_femnd$BL_ku_height_cm_calc == 167.25), aes(BL_ku_height_cm_calc), colour="darkseagreen3", width = 83, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_femnd, GCKD_df3_femnd$BL_ku_height_cm_calc == 165.45), aes(BL_ku_height_cm_calc), colour="darkseagreen3", width = 69, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_femnd, GCKD_df3_femnd$BL_ku_height_cm_calc == 163.3), aes(BL_ku_height_cm_calc), colour="darkseagreen3", width = 45, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_femnd, GCKD_df3_femnd$BL_ku_height_cm_calc == 175.6), aes(BL_ku_height_cm_calc), colour="darkseagreen3", width = 50, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_femnd, GCKD_df3_femnd$BL_ku_height_cm_calc == 191.2), aes(BL_ku_height_cm_calc), colour="darkseagreen3", width = 47, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 135.75), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 45, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 142), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 28, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 142.65), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 43, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 145.45), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 39, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 149.3), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 49, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 150.9), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 23, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 152.35), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 26, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 154.35), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 30, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 155.6), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 53, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 159.5), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 19, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 160.75), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 43, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 161.7), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 51, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 162.2), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 22, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 165.85), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 28, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 167.7), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 20, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 171.5), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 23, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 174.7), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 40, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 175.5), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 20, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 176.5), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 26, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 180.25), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 17, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 186.6), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 27, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 187.65), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 37, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 199.75), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 40, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(limits = c(100, 220), sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1)
ggplot() + 
  geom_density(data = GCKD_df1_o, aes(BL_ku_height_cm, y = ..density..*30000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(GCKD_df2_mut, !is.na(GCKD_df2_mut$BL_ku_height_cm)), aes(BL_ku_height_cm), colour="gold", width = 10, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_mut, !is.na(GCKD_df1_mut$BL_ku_height_cm)), aes(BL_ku_height_cm), colour="indianred4", width = 20, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_femnd, GCKD_df3_femnd$BL_ku_height_cm_calc == 135.1), aes(BL_ku_height_cm_calc), colour="darkseagreen3", width = 55, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_femnd, GCKD_df3_femnd$BL_ku_height_cm_calc == 140.05), aes(BL_ku_height_cm_calc), colour="darkseagreen3", width = 65, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_femnd, GCKD_df3_femnd$BL_ku_height_cm_calc == 147.2), aes(BL_ku_height_cm_calc), colour="darkseagreen3", width = 65, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_femnd, GCKD_df3_femnd$BL_ku_height_cm_calc == 167.25), aes(BL_ku_height_cm_calc), colour="darkseagreen3", width = 83, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_femnd, GCKD_df3_femnd$BL_ku_height_cm_calc == 165.45), aes(BL_ku_height_cm_calc), colour="darkseagreen3", width = 69, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_femnd, GCKD_df3_femnd$BL_ku_height_cm_calc == 163.3), aes(BL_ku_height_cm_calc), colour="darkseagreen3", width = 45, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_femnd, GCKD_df3_femnd$BL_ku_height_cm_calc == 175.6), aes(BL_ku_height_cm_calc), colour="darkseagreen3", width = 50, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_femnd, GCKD_df3_femnd$BL_ku_height_cm_calc == 191.2), aes(BL_ku_height_cm_calc), colour="darkseagreen3", width = 47, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 135.75), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 45, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 142), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 28, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 142.65), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 43, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 145.45), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 39, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 149.3), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 49, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 150.9), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 23, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 152.35), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 26, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 154.35), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 30, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 155.6), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 53, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 159.5), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 19, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 160.75), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 43, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 161.7), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 51, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 162.2), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 22, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 165.85), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 28, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 167.7), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 20, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 171.5), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 23, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 174.7), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 40, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 175.5), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 20, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 176.5), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 26, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 180.25), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 17, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 186.6), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 27, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 187.65), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 37, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_femnd, GCKD_df4_femnd$BL_ku_height_cm_calc == 199.75), aes(BL_ku_height_cm_calc), colour="lightblue4", width = 40, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(limits = c(100, 220), sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

## BMI (for specific health data scenario)
ggplot() + 
  geom_histogram(data = GCKD_df1_o_femnd, aes(x = BL_ku_bmi), binwidth=1, colour="white", fill="azure4")
ggplot() + 
  geom_density(data = GCKD_df1_o_femnd, aes(x = BL_ku_bmi), colour="white", alpha = 0.5, fill="azure4")
ggplot() +
  geom_bar(data = subset(GCKD_df3_femnd, !is.na(GCKD_df3_femnd$BL_ku_bmi)), aes(x = BL_ku_bmi), colour="white", fill="darkseagreen4")
ggplot() +
  geom_bar(data = subset(GCKD_df4_femnd, !is.na(GCKD_df4_femnd$BL_ku_bmi)), aes(x = BL_ku_bmi), colour="white", fill="darkseagreen")
GCKD_df3_mut <- GCKD_df3_femnd
GCKD_df3_mut$BL_ku_bmi <- mapvalues(GCKD_df3_mut$BL_ku_bmi, from = c("[18.5, 25[", "[25, 30[", "[30, 35[", "[35, 40[", "[40, 70["), to = c("21.75", "27.5", "32.5", "37.5", "55"))
GCKD_df3_mut$BL_ku_bmi <- as.numeric(GCKD_df3_mut$BL_ku_bmi)
GCKD_df4_mut <- GCKD_df4_femnd
GCKD_df4_mut$BL_ku_bmi <- mapvalues(GCKD_df4_mut$BL_ku_bmi, from = c("[18.5, 25[", "[25, 30[", "[30, 35[", "[35, 40[", "[40, 70["), to = c("21.75", "27.5", "32.5", "37.5", "55"))
GCKD_df4_mut$BL_ku_bmi <- as.numeric(GCKD_df4_mut$BL_ku_bmi)
ggplot() + 
  geom_density(data = GCKD_df1_o, aes(BL_ku_bmi, y = ..density..*8000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(GCKD_df4_mut, !is.na(GCKD_df4_mut$BL_ku_bmi)), aes(BL_ku_bmi), colour="darkseagreen", size = 1, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, !is.na(GCKD_df3_mut$BL_ku_bmi)), aes(BL_ku_bmi), colour="darkseagreen4", size = 1, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(limits = c(15, 60), sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1)
ggplot()+ 
  geom_density(data = GCKD_df1_o, aes(BL_ku_bmi, y = ..density..*8000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(GCKD_df4_mut, !is.na(GCKD_df4_mut$BL_ku_bmi)), aes(BL_ku_bmi), colour="darkseagreen", size = 1, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, !is.na(GCKD_df3_mut$BL_ku_bmi)), aes(BL_ku_bmi), colour="darkseagreen4", size = 1, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(limits = c(15, 60), sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggplot() + 
  geom_density(data = GCKD_df1_o, aes(BL_ku_bmi, y = ..density..*8000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(GCKD_df4_mut, GCKD_df4_mut$BL_ku_bmi == 21.75), aes(BL_ku_bmi), colour="darkseagreen", width = 6.5, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_mut, GCKD_df4_mut$BL_ku_bmi == 55), aes(BL_ku_bmi), colour="darkseagreen", width = 30, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_mut, !is.na(GCKD_df4_mut$BL_ku_bmi) & GCKD_df4_mut$BL_ku_bmi != 21.75 & GCKD_df4_mut$BL_ku_bmi != 55), aes(BL_ku_bmi), colour="darkseagreen", width = 5, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, GCKD_df3_mut$BL_ku_bmi == 21.75), aes(BL_ku_bmi), colour="darkseagreen4", width = 6.5, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, GCKD_df3_mut$BL_ku_bmi == 55), aes(BL_ku_bmi), colour="darkseagreen4", width = 30, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, !is.na(GCKD_df3_mut$BL_ku_bmi) & GCKD_df3_mut$BL_ku_bmi != 21.75 & GCKD_df3_mut$BL_ku_bmi != 55), aes(BL_ku_bmi), colour="darkseagreen4", width = 5, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(limits = c(10, 70), sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1)
ggplot() + 
  geom_density(data = GCKD_df1_o, aes(BL_ku_bmi, y = ..density..*8000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(GCKD_df4_mut, GCKD_df4_mut$BL_ku_bmi == 21.75), aes(BL_ku_bmi), colour="darkseagreen", width = 6.5, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_mut, GCKD_df4_mut$BL_ku_bmi == 55), aes(BL_ku_bmi), colour="darkseagreen", width = 30, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_mut, !is.na(GCKD_df4_mut$BL_ku_bmi) & GCKD_df4_mut$BL_ku_bmi != 21.75 & GCKD_df4_mut$BL_ku_bmi != 55), aes(BL_ku_bmi), colour="darkseagreen", width = 5, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, GCKD_df3_mut$BL_ku_bmi == 21.75), aes(BL_ku_bmi), colour="darkseagreen4", width = 6.5, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, GCKD_df3_mut$BL_ku_bmi == 55), aes(BL_ku_bmi), colour="darkseagreen4", width = 30, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, !is.na(GCKD_df3_mut$BL_ku_bmi) & GCKD_df3_mut$BL_ku_bmi != 21.75 & GCKD_df3_mut$BL_ku_bmi != 55), aes(BL_ku_bmi), colour="darkseagreen4", width = 5, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(limits = c(10, 70), sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

### combined with calculated BMI from generic health data scenario
GCKD_df1_femnd[c("height_cm_low", "height_cm_up")] <- str_split_fixed(GCKD_df1_femnd$BL_ku_height_cm, ", ", 2)
GCKD_df1_femnd$height_cm_low <- gsub("^.", "", as.character(GCKD_df1_femnd$height_cm_low))
GCKD_df1_femnd$height_cm_low <- as.numeric(GCKD_df1_femnd$height_cm_low)
GCKD_df1_femnd$height_cm_up <- gsub(".$", "", as.character(GCKD_df1_femnd$height_cm_up))
GCKD_df1_femnd$height_cm_up <- as.numeric(GCKD_df1_femnd$height_cm_up)
GCKD_df1_femnd[c("weight_low", "weight_up")] <- str_split_fixed(GCKD_df1_femnd$BL_ku_weight, ", ", 2)
GCKD_df1_femnd$weight_low <- gsub("^.", "", as.character(GCKD_df1_femnd$weight_low))
GCKD_df1_femnd$weight_low <- as.numeric(GCKD_df1_femnd$weight_low)
GCKD_df1_femnd$weight_up <- gsub(".$", "", as.character(GCKD_df1_femnd$weight_up))
GCKD_df1_femnd$weight_up <- as.numeric(GCKD_df1_femnd$weight_up)
GCKD_df1_femnd <- GCKD_df1_femnd %>% mutate_at(c("height_cm_low"), ~na_if(., 20))
GCKD_df1_femnd <- GCKD_df1_femnd %>% mutate_at(c("height_cm_up"), ~na_if(., 280))
GCKD_df1_femnd <- GCKD_df1_femnd %>% mutate_at(c("weight_low"), ~na_if(., 0))
GCKD_df1_femnd <- GCKD_df1_femnd %>% mutate_at(c("weight_up"), ~na_if(., 300))
GCKD_df1_femnd$BMI_low_calc <- as.numeric(GCKD_df1_femnd$weight_low/(GCKD_df1_femnd$height_cm_up/100)^2)
GCKD_df1_femnd$BMI_up_calc <- as.numeric(GCKD_df1_femnd$weight_up/(GCKD_df1_femnd$height_cm_low/100)^2)
GCKD_df1_femnd$BMI_up_calc <- round(GCKD_df1_femnd$BMI_up_calc ,digit=1)
GCKD_df1_femnd$BMI_low_calc <- round(GCKD_df1_femnd$BMI_low_calc ,digit=1)
GCKD_df1_femnd <- GCKD_df1_femnd %>% 
  mutate(BL_ku_bmi_calc = BMI_low_calc + ((BMI_up_calc-BMI_low_calc)/2))
GCKD_df1_femnd$BL_ku_bmi_calc <- round(GCKD_df1_femnd$BL_ku_bmi_calc, digit=2)
GCKD_df2_femnd[c("height_cm_low", "height_cm_up")] <- str_split_fixed(GCKD_df2_femnd$BL_ku_height_cm, ", ", 2)
GCKD_df2_femnd$height_cm_low <- gsub("^.", "", as.character(GCKD_df2_femnd$height_cm_low))
GCKD_df2_femnd$height_cm_low <- as.numeric(GCKD_df2_femnd$height_cm_low)
GCKD_df2_femnd$height_cm_up <- gsub(".$", "", as.character(GCKD_df2_femnd$height_cm_up))
GCKD_df2_femnd$height_cm_up <- as.numeric(GCKD_df2_femnd$height_cm_up)
GCKD_df2_femnd[c("weight_low", "weight_up")] <- str_split_fixed(GCKD_df2_femnd$BL_ku_weight, ", ", 2)
GCKD_df2_femnd$weight_low <- gsub("^.", "", as.character(GCKD_df2_femnd$weight_low))
GCKD_df2_femnd$weight_low <- as.numeric(GCKD_df2_femnd$weight_low)
GCKD_df2_femnd$weight_up <- gsub(".$", "", as.character(GCKD_df2_femnd$weight_up))
GCKD_df2_femnd$weight_up <- as.numeric(GCKD_df2_femnd$weight_up)
GCKD_df2_femnd <- GCKD_df2_femnd %>% mutate_at(c("height_cm_low"), ~na_if(., 20))
GCKD_df2_femnd <- GCKD_df2_femnd %>% mutate_at(c("height_cm_up"), ~na_if(., 280))
GCKD_df2_femnd <- GCKD_df2_femnd %>% mutate_at(c("weight_low"), ~na_if(., 0))
GCKD_df2_femnd <- GCKD_df2_femnd %>% mutate_at(c("weight_up"), ~na_if(., 300))
GCKD_df2_femnd$BMI_low_calc <- as.numeric(GCKD_df2_femnd$weight_low/(GCKD_df2_femnd$height_cm_up/100)^2)
GCKD_df2_femnd$BMI_up_calc <- as.numeric(GCKD_df2_femnd$weight_up/(GCKD_df2_femnd$height_cm_low/100)^2)
GCKD_df2_femnd$BMI_up_calc <- round(GCKD_df2_femnd$BMI_up_calc ,digit=1)
GCKD_df2_femnd$BMI_low_calc <- round(GCKD_df2_femnd$BMI_low_calc ,digit=1)
GCKD_df2_femnd <- GCKD_df2_femnd %>% 
  mutate(BL_ku_bmi_calc = BMI_low_calc + ((BMI_up_calc-BMI_low_calc)/2))
GCKD_df2_femnd$BL_ku_bmi_calc <- round(GCKD_df4_femnd$BL_ku_bmi_calc, digit=2)
ggplot() + 
  geom_density(data = GCKD_df1_o, aes(BL_ku_bmi, y = ..density..*10000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(GCKD_df4_mut, GCKD_df4_mut$BL_ku_bmi == 21.75), aes(BL_ku_bmi), colour="lightblue4", width = 6.5, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_mut, GCKD_df4_mut$BL_ku_bmi == 55), aes(BL_ku_bmi), colour="lightblue4", width = 30, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_mut, !is.na(GCKD_df4_mut$BL_ku_bmi) & GCKD_df4_mut$BL_ku_bmi != 21.75 & GCKD_df4_mut$BL_ku_bmi != 55), aes(BL_ku_bmi), colour="lightblue4", width = 5, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, GCKD_df3_mut$BL_ku_bmi == 21.75), aes(BL_ku_bmi), colour="darkseagreen3", width = 6.5, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, GCKD_df3_mut$BL_ku_bmi == 55), aes(BL_ku_bmi), colour="darkseagreen3", width = 30, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, !is.na(GCKD_df3_mut$BL_ku_bmi) & GCKD_df3_mut$BL_ku_bmi != 21.75 & GCKD_df3_mut$BL_ku_bmi != 55), aes(BL_ku_bmi), colour="darkseagreen3", width = 5, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_femnd, GCKD_df1_femnd$BL_ku_bmi_calc == 21.75), aes(BL_ku_bmi_calc), colour="indianred4", width = 18, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_femnd, GCKD_df1_femnd$BL_ku_bmi_calc == 28.2), aes(BL_ku_bmi_calc), colour="indianred4", width = 26, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_femnd, GCKD_df1_femnd$BL_ku_bmi_calc == 35.8), aes(BL_ku_bmi_calc), colour="indianred4", width = 19, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_femnd, GCKD_df1_femnd$BL_ku_bmi_calc == 46.2), aes(BL_ku_bmi_calc), colour="indianred4", width = 30, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_femnd, GCKD_df1_femnd$BL_ku_bmi_calc == 49.75), aes(BL_ku_bmi_calc), colour="indianred4", width = 25, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 16.55), aes(BL_ku_bmi_calc), colour="gold", width = 8, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 18.6), aes(BL_ku_bmi_calc), colour="gold", width = 10, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 21.15), aes(BL_ku_bmi_calc), colour="gold", width = 12, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 20.65), aes(BL_ku_bmi_calc), colour="gold", width = 10, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 24.2), aes(BL_ku_bmi_calc), colour="gold", width = 12, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 23.1), aes(BL_ku_bmi_calc), colour="gold", width = 9, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 26.0), aes(BL_ku_bmi_calc), colour="gold", width = 11, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 26.55), aes(BL_ku_bmi_calc), colour="gold", width = 11, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 29.5), aes(BL_ku_bmi_calc), colour="gold", width = 12, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 29.65), aes(BL_ku_bmi_calc), colour="gold", width = 11, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 33.75), aes(BL_ku_bmi_calc), colour="gold", width = 17, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 32.35), aes(BL_ku_bmi_calc), colour="gold", width = 13, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 33.4), aes(BL_ku_bmi_calc), colour="gold", width = 12, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 36.2), aes(BL_ku_bmi_calc), colour="gold", width = 12, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 37.8), aes(BL_ku_bmi_calc), colour="gold", width = 14, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 40.75), aes(BL_ku_bmi_calc), colour="gold", width = 12, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 43.3), aes(BL_ku_bmi_calc), colour="gold", width = 16, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 42.7), aes(BL_ku_bmi_calc), colour="gold", width = 12, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 46.2), aes(BL_ku_bmi_calc), colour="gold", width = 14, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 48.1), aes(BL_ku_bmi_calc), colour="gold", width = 15, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 54.55), aes(BL_ku_bmi_calc), colour="gold", width = 12, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(limits = c(10, 70), sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1)
ggplot() + 
  geom_density(data = GCKD_df1_o, aes(BL_ku_bmi, y = ..density..*10000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(GCKD_df4_mut, GCKD_df4_mut$BL_ku_bmi == 21.75), aes(BL_ku_bmi), colour="lightblue4", width = 6.5, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_mut, GCKD_df4_mut$BL_ku_bmi == 55), aes(BL_ku_bmi), colour="lightblue4", width = 30, fill="transparent") +
  geom_bar(data = subset(GCKD_df4_mut, !is.na(GCKD_df4_mut$BL_ku_bmi) & GCKD_df4_mut$BL_ku_bmi != 21.75 & GCKD_df4_mut$BL_ku_bmi != 55), aes(BL_ku_bmi), colour="lightblue4", width = 5, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, GCKD_df3_mut$BL_ku_bmi == 21.75), aes(BL_ku_bmi), colour="darkseagreen3", width = 6.5, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, GCKD_df3_mut$BL_ku_bmi == 55), aes(BL_ku_bmi), colour="darkseagreen3", width = 30, fill="transparent") +
  geom_bar(data = subset(GCKD_df3_mut, !is.na(GCKD_df3_mut$BL_ku_bmi) & GCKD_df3_mut$BL_ku_bmi != 21.75 & GCKD_df3_mut$BL_ku_bmi != 55), aes(BL_ku_bmi), colour="darkseagreen3", width = 5, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_femnd, GCKD_df1_femnd$BL_ku_bmi_calc == 21.75), aes(BL_ku_bmi_calc), colour="indianred4", width = 18, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_femnd, GCKD_df1_femnd$BL_ku_bmi_calc == 28.2), aes(BL_ku_bmi_calc), colour="indianred4", width = 26, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_femnd, GCKD_df1_femnd$BL_ku_bmi_calc == 35.8), aes(BL_ku_bmi_calc), colour="indianred4", width = 19, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_femnd, GCKD_df1_femnd$BL_ku_bmi_calc == 46.2), aes(BL_ku_bmi_calc), colour="indianred4", width = 30, fill="transparent") +
  geom_bar(data = subset(GCKD_df1_femnd, GCKD_df1_femnd$BL_ku_bmi_calc == 49.75), aes(BL_ku_bmi_calc), colour="indianred4", width = 25, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 16.55), aes(BL_ku_bmi_calc), colour="gold", width = 8, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 18.6), aes(BL_ku_bmi_calc), colour="gold", width = 10, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 21.15), aes(BL_ku_bmi_calc), colour="gold", width = 12, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 20.65), aes(BL_ku_bmi_calc), colour="gold", width = 10, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 24.2), aes(BL_ku_bmi_calc), colour="gold", width = 12, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 23.1), aes(BL_ku_bmi_calc), colour="gold", width = 9, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 26.0), aes(BL_ku_bmi_calc), colour="gold", width = 11, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 26.55), aes(BL_ku_bmi_calc), colour="gold", width = 11, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 29.5), aes(BL_ku_bmi_calc), colour="gold", width = 12, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 29.65), aes(BL_ku_bmi_calc), colour="gold", width = 11, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 33.75), aes(BL_ku_bmi_calc), colour="gold", width = 17, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 32.35), aes(BL_ku_bmi_calc), colour="gold", width = 13, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 33.4), aes(BL_ku_bmi_calc), colour="gold", width = 12, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 36.2), aes(BL_ku_bmi_calc), colour="gold", width = 12, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 37.8), aes(BL_ku_bmi_calc), colour="gold", width = 14, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 40.75), aes(BL_ku_bmi_calc), colour="gold", width = 12, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 43.3), aes(BL_ku_bmi_calc), colour="gold", width = 16, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 42.7), aes(BL_ku_bmi_calc), colour="gold", width = 12, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 46.2), aes(BL_ku_bmi_calc), colour="gold", width = 14, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 48.1), aes(BL_ku_bmi_calc), colour="gold", width = 15, fill="transparent") +
  geom_bar(data = subset(GCKD_df2_femnd, GCKD_df2_femnd$BL_ku_bmi_calc == 54.55), aes(BL_ku_bmi_calc), colour="gold", width = 12, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(limits = c(10, 70), sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# Suppl. figure FigS1
GCKD_df1_o_FigS1 <- GCKD_df1_o
GCKD_df1_o_FigS1 <- GCKD_df1_o_FigS1 %>% mutate(
  BL_age_cat = ifelse(GCKD_df1_o_FigS1$BL_age < 20, 1, ifelse(GCKD_df1_o_FigS1$BL_age >= 20 & GCKD_df1_o_FigS1$BL_age < 30, 2, ifelse(
    GCKD_df1_o_FigS1$BL_age >= 30 & GCKD_df1_o_FigS1$BL_age < 40, 3, ifelse(GCKD_df1_o_FigS1$BL_age >= 40 & GCKD_df1_o_FigS1$BL_age < 50, 4, ifelse(
      GCKD_df1_o_FigS1$BL_age >= 50 & GCKD_df1_o_FigS1$BL_age < 60, 5, ifelse(GCKD_df1_o_FigS1$BL_age >= 60 & GCKD_df1_o_FigS1$BL_age < 70, 6, 7)))))))
#### k11
GCKD_df3_FigS1 <- GCKD_df3
GCKD_df3_FigS1 <- GCKD_df3_FigS1 %>% mutate(
  BL_age_cat = ifelse(GCKD_df3_FigS1$BL_age == "[10, 20[", 1, ifelse(GCKD_df3_FigS1$BL_age == "[20, 30[", 2, ifelse(
    GCKD_df3_FigS1$BL_age == "[30, 40[", 3, ifelse(GCKD_df3_FigS1$BL_age == "[40, 50[", 4, ifelse(
      GCKD_df3_FigS1$BL_age == "[50, 60[", 5, ifelse(GCKD_df3_FigS1$BL_age == "[60, 70[", 6, 7)))))))
#### k11k2
GCKD_df4_FigS1 <- GCKD_df4
GCKD_df4_FigS1 <- GCKD_df4_FigS1 %>% mutate(
  BL_age_cat = ifelse(GCKD_df4_FigS1$BL_age == "[15, 20[", 1, ifelse(GCKD_df4_FigS1$BL_age == "[20, 25[" | GCKD_df4_FigS1$BL_age == "[25, 30[", 2, ifelse(
    GCKD_df4_FigS1$BL_age == "[30, 35[" | GCKD_df4_FigS1$BL_age == "[35, 40[", 3, ifelse(GCKD_df4_FigS1$BL_age == "[40, 45[" | GCKD_df4_FigS1$BL_age == "[45, 50[", 4, ifelse(
      GCKD_df4_FigS1$BL_age == "[50, 55[" | GCKD_df4_FigS1$BL_age == "[55, 60[", 5, ifelse(GCKD_df4_FigS1$BL_age == "[60, 65[" | GCKD_df4_FigS1$BL_age == "[65, 70[", 6, 7)))))))
## subset male
ggplot() +
  geom_bar(data = subset(GCKD_df1_o_FigS1, GCKD_df1_o_FigS1$dem_sex == "Male"), 
           aes(BL_age_cat), colour = "white", fill = "azure4", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(GCKD_df3_FigS1, GCKD_df3_FigS1$dem_sex == "Male"), 
           aes(BL_age_cat), colour = "gold", fill = "transparent", width = 1.0) +
  geom_bar(data = subset(GCKD_df4_FigS1, GCKD_df4_FigS1$dem_sex == "Male"), 
           aes(BL_age_cat), colour = "darkseagreen4", fill = "transparent", width = 1.0) +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
  coord_flip() +
  theme(aspect.ratio = 3/2)
ggplot() +
  geom_bar(data = subset(GCKD_df1_o_FigS1, GCKD_df1_o_FigS1$dem_sex == "Male"), 
           aes(BL_age_cat), colour = "white", fill = "azure4", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(GCKD_df3_FigS1, GCKD_df3_FigS1$dem_sex == "Male"), 
           aes(BL_age_cat), colour = "gold", fill = "transparent", width = 1.0) +
  geom_bar(data = subset(GCKD_df4_FigS1, GCKD_df4_FigS1$dem_sex == "Male"), 
           aes(BL_age_cat), colour = "darkseagreen4", fill = "transparent", width = 1.0) +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
  coord_flip() +
  theme(aspect.ratio = 3/2) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
## subset female
ggplot() +
  geom_bar(data = subset(GCKD_df1_o_FigS1, GCKD_df1_o_FigS1$dem_sex == "Female"), 
           aes(BL_age_cat), colour = "white", fill = "azure4", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(GCKD_df3_FigS1, GCKD_df3_FigS1$dem_sex == "Female"), 
           aes(BL_age_cat), colour = "gold", fill = "transparent", width = 1.0) +
  geom_bar(data = subset(GCKD_df4_FigS1, GCKD_df4_FigS1$dem_sex == "Female"), 
           aes(BL_age_cat), colour = "darkseagreen4", fill = "transparent", width = 1.0) +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
  coord_flip() +
  theme(aspect.ratio = 3/2)
ggplot() +
  geom_bar(data = subset(GCKD_df1_o_FigS1, GCKD_df1_o_FigS1$dem_sex == "Female"), 
           aes(BL_age_cat), colour = "white", fill = "azure4", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(GCKD_df3_FigS1, GCKD_df3_FigS1$dem_sex == "Female"), 
           aes(BL_age_cat), colour = "gold", fill = "transparent", width = 1.0) +
  geom_bar(data = subset(GCKD_df4_FigS1, GCKD_df4_FigS1$dem_sex == "Female"), 
           aes(BL_age_cat), colour = "darkseagreen4", fill = "transparent", width = 1.0) +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
  coord_flip() +
  theme(aspect.ratio = 3/2) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
## subset diabetes
ggplot() +
  geom_bar(data = subset(GCKD_df1_o_FigS1, GCKD_df1_o_FigS1$diabetes == "1"), 
           aes(BL_age_cat), colour = "white", fill = "azure4", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(GCKD_df3_FigS1, GCKD_df3_FigS1$diabetes == "1"), 
           aes(BL_age_cat), colour = "gold", fill = "transparent", width = 1.0) +
  geom_bar(data = subset(GCKD_df4_FigS1, GCKD_df4_FigS1$diabetes == "1"), 
           aes(BL_age_cat), colour = "darkseagreen4", fill = "transparent", width = 1.0) +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
  scale_x_discrete(limits = as.character(1:7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)
ggplot() +
  geom_bar(data = subset(GCKD_df1_o_FigS1, GCKD_df1_o_FigS1$diabetes == "1"), 
           aes(BL_age_cat), colour = "white", fill = "azure4", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(GCKD_df3_FigS1, GCKD_df3_FigS1$diabetes == "1"), 
           aes(BL_age_cat), colour = "gold", fill = "transparent", width = 1.0) +
  geom_bar(data = subset(GCKD_df4_FigS1, GCKD_df4_FigS1$diabetes == "1"), 
           aes(BL_age_cat), colour = "darkseagreen4", fill = "transparent", width = 1.0) +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) +  
  scale_x_discrete(limits = as.character(1:7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
## subset no diabetes
ggplot() +
  geom_bar(data = subset(GCKD_df1_o_FigS1, GCKD_df1_o_FigS1$diabetes == "2"), 
           aes(BL_age_cat), colour = "white", fill = "azure4", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(GCKD_df3_FigS1, GCKD_df3_FigS1$diabetes == "2"), 
           aes(BL_age_cat), colour = "gold", fill = "transparent", width = 1.0) +
  geom_bar(data = subset(GCKD_df4_FigS1, GCKD_df4_FigS1$diabetes == "2"), 
           aes(BL_age_cat), colour = "darkseagreen4", fill = "transparent", width = 1.0) +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) +  
  scale_x_discrete(limits = as.character(1:7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)
ggplot() +
  geom_bar(data = subset(GCKD_df1_o_FigS1, GCKD_df1_o_FigS1$diabetes == "2"), 
           aes(BL_age_cat), colour = "white", fill = "azure4", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(GCKD_df3_FigS1, GCKD_df3_FigS1$diabetes == "2"), 
           aes(BL_age_cat), colour = "gold", fill = "transparent", width = 1.0) +
  geom_bar(data = subset(GCKD_df4_FigS1, GCKD_df4_FigS1$diabetes == "2"), 
           aes(BL_age_cat), colour = "darkseagreen4", fill = "transparent", width = 1.0) +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) +  
  scale_x_discrete(limits = as.character(1:7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())




# Comparing generic purpose utility and privacy metrics
df <- as_tibble(read.xlsx("GCKD_results_genericmetrics.xlsx", sep = ";"))
df <- df %>% mutate(avg_safe = 100-Risk_avg)
ggplot() +
  geom_point(data = df, aes(avg_safe, Granular, colour = factor(Model)), shape = 16, size = 5) +
  geom_point(data = df, aes(avg_safe, Discern, colour = factor(Model)), shape = 17, size = 5) +
  geom_point(data = df, aes(avg_safe, Entropy, colour = factor(Model)), shape = 15, size = 5) +
  scale_color_manual(values = c("Gen_11" = "indianred4", "Gen_11_2" = "gold", "Spec_11" = "darkseagreen3", "Spec_11_2" = "lightblue4")) +
  scale_y_continuous(limits = c(0, 100)) + 
  scale_x_continuous(limits = c(0, 100))
ggplot() +
  geom_point(data = df, aes(avg_safe, Granular, colour = factor(Model)), shape = 16, size = 5) +
  geom_point(data = df, aes(avg_safe, Discern, colour = factor(Model)), shape = 17, size = 5) +
  geom_point(data = df, aes(avg_safe, Entropy, colour = factor(Model)), shape = 15, size = 5) +
  scale_color_manual(values = c("Gen_11" = "indianred4", "Gen_11_2" = "gold", "Spec_11" = "darkseagreen3", "Spec_11_2" = "lightblue4")) +
  scale_y_continuous(limits = c(0, 100)) + 
  scale_x_continuous(limits = c(0, 100)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")


