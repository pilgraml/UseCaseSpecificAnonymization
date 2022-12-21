# Packages 
pacman::p_load(tidyr, stringr, dplyr, openxlsx, naniar, emmeans, multcomp, 
               plyr, finalfit, ggplot2, tibble, lmtest, sandwich,
               tidyverse, tidyselect, summarytools, scales, gridExtra, 
               lubridate, eeptools, gtsummary, flextable, boot, mosaic, 
               patchwork, rms, coxed, DescTools, PropCIs)

# Dataset
path_tbl = "C:/Users/User/OneDrive/Documents/PRIVAT/Charite/Forschung/Projekt Computerbasierte Anonymisierung/Titzeetal/Ergebnisse/Tbl_perc_CI"
setwd(path_tbl)

## Defining function for overlap in the interval lengths 
### based on Karr et al
#### no overlap is defined as 0
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

# Tbl 1
## Datasets
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
tbl1_anonym <- as_tibble(read.xlsx("tbl1_strictaverage_usecase_11.xlsx"))
tbl1_anonym <- subset(tbl1_anonym,  
                      tbl1_anonym$Variable != "Male")
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
## Rounding 1 decimal
tbl1_origin <- tbl1_origin %>% mutate(across(where(is.numeric), ~round(., 1)))
tbl1_anonym <- tbl1_anonym %>% mutate(across(where(is.numeric), ~round(., 1)))
## Overlapping 95% CI
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
write.xlsx(tbl1_ci, "tbl1_CIoverlap_strictaverage_usecase_11.xlsx")

# Fig 1: 100% replicability (included as more a table than a figure)

# Tbl 2
## Datasets
tbl2_total_origin <- as_tibble(read.xlsx("tbl2_origin.xlsx"))
tbl2_total_origin <- subset(tbl2_total_origin, tbl2_total_origin$Variable != "BL_age")
tbl2_origin <- subset(tbl2_total_origin, tbl2_total_origin$Variable == "Male" |
                        tbl2_total_origin$Variable == "biopsy_yes" |
                        tbl2_total_origin$Variable == "cardiovasc_less60_yes" |
                        tbl2_total_origin$Variable == "cardiovasc_more60_yes")
tbl2_anonym <- as_tibble(read.xlsx("tbl2_strictaverage_usecase_11.xlsx"))

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
## Overlapping 95% CI
tbl2_ci <- data.frame(NA_col = rep(NA, 4))
res_ci_egfr = numeric(4)
res_ci_prot = numeric(4)
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
## Adding 100% replicability
tbl2_ci <- full_join(x = tbl2_total_origin, y = tbl2_ci, by = "Variable")
tbl2_ci <- tbl2_ci %>% replace(is.na(.), 100.0)
tbl2_ci <- subset(tbl2_ci, select = c(CI_overlap_egfr, CI_overlap_prot, Variable))
write.xlsx(tbl2_ci, "tbl2_CIoverlap_strictaverage_usecase_11.xlsx")

# Fig 2: 100% replicability (tbd: some numbers but predominantly figure)

# Tbl 3
## Datasets
tbl3_total_origin <- as_tibble(read.xlsx("tbl3_origin.xlsx"))
tbl3_total_origin_t <- data.frame(t(tbl3_total_origin[-1]))
colnames(tbl3_total_origin_t) <- c("biopsy", "biopsy_no", "CI_biopsy", "(Missing)")
tbl3_total_origin_t$Variable <- row.names(tbl3_total_origin_t)
tbl3_origin <- subset(tbl3_total_origin_t, select = c("Variable", "CI_biopsy"), !(startsWith(tbl3_total_origin_t$Variable, "perc_")))
tbl3_total_anonym <- as_tibble(read.xlsx("tbl3_strictaverage_usecase_11.xlsx"))
tbl3_total_anonym_t <- data.frame(t(tbl3_total_anonym[-1]))
colnames(tbl3_total_anonym_t) <- c("biopsy", "biopsy_no", "CI_biopsy", "(Missing)")
tbl3_total_anonym_t$Variable <- row.names(tbl3_total_anonym_t)
tbl3_anonym <- subset(tbl3_total_anonym_t, select = c("Variable", "CI_biopsy"), !(startsWith(tbl3_total_anonym_t$Variable, "perc_")))
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
## Overlapping 95% CI
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
## Adding 100% replicability
CI_overlap_ckd_diab = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
CI_overlap_ckd_oth = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
CI_overlap_ckd_lead_uk = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
CI_overlap_ckd_vask = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
CI_overlap_ckd_syst = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
CI_overlap_ckd_glom_prim = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
CI_overlap_ckd_interst = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
CI_overlap_ckd_aki = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
CI_overlap_ckd_single = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
CI_overlap_ckd_heredit = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
CI_overlap_ckd_obstr = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
tbl3_ci_total <- cbind(CI_overlap_ckd_diab, CI_overlap_ckd_oth, CI_overlap_ckd_lead_uk, CI_overlap_ckd_vask, CI_overlap_ckd_syst, CI_overlap_ckd_glom_prim, 
                       CI_overlap_ckd_interst, CI_overlap_ckd_aki, CI_overlap_ckd_single, CI_overlap_ckd_heredit, CI_overlap_ckd_obstr, tbl3_ci)
write.xlsx(tbl3_ci_total, "tbl3_CIoverlap_strictaverage_usecase_11.xlsx")

# Fig 3: 100% replicability (not included: predominantly figure)

# Tbl 4
## Datasets
tbl4_origin <- as_tibble(read.xlsx("tbl4_origin.xlsx"))
tbl4_anonym <- as_tibble(read.xlsx("tbl4_strictaverage_usecase_11.xlsx"))
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
## Overlapping 95% CI
tbl4_ci <- data.frame(NA_col = rep(NA, 18))
res_ci_male_d = numeric(18)
res_ci_male_nd = numeric(18)
res_ci_female_d = numeric(18)
res_ci_female_nd = numeric(18)
res_ci = numeric(18)
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
## Adding 100% replicability
tbl4_ci <- tbl4_ci %>% replace(is.na(.), 100.0)
write.xlsx(tbl4_ci, "tbl4_CIoverlap_strictaverage_usecase_11.xlsx")

# Suppl. Tbl 2: 100% replicability

# Tbl s3
## Datasets
tbls3_total_origin <- as_tibble(read.xlsx("tbls3_origin.xlsx"))
tbls3_total_origin <- subset(tbls3_total_origin, tbls3_total_origin$Variable != "BL_age" &
                               tbls3_total_origin$Variable != "BL_ku_bmi" &
                               tbls3_total_origin$Variable != "BL_ku_height_cm" &
                               tbls3_total_origin$Variable != "BL_ku_weight")
tbls3_origin <- subset(tbls3_total_origin, tbls3_total_origin$Variable == "biopsy_yes")
tbls3_anonym <- as_tibble(read.xlsx("tbls3_strictaverage_usecase_11.xlsx"))
## Separating 95% CI bounds
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
## Rounding 1 decimal
tbls3_origin <- tbls3_origin %>% mutate(across(where(is.numeric), ~round(., 1)))
tbls3_anonym <- tbls3_anonym %>% mutate(across(where(is.numeric), ~round(., 1)))
## Overlapping 95% CI
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
## Adding 100% replicability
tbls3_ci <- full_join(x = tbls3_total_origin, y = tbls3_ci, by = "Variable")
tbls3_ci <- tbls3_ci %>% replace(is.na(.), 100.0)
tbls3_ci <- subset(tbls3_ci, select = c(CI_overlap_DMwDN, CI_overlap_DMwoDN, CI_overlap_NoDM, Variable))
write.xlsx(tbls3_ci, "tbls3_CIoverlap_strictaverage_usecase_11.xlsx")

# Suppl. Fig S1: visual comparison (not included) 