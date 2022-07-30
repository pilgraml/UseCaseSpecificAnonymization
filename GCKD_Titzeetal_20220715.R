# Packages 
pacman::p_load(tidyr, stringr, dplyr, openxlsx, naniar, emmeans, multcomp, 
               plyr, finalfit, ggplot2, tibble, MatchIt, optmatch, lmtest, sandwich,
               mice, tidyverse, tidyselect, summarytools, scales, gridExtra, 
               lubridate, eeptools, kidney.epi, nephro)

# Datensatz
setwd("Z:/GCKD/2022-07-11_Daten_für_LPilgram/")
# Titze <- as_tibble(read.csv2("GCKD_BL-Daten_für_LPilgram_2022-06-03_test.csv", header = TRUE, sep = ";"))
Titze <- as_tibble(read.xlsx("GCKD_BL-Daten_für_LPilgram_2022-06-03_test.xlsx", sep = ";"))
dim(Titze)
View(Titze)
str(Titze)

# Data Preprocessing
## Alter: Berechnung aus Visitendatum + Geburtsdatum, Umkodierung der NA in 0
Titze_visdat_mis <- Titze[is.na(Titze$ein_visdat),]
Titze$ein_visdat[is.na(Titze$ein_visdat)] <- "01.09.1977"
Titze$ein_visdat <- as.Date(Titze$ein_visdat, format = "%d.%m.%Y")
Titze$dem_geburt <- as.Date(Titze$dem_geburt, format = "%d.%m.%Y")
Titze$dem_age <- floor(age_calc(Titze$dem_geburt, enddate = Titze$ein_visdat, units = "years"))
Titze <- Titze %>% mutate(BL_age = ifelse(Titze$dem_age == 0, 9999, dem_age))
Titze <- Titze %>% na_if(9999)
## Familienanamnese
### Stroke, Myokardinfarkt, Hypertension, Diabetes, Nierenerkrankung, Nierensteine, Dialyse, NTx
Titze <- Titze %>% mutate(aa_stroke = ifelse(aa_schlag2 > 0, 1, 2))
Titze <- Titze %>% mutate(aa_myocard = ifelse(aa_herzinf2 > 0, 1, 2))
Titze <- Titze %>% mutate(aa_hypertens = ifelse(aa_blutdr2 > 0, 1, 2))
Titze <- Titze %>% mutate(aa_diabetes = ifelse(aa_blutzu2 > 0, 1, 2))
Titze <- Titze %>% mutate(aa_renal = ifelse(aa_nierenerk_1 > 0, 1, 2))
Titze <- Titze %>% mutate(aa_renal_stones = ifelse(aa_nie_stein_1 > 0, 1, 2))
Titze <- Titze %>% mutate(aa_dialyse = ifelse(aa_dialyse_1 > 0, 1, 2))
Titze <- Titze %>% mutate(aa_ntx = ifelse(aa_transpl_1 > 0, 1, 2))
## Geschlecht > unterschiedl. Labeling für GFR Packages
table(Titze$dem_geschl)
Titze$dem_sex <- mapvalues(Titze$dem_geschl, from = c("1", "2"), to = c("Male", "Female"))
Titze$dem_sex <- factor(Titze$dem_sex, levels = c("Male", "Female"))
Titze$dem_sex_2 <- mapvalues(Titze$dem_geschl, from = c("1", "2"), to = c("1", "0"))
Titze$dem_sex_2 <- as.integer(Titze$dem_sex_2)
## Rauchstatus
Titze <- Titze %>% mutate(smoking = ifelse(BL_pf_je_tabak == 2, 1, ifelse(BL_pf_je_tabak == 1 & BL_pf_akt_tabak == 1, 2, ifelse(BL_pf_je_tabak == 1 & (BL_pf_akt_tabak == 2 | BL_pf_akt_tabak == 3), 3, 9999))))
## Hospitalisierung
Titze <- Titze %>% mutate(hospital = ifelse(aa_beh_kh == 1, 1, ifelse(aa_beh_kh == 2, 2, 9999)))
## BMI > Missing der Messung durch Pat Angaben ersetzten (ku_gr_pat, ku_gew_pat)
Titze$BL_ku_groesse_m <- as.numeric(Titze$BL_ku_groesse/100)
Titze$BL_ku_gr_pat_m <- as.numeric(Titze$BL_ku_gr_pat/100)
Titze <- Titze %>% mutate(BL_ku_height = ifelse(is.na(Titze$BL_ku_groesse_m) == TRUE, BL_ku_gr_pat_m, BL_ku_groesse_m))
Titze <- Titze %>% mutate(BL_ku_weight = ifelse(is.na(Titze$BL_ku_gewicht) == TRUE, BL_ku_gew_pat, BL_ku_gewicht))
Titze$BL_ku_bmi <- as.numeric(Titze$BL_ku_weight/(Titze$BL_ku_height*Titze$BL_ku_height))
Titze <- Titze %>% mutate(BL_ku_bmi_cat = ifelse(BL_ku_bmi <= 25, 1, ifelse(BL_ku_bmi > 25 & BL_ku_bmi <= 30, 2, 3)))
## Blutdruck
Titze$BL_ku_sys <- as.numeric((Titze$BL_ku_mess1_sys_1 + Titze$BL_ku_mess2_sys_1 + Titze$BL_ku_mess3_sys)/3)
Titze$BL_ku_dia <- as.numeric((Titze$BL_ku_mess1_dia_1 + Titze$BL_ku_mess2_dia_1 + Titze$BL_ku_mess3_dia)/3)
Titze$BL_ku_map <- as.numeric((2*Titze$BL_ku_dia + Titze$BL_ku_sys)/3)
Titze <- Titze %>% mutate(BL_ku_rr_130 = ifelse(BL_ku_sys < 130 & BL_ku_dia < 80, 1, 2))
Titze <- Titze %>% mutate(BL_ku_rr_140 = ifelse(BL_ku_sys < 140 & BL_ku_dia < 90, 1, 2))
## GFR 
### Kreatinin > mg/dl
Titze$BL_creavalue <- as.numeric(Titze$BL_creavalue)
table(Titze$BL_creaunit)
### Cystatin > mg/l
Titze$BL_cysvalue <- as.numeric(Titze$BL_cysvalue)
table(Titze$BL_cysunit)
### Spalte Ethnicity
Titze$dem_ethn = 0
### mit Package kidney.epi: falsche Berechnung
### mit Package nephro
#### MRDR (4-Variablen-Formel)
Titze$BL_gfr_mdrd <- MDRD4(creatinine = Titze$BL_creavalue, age = Titze$BL_age, sex = Titze$dem_sex_2, ethnicity = Titze$dem_ethn)
Titze <- Titze %>% mutate(BL_gfr_mdrd_cat = ifelse(BL_gfr_mdrd >= 90, "G1", ifelse(BL_gfr_mdrd >= 60 & BL_gfr_mdrd < 90, "G2", ifelse(BL_gfr_mdrd >= 45 & BL_gfr_mdrd < 60, "G3a", ifelse(BL_gfr_mdrd >= 30 & BL_gfr_mdrd < 45, "G3b", ifelse(BL_gfr_mdrd >= 15 & BL_gfr_mdrd < 30, "G4", "G5"))))))
#### CKD-EPI Krea
Titze$BL_gfr_ckdepi <- CKDEpi.creat(creatinine = Titze$BL_creavalue, age = Titze$BL_age, sex = Titze$dem_sex_2, ethnicity = Titze$dem_ethn)
Titze <- Titze %>% mutate(BL_gfr_ckdepi_cat = ifelse(BL_gfr_ckdepi >= 90, "G1", ifelse(BL_gfr_ckdepi >= 60 & BL_gfr_ckdepi < 90, "G2", ifelse(BL_gfr_ckdepi >= 45 & BL_gfr_ckdepi < 60, "G3a", ifelse(BL_gfr_ckdepi >= 30 & BL_gfr_ckdepi < 45, "G3b", ifelse(BL_gfr_ckdepi >= 15 & BL_gfr_ckdepi < 30, "G4", "G5"))))))
#### CKD-EPI Cys
Titze$BL_gfr_ckdepi_cys <- CKDEpi.cys(cystatin = Titze$BL_cysvalue, age = Titze$BL_age, sex = Titze$dem_sex_2)
Titze <- Titze %>% mutate(BL_gfr_ckdepi_cys_cat = ifelse(BL_gfr_ckdepi_cys >= 90, "G1", ifelse(BL_gfr_ckdepi_cys >= 60 & BL_gfr_ckdepi_cys < 90, "G2", ifelse(BL_gfr_ckdepi_cys >= 45 & BL_gfr_ckdepi_cys < 60, "G3a", ifelse(BL_gfr_ckdepi_cys >= 30 & BL_gfr_ckdepi_cys < 45, "G3b", ifelse(BL_gfr_ckdepi_cys >= 15 & BL_gfr_ckdepi_cys < 30, "G4", "G5"))))))
#### CKD-EPI Cys-Crea
Titze$BL_gfr_ckdepi_creacys <- CKDEpi.creat.cys(creatinine = Titze$BL_creavalue, cystatin = Titze$BL_cysvalue, age = Titze$BL_age, sex = Titze$dem_sex_2, ethnicity = Titze$dem_ethn)
Titze <- Titze %>% mutate(BL_gfr_ckdepi_creacys_cat = ifelse(BL_gfr_ckdepi_creacys >= 90, "G1", ifelse(BL_gfr_ckdepi_creacys >= 60 & BL_gfr_ckdepi_creacys < 90, "G2", ifelse(BL_gfr_ckdepi_creacys >= 45 & BL_gfr_ckdepi_creacys < 60, "G3a", ifelse(BL_gfr_ckdepi_creacys >= 30 & BL_gfr_ckdepi_creacys < 45, "G3b", ifelse(BL_gfr_ckdepi_creacys >= 15 & BL_gfr_ckdepi_creacys < 30, "G4", "G5"))))))
## Urin Alb/Kreatinin > mg/gKrea: Urin Albumin > mg/l, Urin Kreatinin mg/dl > Umwandlung in g/l
Titze$BL_ucreavalue <- as.numeric(Titze$BL_ucreavalue)
Titze$BL_ualbuvalue <- as.numeric(Titze$BL_ualbuvalue)
Titze$BL_ucreavalue_gl <- as.numeric(Titze$BL_ucreavalue/100)
Titze$BL_uacr <- as.numeric(Titze$BL_ualbuvalue/Titze$BL_ucreavalue_gl)
Titze <- Titze %>% mutate(BL_uacr_cat = ifelse(BL_uacr < 30, "A1", ifelse(BL_uacr >= 30 & BL_uacr <= 300, "A2", "A3")))
## Medikation ATC-Kodierung Anzahl 1-40, datatype character
### Problem 1 numerical bei komplett NA (ab Code 33) startsWith funktioniert nicht > Spalten herausnehmen
### Problem 2 jegliche NA werden zu komplett NA > NA umkodieren in "9999"
Titze_bm_atc_NA <- Titze %>% dplyr::select_if(colSums(is.na(.)) == nrow(Titze))
colnames(Titze_bm_atc_NA)
Titze <- Titze %>% mutate_if(is.character, ~replace_na(., "9999"))
### Antihypertensiva C02, C03, C07, C08C, C08E, C08G, C09
#### ACE-Inhibitoren C09A, C09B
Titze <- Titze %>% mutate(bm_atc_09a = ifelse(startsWith(bm_atc_medi_code_1, "C09A") | 
                                                startsWith(bm_atc_medi_code_2, "C09A") | 
                                                startsWith(bm_atc_medi_code_3, "C09A") | 
                                                startsWith(bm_atc_medi_code_4, "C09A") | 
                                                startsWith(bm_atc_medi_code_5, "C09A") | 
                                                startsWith(bm_atc_medi_code_6, "C09A") | 
                                                startsWith(bm_atc_medi_code_7, "C09A") | 
                                                startsWith(bm_atc_medi_code_8, "C09A") | 
                                                startsWith(bm_atc_medi_code_9, "C09A") | 
                                                startsWith(bm_atc_medi_code_10, "C09A") | 
                                                startsWith(bm_atc_medi_code_11, "C09A") | 
                                                startsWith(bm_atc_medi_code_12, "C09A") | 
                                                startsWith(bm_atc_medi_code_13, "C09A") | 
                                                startsWith(bm_atc_medi_code_14, "C09A") | 
                                                startsWith(bm_atc_medi_code_15, "C09A") | 
                                                startsWith(bm_atc_medi_code_16, "C09A") | 
                                                startsWith(bm_atc_medi_code_17, "C09A") | 
                                                startsWith(bm_atc_medi_code_18, "C09A") | 
                                                startsWith(bm_atc_medi_code_19, "C09A") | 
                                                startsWith(bm_atc_medi_code_20, "C09A") | 
                                                startsWith(bm_atc_medi_code_21, "C09A") | 
                                                startsWith(bm_atc_medi_code_22, "C09A") | 
                                                startsWith(bm_atc_medi_code_23, "C09A") | 
                                                startsWith(bm_atc_medi_code_24, "C09A") | 
                                                startsWith(bm_atc_medi_code_25, "C09A") | 
                                                startsWith(bm_atc_medi_code_26, "C09A") | 
                                                startsWith(bm_atc_medi_code_27, "C09A") | 
                                                startsWith(bm_atc_medi_code_28, "C09A") | 
                                                startsWith(bm_atc_medi_code_29, "C09A") | 
                                                startsWith(bm_atc_medi_code_30, "C09A") | 
                                                startsWith(bm_atc_medi_code_31, "C09A") | 
                                                startsWith(bm_atc_medi_code_32, "C09A"), 1,
                                              ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

Titze <- Titze %>% mutate(bm_atc_09b = ifelse(startsWith(bm_atc_medi_code_1, "C09B") | 
                                                startsWith(bm_atc_medi_code_2, "C09B") | 
                                                startsWith(bm_atc_medi_code_3, "C09B") | 
                                                startsWith(bm_atc_medi_code_4, "C09B") | 
                                                startsWith(bm_atc_medi_code_5, "C09B") | 
                                                startsWith(bm_atc_medi_code_6, "C09B") | 
                                                startsWith(bm_atc_medi_code_7, "C09B") | 
                                                startsWith(bm_atc_medi_code_8, "C09B") | 
                                                startsWith(bm_atc_medi_code_9, "C09B") | 
                                                startsWith(bm_atc_medi_code_10, "C09B") | 
                                                startsWith(bm_atc_medi_code_11, "C09B") | 
                                                startsWith(bm_atc_medi_code_12, "C09B") | 
                                                startsWith(bm_atc_medi_code_13, "C09B") | 
                                                startsWith(bm_atc_medi_code_14, "C09B") | 
                                                startsWith(bm_atc_medi_code_15, "C09B") | 
                                                startsWith(bm_atc_medi_code_16, "C09B") | 
                                                startsWith(bm_atc_medi_code_17, "C09B") | 
                                                startsWith(bm_atc_medi_code_18, "C09B") | 
                                                startsWith(bm_atc_medi_code_19, "C09B") | 
                                                startsWith(bm_atc_medi_code_20, "C09B") | 
                                                startsWith(bm_atc_medi_code_21, "C09B") | 
                                                startsWith(bm_atc_medi_code_22, "C09B") | 
                                                startsWith(bm_atc_medi_code_23, "C09B") | 
                                                startsWith(bm_atc_medi_code_24, "C09B") | 
                                                startsWith(bm_atc_medi_code_25, "C09B") | 
                                                startsWith(bm_atc_medi_code_26, "C09B") | 
                                                startsWith(bm_atc_medi_code_27, "C09B") | 
                                                startsWith(bm_atc_medi_code_28, "C09B") | 
                                                startsWith(bm_atc_medi_code_29, "C09B") | 
                                                startsWith(bm_atc_medi_code_30, "C09B") | 
                                                startsWith(bm_atc_medi_code_31, "C09B") | 
                                                startsWith(bm_atc_medi_code_32, "C09B"), 1, 
                                              ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

Titze <- Titze %>% mutate(bm_atc_ace = ifelse(bm_atc_09a == 1 | bm_atc_09b == 1, 1, ifelse (bm_atc_09a == 2 & bm_atc_09b == 2, 2, 9999)))

#### ARB C09C, C09D
Titze <- Titze %>% mutate(bm_atc_09c = ifelse(startsWith(bm_atc_medi_code_1, "C09C") | 
                                                startsWith(bm_atc_medi_code_2, "C09C") | 
                                                startsWith(bm_atc_medi_code_3, "C09C") | 
                                                startsWith(bm_atc_medi_code_4, "C09C") | 
                                                startsWith(bm_atc_medi_code_5, "C09C") | 
                                                startsWith(bm_atc_medi_code_6, "C09C") | 
                                                startsWith(bm_atc_medi_code_7, "C09C") | 
                                                startsWith(bm_atc_medi_code_8, "C09C") | 
                                                startsWith(bm_atc_medi_code_9, "C09C") | 
                                                startsWith(bm_atc_medi_code_10, "C09C") | 
                                                startsWith(bm_atc_medi_code_11, "C09C") | 
                                                startsWith(bm_atc_medi_code_12, "C09C") | 
                                                startsWith(bm_atc_medi_code_13, "C09C") | 
                                                startsWith(bm_atc_medi_code_14, "C09C") | 
                                                startsWith(bm_atc_medi_code_15, "C09C") | 
                                                startsWith(bm_atc_medi_code_16, "C09C") | 
                                                startsWith(bm_atc_medi_code_17, "C09C") | 
                                                startsWith(bm_atc_medi_code_18, "C09C") | 
                                                startsWith(bm_atc_medi_code_19, "C09C") | 
                                                startsWith(bm_atc_medi_code_20, "C09C") | 
                                                startsWith(bm_atc_medi_code_21, "C09C") | 
                                                startsWith(bm_atc_medi_code_22, "C09C") | 
                                                startsWith(bm_atc_medi_code_23, "C09C") | 
                                                startsWith(bm_atc_medi_code_24, "C09C") | 
                                                startsWith(bm_atc_medi_code_25, "C09C") | 
                                                startsWith(bm_atc_medi_code_26, "C09C") | 
                                                startsWith(bm_atc_medi_code_27, "C09C") | 
                                                startsWith(bm_atc_medi_code_28, "C09C") | 
                                                startsWith(bm_atc_medi_code_29, "C09C") | 
                                                startsWith(bm_atc_medi_code_30, "C09C") | 
                                                startsWith(bm_atc_medi_code_31, "C09C") | 
                                                startsWith(bm_atc_medi_code_32, "C09C"), 1, 
                                              ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

Titze <- Titze %>% mutate(bm_atc_09d = ifelse(startsWith(bm_atc_medi_code_1, "C09D") | 
                                                startsWith(bm_atc_medi_code_2, "C09D") | 
                                                startsWith(bm_atc_medi_code_3, "C09D") | 
                                                startsWith(bm_atc_medi_code_4, "C09D") | 
                                                startsWith(bm_atc_medi_code_5, "C09D") | 
                                                startsWith(bm_atc_medi_code_6, "C09D") | 
                                                startsWith(bm_atc_medi_code_7, "C09D") | 
                                                startsWith(bm_atc_medi_code_8, "C09D") | 
                                                startsWith(bm_atc_medi_code_9, "C09D") | 
                                                startsWith(bm_atc_medi_code_10, "C09D") | 
                                                startsWith(bm_atc_medi_code_11, "C09D") | 
                                                startsWith(bm_atc_medi_code_12, "C09D") | 
                                                startsWith(bm_atc_medi_code_13, "C09D") | 
                                                startsWith(bm_atc_medi_code_14, "C09D") | 
                                                startsWith(bm_atc_medi_code_15, "C09D") | 
                                                startsWith(bm_atc_medi_code_16, "C09D") | 
                                                startsWith(bm_atc_medi_code_17, "C09D") | 
                                                startsWith(bm_atc_medi_code_18, "C09D") | 
                                                startsWith(bm_atc_medi_code_19, "C09D") | 
                                                startsWith(bm_atc_medi_code_20, "C09D") | 
                                                startsWith(bm_atc_medi_code_21, "C09D") | 
                                                startsWith(bm_atc_medi_code_22, "C09D") | 
                                                startsWith(bm_atc_medi_code_23, "C09D") | 
                                                startsWith(bm_atc_medi_code_24, "C09D") | 
                                                startsWith(bm_atc_medi_code_25, "C09D") | 
                                                startsWith(bm_atc_medi_code_26, "C09D") | 
                                                startsWith(bm_atc_medi_code_27, "C09D") | 
                                                startsWith(bm_atc_medi_code_28, "C09D") | 
                                                startsWith(bm_atc_medi_code_29, "C09D") | 
                                                startsWith(bm_atc_medi_code_30, "C09D") | 
                                                startsWith(bm_atc_medi_code_31, "C09D") | 
                                                startsWith(bm_atc_medi_code_32, "C09D"), 1, 
                                              ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

Titze <- Titze %>% mutate(bm_atc_arb = ifelse(bm_atc_09c == 1 | bm_atc_09d == 1, 1, ifelse (bm_atc_09c == 2 & bm_atc_09d == 2, 2, 9999)))

#### RAAS-Blockade insgesamt
Titze <- Titze %>% mutate(bm_atc_raas = ifelse(bm_atc_ace == 1 | bm_atc_arb == 1, 1, ifelse (bm_atc_ace == 2 & bm_atc_arb == 2, 2, 9999)))
Titze <- Titze %>% mutate(bm_atc_raas_comb = ifelse(bm_atc_ace == 1 & bm_atc_arb == 1, 1, ifelse (bm_atc_ace == 9999 | bm_atc_arb == 9999, 9999, 2)))

#### Diuretika C03
Titze <- Titze %>% mutate(bm_atc_diuretics = ifelse(startsWith(bm_atc_medi_code_1, "C03") | 
                                               startsWith(bm_atc_medi_code_2, "C03") | 
                                               startsWith(bm_atc_medi_code_3, "C03") | 
                                               startsWith(bm_atc_medi_code_4, "C03") | 
                                               startsWith(bm_atc_medi_code_5, "C03") | 
                                               startsWith(bm_atc_medi_code_6, "C03") | 
                                               startsWith(bm_atc_medi_code_7, "C03") | 
                                               startsWith(bm_atc_medi_code_8, "C03") | 
                                               startsWith(bm_atc_medi_code_9, "C03") | 
                                               startsWith(bm_atc_medi_code_10, "C03") | 
                                               startsWith(bm_atc_medi_code_11, "C03") | 
                                               startsWith(bm_atc_medi_code_12, "C03") | 
                                               startsWith(bm_atc_medi_code_13, "C03") | 
                                               startsWith(bm_atc_medi_code_14, "C03") | 
                                               startsWith(bm_atc_medi_code_15, "C03") | 
                                               startsWith(bm_atc_medi_code_16, "C03") | 
                                               startsWith(bm_atc_medi_code_17, "C03") | 
                                               startsWith(bm_atc_medi_code_18, "C03") | 
                                               startsWith(bm_atc_medi_code_19, "C03") | 
                                               startsWith(bm_atc_medi_code_20, "C03") | 
                                               startsWith(bm_atc_medi_code_21, "C03") | 
                                               startsWith(bm_atc_medi_code_22, "C03") | 
                                               startsWith(bm_atc_medi_code_23, "C03") | 
                                               startsWith(bm_atc_medi_code_24, "C03") | 
                                               startsWith(bm_atc_medi_code_25, "C03") | 
                                               startsWith(bm_atc_medi_code_26, "C03") | 
                                               startsWith(bm_atc_medi_code_27, "C03") | 
                                               startsWith(bm_atc_medi_code_28, "C03") | 
                                               startsWith(bm_atc_medi_code_29, "C03") | 
                                               startsWith(bm_atc_medi_code_30, "C03") | 
                                               startsWith(bm_atc_medi_code_31, "C03") | 
                                               startsWith(bm_atc_medi_code_32, "C03"), 1, 
                                             ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

##### Thiazide, Thiazid-like C03A, C03B, C03EA, C07B, C07D
Titze <- Titze %>% mutate(bm_atc_03a = ifelse(startsWith(bm_atc_medi_code_1, "C03A") | 
                                                startsWith(bm_atc_medi_code_2, "C03A") | 
                                                startsWith(bm_atc_medi_code_3, "C03A") | 
                                                startsWith(bm_atc_medi_code_4, "C03A") | 
                                                startsWith(bm_atc_medi_code_5, "C03A") | 
                                                startsWith(bm_atc_medi_code_6, "C03A") | 
                                                startsWith(bm_atc_medi_code_7, "C03A") | 
                                                startsWith(bm_atc_medi_code_8, "C03A") | 
                                                startsWith(bm_atc_medi_code_9, "C03A") | 
                                                startsWith(bm_atc_medi_code_10, "C03A") | 
                                                startsWith(bm_atc_medi_code_11, "C03A") | 
                                                startsWith(bm_atc_medi_code_12, "C03A") | 
                                                startsWith(bm_atc_medi_code_13, "C03A") | 
                                                startsWith(bm_atc_medi_code_14, "C03A") | 
                                                startsWith(bm_atc_medi_code_15, "C03A") | 
                                                startsWith(bm_atc_medi_code_16, "C03A") | 
                                                startsWith(bm_atc_medi_code_17, "C03A") | 
                                                startsWith(bm_atc_medi_code_18, "C03A") | 
                                                startsWith(bm_atc_medi_code_19, "C03A") | 
                                                startsWith(bm_atc_medi_code_20, "C03A") | 
                                                startsWith(bm_atc_medi_code_21, "C03A") | 
                                                startsWith(bm_atc_medi_code_22, "C03A") | 
                                                startsWith(bm_atc_medi_code_23, "C03A") | 
                                                startsWith(bm_atc_medi_code_24, "C03A") | 
                                                startsWith(bm_atc_medi_code_25, "C03A") | 
                                                startsWith(bm_atc_medi_code_26, "C03A") | 
                                                startsWith(bm_atc_medi_code_27, "C03A") | 
                                                startsWith(bm_atc_medi_code_28, "C03A") | 
                                                startsWith(bm_atc_medi_code_29, "C03A") | 
                                                startsWith(bm_atc_medi_code_30, "C03A") | 
                                                startsWith(bm_atc_medi_code_31, "C03A") | 
                                                startsWith(bm_atc_medi_code_32, "C03A"), 1, 
                                              ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

Titze <- Titze %>% mutate(bm_atc_03b = ifelse(startsWith(bm_atc_medi_code_1, "C03B") | 
                                                startsWith(bm_atc_medi_code_2, "C03B") | 
                                                startsWith(bm_atc_medi_code_3, "C03B") | 
                                                startsWith(bm_atc_medi_code_4, "C03B") | 
                                                startsWith(bm_atc_medi_code_5, "C03B") | 
                                                startsWith(bm_atc_medi_code_6, "C03B") | 
                                                startsWith(bm_atc_medi_code_7, "C03B") | 
                                                startsWith(bm_atc_medi_code_8, "C03B") | 
                                                startsWith(bm_atc_medi_code_9, "C03B") | 
                                                startsWith(bm_atc_medi_code_10, "C03B") | 
                                                startsWith(bm_atc_medi_code_11, "C03B") | 
                                                startsWith(bm_atc_medi_code_12, "C03B") | 
                                                startsWith(bm_atc_medi_code_13, "C03B") | 
                                                startsWith(bm_atc_medi_code_14, "C03B") | 
                                                startsWith(bm_atc_medi_code_15, "C03B") | 
                                                startsWith(bm_atc_medi_code_16, "C03B") | 
                                                startsWith(bm_atc_medi_code_17, "C03B") | 
                                                startsWith(bm_atc_medi_code_18, "C03B") | 
                                                startsWith(bm_atc_medi_code_19, "C03B") | 
                                                startsWith(bm_atc_medi_code_20, "C03B") | 
                                                startsWith(bm_atc_medi_code_21, "C03B") | 
                                                startsWith(bm_atc_medi_code_22, "C03B") | 
                                                startsWith(bm_atc_medi_code_23, "C03B") | 
                                                startsWith(bm_atc_medi_code_24, "C03B") | 
                                                startsWith(bm_atc_medi_code_25, "C03B") | 
                                                startsWith(bm_atc_medi_code_26, "C03B") | 
                                                startsWith(bm_atc_medi_code_27, "C03B") | 
                                                startsWith(bm_atc_medi_code_28, "C03B") | 
                                                startsWith(bm_atc_medi_code_29, "C03B") | 
                                                startsWith(bm_atc_medi_code_30, "C03B") | 
                                                startsWith(bm_atc_medi_code_31, "C03B") | 
                                                startsWith(bm_atc_medi_code_32, "C03B"), 1, 
                                              ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

Titze <- Titze %>% mutate(bm_atc_03ea = ifelse(startsWith(bm_atc_medi_code_1, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_2, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_3, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_4, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_5, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_6, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_7, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_8, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_9, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_10, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_11, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_12, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_13, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_14, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_15, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_16, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_17, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_18, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_19, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_20, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_21, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_22, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_23, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_24, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_25, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_26, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_27, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_28, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_29, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_30, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_31, "C03EA") | 
                                                 startsWith(bm_atc_medi_code_32, "C03EA"), 1, 
                                               ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

Titze <- Titze %>% mutate(bm_atc_07b = ifelse(startsWith(bm_atc_medi_code_1, "C07B") | 
                                                startsWith(bm_atc_medi_code_2, "C07B") | 
                                                startsWith(bm_atc_medi_code_3, "C07B") | 
                                                startsWith(bm_atc_medi_code_4, "C07B") | 
                                                startsWith(bm_atc_medi_code_5, "C07B") | 
                                                startsWith(bm_atc_medi_code_6, "C07B") | 
                                                startsWith(bm_atc_medi_code_7, "C07B") | 
                                                startsWith(bm_atc_medi_code_8, "C07B") | 
                                                startsWith(bm_atc_medi_code_9, "C07B") | 
                                                startsWith(bm_atc_medi_code_10, "C07B") | 
                                                startsWith(bm_atc_medi_code_11, "C07B") | 
                                                startsWith(bm_atc_medi_code_12, "C07B") | 
                                                startsWith(bm_atc_medi_code_13, "C07B") | 
                                                startsWith(bm_atc_medi_code_14, "C07B") | 
                                                startsWith(bm_atc_medi_code_15, "C07B") | 
                                                startsWith(bm_atc_medi_code_16, "C07B") | 
                                                startsWith(bm_atc_medi_code_17, "C07B") | 
                                                startsWith(bm_atc_medi_code_18, "C07B") | 
                                                startsWith(bm_atc_medi_code_19, "C07B") | 
                                                startsWith(bm_atc_medi_code_20, "C07B") | 
                                                startsWith(bm_atc_medi_code_21, "C07B") | 
                                                startsWith(bm_atc_medi_code_22, "C07B") | 
                                                startsWith(bm_atc_medi_code_23, "C07B") | 
                                                startsWith(bm_atc_medi_code_24, "C07B") | 
                                                startsWith(bm_atc_medi_code_25, "C07B") | 
                                                startsWith(bm_atc_medi_code_26, "C07B") | 
                                                startsWith(bm_atc_medi_code_27, "C07B") | 
                                                startsWith(bm_atc_medi_code_28, "C07B") | 
                                                startsWith(bm_atc_medi_code_29, "C07B") | 
                                                startsWith(bm_atc_medi_code_30, "C07B") | 
                                                startsWith(bm_atc_medi_code_31, "C07B") | 
                                                startsWith(bm_atc_medi_code_32, "C07B"), 1, 
                                              ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

Titze <- Titze %>% mutate(bm_atc_07d = ifelse(startsWith(bm_atc_medi_code_1, "C07D") | 
                                                startsWith(bm_atc_medi_code_2, "C07D") | 
                                                startsWith(bm_atc_medi_code_3, "C07D") | 
                                                startsWith(bm_atc_medi_code_4, "C07D") | 
                                                startsWith(bm_atc_medi_code_5, "C07D") | 
                                                startsWith(bm_atc_medi_code_6, "C07D") | 
                                                startsWith(bm_atc_medi_code_7, "C07D") | 
                                                startsWith(bm_atc_medi_code_8, "C07D") | 
                                                startsWith(bm_atc_medi_code_9, "C07D") | 
                                                startsWith(bm_atc_medi_code_10, "C07D") | 
                                                startsWith(bm_atc_medi_code_11, "C07D") | 
                                                startsWith(bm_atc_medi_code_12, "C07D") | 
                                                startsWith(bm_atc_medi_code_13, "C07D") | 
                                                startsWith(bm_atc_medi_code_14, "C07D") | 
                                                startsWith(bm_atc_medi_code_15, "C07D") | 
                                                startsWith(bm_atc_medi_code_16, "C07D") | 
                                                startsWith(bm_atc_medi_code_17, "C07D") | 
                                                startsWith(bm_atc_medi_code_18, "C07D") | 
                                                startsWith(bm_atc_medi_code_19, "C07D") | 
                                                startsWith(bm_atc_medi_code_20, "C07D") | 
                                                startsWith(bm_atc_medi_code_21, "C07D") | 
                                                startsWith(bm_atc_medi_code_22, "C07D") | 
                                                startsWith(bm_atc_medi_code_23, "C07D") | 
                                                startsWith(bm_atc_medi_code_24, "C07D") | 
                                                startsWith(bm_atc_medi_code_25, "C07D") | 
                                                startsWith(bm_atc_medi_code_26, "C07D") | 
                                                startsWith(bm_atc_medi_code_27, "C07D") | 
                                                startsWith(bm_atc_medi_code_28, "C07D") | 
                                                startsWith(bm_atc_medi_code_29, "C07D") | 
                                                startsWith(bm_atc_medi_code_30, "C07D") | 
                                                startsWith(bm_atc_medi_code_31, "C07D") | 
                                                startsWith(bm_atc_medi_code_32, "C07D"), 1, 
                                              ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

Titze <- Titze %>% mutate(bm_atc_thiazide = ifelse(bm_atc_03a == 1 | bm_atc_03b == 1 | bm_atc_03ea == 1 | bm_atc_07b == 1 | bm_atc_07d == 1, 1, ifelse (bm_atc_03a == 2 & bm_atc_03b == 2 & bm_atc_03ea == 2 & bm_atc_07b == 2 & bm_atc_07d == 2, 2, 9999)))

##### Aldosteronantagonisten C03DA, C03EC, C03ED
Titze <- Titze %>% mutate(bm_atc_03da = ifelse(startsWith(bm_atc_medi_code_1, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_2, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_3, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_4, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_5, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_6, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_7, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_8, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_9, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_10, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_11, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_12, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_13, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_14, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_15, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_16, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_17, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_18, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_19, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_20, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_21, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_22, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_23, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_24, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_25, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_26, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_27, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_28, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_29, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_30, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_31, "C03DA") | 
                                                 startsWith(bm_atc_medi_code_32, "C03DA"), 1, 
                                               ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

Titze <- Titze %>% mutate(bm_atc_03ec = ifelse(startsWith(bm_atc_medi_code_1, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_2, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_3, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_4, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_5, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_6, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_7, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_8, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_9, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_10, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_11, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_12, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_13, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_14, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_15, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_16, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_17, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_18, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_19, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_20, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_21, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_22, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_23, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_24, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_25, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_26, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_27, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_28, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_29, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_30, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_31, "C03EC") | 
                                                 startsWith(bm_atc_medi_code_32, "C03EC"), 1, 
                                               ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

Titze <- Titze %>% mutate(bm_atc_03ed = ifelse(startsWith(bm_atc_medi_code_1, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_2, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_3, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_4, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_5, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_6, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_7, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_8, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_9, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_10, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_11, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_12, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_13, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_14, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_15, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_16, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_17, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_18, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_19, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_20, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_21, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_22, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_23, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_24, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_25, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_26, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_27, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_28, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_29, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_30, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_31, "C03ED") | 
                                                 startsWith(bm_atc_medi_code_32, "C03ED"), 1, 
                                               ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

Titze <- Titze %>% mutate(bm_atc_aldoster = ifelse(bm_atc_03da == 1 | bm_atc_03ec == 1 | bm_atc_03ed == 1, 1, ifelse (bm_atc_03da == 2 & bm_atc_03ec == 2 & bm_atc_03ed == 2, 2, 9999)))                                                               

##### Schleifendiuretika C03C, C03EB, C03ED
Titze <- Titze %>% mutate(bm_atc_03c = ifelse(startsWith(bm_atc_medi_code_1, "C03C") | 
                                                startsWith(bm_atc_medi_code_2, "C03C") | 
                                                startsWith(bm_atc_medi_code_3, "C03C") | 
                                                startsWith(bm_atc_medi_code_4, "C03C") | 
                                                startsWith(bm_atc_medi_code_5, "C03C") | 
                                                startsWith(bm_atc_medi_code_6, "C03C") | 
                                                startsWith(bm_atc_medi_code_7, "C03C") | 
                                                startsWith(bm_atc_medi_code_8, "C03C") | 
                                                startsWith(bm_atc_medi_code_9, "C03C") | 
                                                startsWith(bm_atc_medi_code_10, "C03C") | 
                                                startsWith(bm_atc_medi_code_11, "C03C") | 
                                                startsWith(bm_atc_medi_code_12, "C03C") | 
                                                startsWith(bm_atc_medi_code_13, "C03C") | 
                                                startsWith(bm_atc_medi_code_14, "C03C") | 
                                                startsWith(bm_atc_medi_code_15, "C03C") | 
                                                startsWith(bm_atc_medi_code_16, "C03C") | 
                                                startsWith(bm_atc_medi_code_17, "C03C") | 
                                                startsWith(bm_atc_medi_code_18, "C03C") | 
                                                startsWith(bm_atc_medi_code_19, "C03C") | 
                                                startsWith(bm_atc_medi_code_20, "C03C") | 
                                                startsWith(bm_atc_medi_code_21, "C03C") | 
                                                startsWith(bm_atc_medi_code_22, "C03C") | 
                                                startsWith(bm_atc_medi_code_23, "C03C") | 
                                                startsWith(bm_atc_medi_code_24, "C03C") | 
                                                startsWith(bm_atc_medi_code_25, "C03C") | 
                                                startsWith(bm_atc_medi_code_26, "C03C") | 
                                                startsWith(bm_atc_medi_code_27, "C03C") | 
                                                startsWith(bm_atc_medi_code_28, "C03C") | 
                                                startsWith(bm_atc_medi_code_29, "C03C") | 
                                                startsWith(bm_atc_medi_code_30, "C03C") | 
                                                startsWith(bm_atc_medi_code_31, "C03C") | 
                                                startsWith(bm_atc_medi_code_32, "C03C"), 1, 
                                              ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

Titze <- Titze %>% mutate(bm_atc_03eb = ifelse(startsWith(bm_atc_medi_code_1, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_2, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_3, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_4, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_5, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_6, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_7, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_8, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_9, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_10, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_11, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_12, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_13, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_14, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_15, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_16, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_17, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_18, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_19, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_20, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_21, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_22, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_23, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_24, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_25, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_26, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_27, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_28, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_29, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_30, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_31, "C03EB") | 
                                                 startsWith(bm_atc_medi_code_32, "C03EB"), 1, 
                                               ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

Titze <- Titze %>% mutate(bm_atc_loop = ifelse(bm_atc_03c == 1 | bm_atc_03eb == 1 | bm_atc_03ed == 1, 1, ifelse (bm_atc_03c == 2 & bm_atc_03eb == 2 & bm_atc_03ed == 2, 2, 9999)))

#### Ca-Blocker C08C, C08G (Herzwirkung: C08D)
Titze <- Titze %>% mutate(bm_atc_08c = ifelse(startsWith(bm_atc_medi_code_1, "C08C") | 
                                                startsWith(bm_atc_medi_code_2, "C08C") | 
                                                startsWith(bm_atc_medi_code_3, "C08C") | 
                                                startsWith(bm_atc_medi_code_4, "C08C") | 
                                                startsWith(bm_atc_medi_code_5, "C08C") | 
                                                startsWith(bm_atc_medi_code_6, "C08C") | 
                                                startsWith(bm_atc_medi_code_7, "C08C") | 
                                                startsWith(bm_atc_medi_code_8, "C08C") | 
                                                startsWith(bm_atc_medi_code_9, "C08C") | 
                                                startsWith(bm_atc_medi_code_10, "C08C") | 
                                                startsWith(bm_atc_medi_code_11, "C08C") | 
                                                startsWith(bm_atc_medi_code_12, "C08C") | 
                                                startsWith(bm_atc_medi_code_13, "C08C") | 
                                                startsWith(bm_atc_medi_code_14, "C08C") | 
                                                startsWith(bm_atc_medi_code_15, "C08C") | 
                                                startsWith(bm_atc_medi_code_16, "C08C") | 
                                                startsWith(bm_atc_medi_code_17, "C08C") | 
                                                startsWith(bm_atc_medi_code_18, "C08C") | 
                                                startsWith(bm_atc_medi_code_19, "C08C") | 
                                                startsWith(bm_atc_medi_code_20, "C08C") | 
                                                startsWith(bm_atc_medi_code_21, "C08C") | 
                                                startsWith(bm_atc_medi_code_22, "C08C") | 
                                                startsWith(bm_atc_medi_code_23, "C08C") | 
                                                startsWith(bm_atc_medi_code_24, "C08C") | 
                                                startsWith(bm_atc_medi_code_25, "C08C") | 
                                                startsWith(bm_atc_medi_code_26, "C08C") | 
                                                startsWith(bm_atc_medi_code_27, "C08C") | 
                                                startsWith(bm_atc_medi_code_28, "C08C") | 
                                                startsWith(bm_atc_medi_code_29, "C08C") | 
                                                startsWith(bm_atc_medi_code_30, "C08C") | 
                                                startsWith(bm_atc_medi_code_31, "C08C") | 
                                                startsWith(bm_atc_medi_code_32, "C08C"), 1, 
                                              ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

Titze <- Titze %>% mutate(bm_atc_08g = ifelse(startsWith(bm_atc_medi_code_1, "C08G") | 
                                                startsWith(bm_atc_medi_code_2, "C08G") | 
                                                startsWith(bm_atc_medi_code_3, "C08G") | 
                                                startsWith(bm_atc_medi_code_4, "C08G") | 
                                                startsWith(bm_atc_medi_code_5, "C08G") | 
                                                startsWith(bm_atc_medi_code_6, "C08G") | 
                                                startsWith(bm_atc_medi_code_7, "C08G") | 
                                                startsWith(bm_atc_medi_code_8, "C08G") | 
                                                startsWith(bm_atc_medi_code_9, "C08G") | 
                                                startsWith(bm_atc_medi_code_10, "C08G") | 
                                                startsWith(bm_atc_medi_code_11, "C08G") | 
                                                startsWith(bm_atc_medi_code_12, "C08G") | 
                                                startsWith(bm_atc_medi_code_13, "C08G") | 
                                                startsWith(bm_atc_medi_code_14, "C08G") | 
                                                startsWith(bm_atc_medi_code_15, "C08G") | 
                                                startsWith(bm_atc_medi_code_16, "C08G") | 
                                                startsWith(bm_atc_medi_code_17, "C08G") | 
                                                startsWith(bm_atc_medi_code_18, "C08G") | 
                                                startsWith(bm_atc_medi_code_19, "C08G") | 
                                                startsWith(bm_atc_medi_code_20, "C08G") | 
                                                startsWith(bm_atc_medi_code_21, "C08G") | 
                                                startsWith(bm_atc_medi_code_22, "C08G") | 
                                                startsWith(bm_atc_medi_code_23, "C08G") | 
                                                startsWith(bm_atc_medi_code_24, "C08G") | 
                                                startsWith(bm_atc_medi_code_25, "C08G") | 
                                                startsWith(bm_atc_medi_code_26, "C08G") | 
                                                startsWith(bm_atc_medi_code_27, "C08G") | 
                                                startsWith(bm_atc_medi_code_28, "C08G") | 
                                                startsWith(bm_atc_medi_code_29, "C08G") | 
                                                startsWith(bm_atc_medi_code_30, "C08G") | 
                                                startsWith(bm_atc_medi_code_31, "C08G") | 
                                                startsWith(bm_atc_medi_code_32, "C08G"), 1, 
                                              ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

Titze <- Titze %>% mutate(bm_atc_ccb = ifelse(bm_atc_08c == 1 | bm_atc_08g == 1, 1, ifelse (bm_atc_08c == 2 & bm_atc_08g == 2, 2, 9999)))

#### b-Blocker C07
Titze <- Titze %>% mutate(bm_atc_bblocker = ifelse(startsWith(bm_atc_medi_code_1, "C07") | 
                                                     startsWith(bm_atc_medi_code_2, "C07") | 
                                                     startsWith(bm_atc_medi_code_3, "C07") | 
                                                     startsWith(bm_atc_medi_code_4, "C07") | 
                                                     startsWith(bm_atc_medi_code_5, "C07") | 
                                                     startsWith(bm_atc_medi_code_6, "C07") | 
                                                     startsWith(bm_atc_medi_code_7, "C07") | 
                                                     startsWith(bm_atc_medi_code_8, "C07") | 
                                                     startsWith(bm_atc_medi_code_9, "C07") | 
                                                     startsWith(bm_atc_medi_code_10, "C07") | 
                                                     startsWith(bm_atc_medi_code_11, "C07") | 
                                                     startsWith(bm_atc_medi_code_12, "C07") | 
                                                     startsWith(bm_atc_medi_code_13, "C07") | 
                                                     startsWith(bm_atc_medi_code_14, "C07") | 
                                                     startsWith(bm_atc_medi_code_15, "C07") | 
                                                     startsWith(bm_atc_medi_code_16, "C07") | 
                                                     startsWith(bm_atc_medi_code_17, "C07") | 
                                                     startsWith(bm_atc_medi_code_18, "C07") | 
                                                     startsWith(bm_atc_medi_code_19, "C07") | 
                                                     startsWith(bm_atc_medi_code_20, "C07") | 
                                                     startsWith(bm_atc_medi_code_21, "C07") | 
                                                     startsWith(bm_atc_medi_code_22, "C07") | 
                                                     startsWith(bm_atc_medi_code_23, "C07") | 
                                                     startsWith(bm_atc_medi_code_24, "C07") | 
                                                     startsWith(bm_atc_medi_code_25, "C07") | 
                                                     startsWith(bm_atc_medi_code_26, "C07") | 
                                                     startsWith(bm_atc_medi_code_27, "C07") | 
                                                     startsWith(bm_atc_medi_code_28, "C07") | 
                                                     startsWith(bm_atc_medi_code_29, "C07") | 
                                                     startsWith(bm_atc_medi_code_30, "C07") | 
                                                     startsWith(bm_atc_medi_code_31, "C07") | 
                                                     startsWith(bm_atc_medi_code_32, "C07"), 1, 
                                                   ifelse(bm_atc_medi_code_1 == "9999", 9999, 2)))

## Biopsie: Variable BL_ar_nie_biop (na_biopsie rein anamnestisch durch Pat, nicht geeignet zum Ausfüllen der NA)
Titze <- Titze %>% mutate(biopsy = ifelse(Titze$BL_ar_nie_biop == 1, 1, ifelse(Titze$BL_ar_nie_biop == 2, 2, 9999)))

## Ursache CKD: obstruktiv = postrenal?
Titze <- Titze %>% mutate(ckd_diab = ifelse(Titze$BL_ar_dm_typ1 == 1 | Titze$BL_ar_dm_typ2 ==1 | Titze$BL_ar_diabnp_so == 1, 1, 2))
Titze <- Titze %>% mutate(ckd_vasc = ifelse(Titze$BL_ar_narteriensten == 1 | Titze$BL_ar_nephroskler ==1 | Titze$BL_ar_ninfarkt == 1 | Titze$BL_ar_vasknp_so == 1, 1, 2))
Titze <- Titze %>% mutate(ckd_syst = ifelse(Titze$BL_ar_m_wegener == 1 | Titze$BL_ar_mik_polyangiitis ==1 | Titze$BL_ar_l_erythematodes == 1 | Titze$BL_ar_sklerodermie == 1 | Titze$BL_ar_ttp == 1 | Titze$BL_ar_gicht == 1 | Titze$BL_ar_tbc == 1 | Titze$BL_ar_amyloidose == 1 | Titze$BL_ar_sarkoidose == 1 | Titze$BL_ar_system_so == 1, 1, 2))
Titze <- Titze %>% mutate(ckd_glom_prim = ifelse(Titze$BL_ar_memb_prolif_gn == 1 | Titze$BL_ar_rp_gn_pauci_im ==1 | Titze$BL_ar_rp_gn_anti_gbm == 1 | Titze$BL_ar_postinfekt_gn == 1 | Titze$BL_ar_fsgs == 1 | Titze$BL_ar_membran_gn == 1 | Titze$BL_ar_iga_nephritis == 1 | Titze$BL_ar_min_change == 1 | Titze$BL_ar_primgnp_so == 1, 1, 2))
Titze <- Titze %>% mutate(ckd_interst = ifelse(Titze$BL_ar_interst_nephrop == 1 | Titze$BL_ar_analgetika ==1 | Titze$BL_ar_interstnp_so == 1, 1, 2))
Titze <- Titze %>% mutate(ckd_heredit = ifelse(Titze$BL_ar_adpkd == 1 | Titze$BL_ar_m_fabry ==1 | Titze$BL_ar_heredit_erk== 1, 1, 2))
Titze <- Titze %>% mutate(ckd_aki = ifelse(Titze$BL_ar_akutem_nvers == 1 | Titze$BL_ar_akutnv_so ==1, 1, 2))
Titze <- Titze %>% mutate(ckd_single = ifelse(Titze$BL_ar_tumornephrek == 1 | Titze$BL_ar_lebendspen ==1 | Titze$BL_ar_and_nephrek == 1 | Titze$BL_ar_kongenital == 1 | Titze$BL_ar_einnierig_so == 1, 1, 2))
Titze <- Titze %>% mutate(ckd_oth = ifelse(Titze$BL_ar_sonstiges == 1, 1, 2))
Titze <- Titze %>% mutate(ckd_obstr = ifelse(Titze$BL_ar_nierensteine == 1 | Titze$BL_ar_rez_entzuend == 1 | Titze$BL_ar_blasenentleer == 1 | Titze$BL_ar_vesikou_reflux == 1 | Titze$BL_ar_postrenal_so == 1, 1, 2))
Titze <- Titze %>% mutate(ckd_lead = ifelse(is.na(Titze$BL_ar_fuehrend) == TRUE, "ckd_lead_uk", ifelse(BL_ar_fuehrend == 1 | BL_ar_fuehrend == 2 | BL_ar_fuehrend == 38, "ckd_diab", 
                                            ifelse((BL_ar_fuehrend >= 3 & BL_ar_fuehrend <= 5) | BL_ar_fuehrend == 39, "ckd_vask", 
                                                   ifelse((BL_ar_fuehrend >= 6 & BL_ar_fuehrend <= 14) | BL_ar_fuehrend == 40, "ckd_syst", 
                                                          ifelse((BL_ar_fuehrend >= 15 & BL_ar_fuehrend <= 21) | BL_ar_fuehrend == 37 | BL_ar_fuehrend == 40, "ckd_glom_prim", 
                                                                 ifelse(BL_ar_fuehrend == 22 | BL_ar_fuehrend == 23 | BL_ar_fuehrend == 42, "ckd_interst", 
                                                                        ifelse(BL_ar_fuehrend == 24 | BL_ar_fuehrend == 43, "ckd_aki", 
                                                                               ifelse ((BL_ar_fuehrend >= 25 & BL_ar_fuehrend <= 28) | BL_ar_fuehrend == 44, "ckd_single", 
                                                                                       ifelse(BL_ar_fuehrend >= 29 & BL_ar_fuehrend <= 31, "ckd_heredit", 
                                                                                              ifelse((BL_ar_fuehrend >= 32 & BL_ar_fuehrend <= 35) | BL_ar_fuehrend == 45, "ckd_obstr", "ckd_oth")))))))))))
## Diabetes mellitus: HbA1c
Titze <- Titze %>% mutate(diabetes = ifelse(Titze$x, 1, ifelse(Titze$x, 2, 3)))
## Hypertension
Titze <- Titze %>% mutate(hypertension = ifelse(Titze$BL_ku_sys > 140 | Titze$BL_ku_dia > 90, 1, ifelse(Titze$BL_ku_sys <= 140 & Titze$BL_ku_dia <= 90, 2, 3)))
### Kontrolle Hypertension aus Anamnese
table(Titze$aa_blutdr, Titze$Hypertension)

## Behandlungsdauer & Awareness: na_erst_info, ar_nephr_beh

## Kardiovask. Erkrankungen: Herklappe | Aneurysma | KHK | zerebrovask | pAVK
### Hypertension: Variable hypertension
### Herzklappenersatz
Titze <- Titze %>% mutate(valve = ifelse(Titze$aa_herzkla == 2 | Titze$aa_herzop == 2, 2, ifelse(Titze$aa_herzkla == 1, 1, 9999)))
### KHK: PTCA | Bypass | Infarkt
Titze <- Titze %>% mutate(coronary = ifelse((Titze$aa_herzop == 2 | Titze$aa_byp_op == 2) & Titze$aa_herzkath == 2 & Titze$aa_herzinf == 2, 2, ifelse(Titze$aa_byp_op == 1 | Titze$aa_herzkath == 1 | Titze$aa_herzinf == 1, 1, 9999)))
### Zerebrovask. Erkrankung: Carotisintervention | -operation | Stroke
Titze <- Titze %>% mutate(cerebrovasc = ifelse(Titze$aa_schlag == 1 | Titze$aa_carot_op == 1 | Titze$aa_carot_ball == 1, 1, ifelse(Titze$aa_schlag == 2 & Titze$aa_carot_op == 2 & Titze$aa_carot_ball == 2, 2, 9999)))
### pAVK: PTA | Y-Prothese | operativ Revask | Amputation
Titze <- Titze %>% mutate(pavk = ifelse((Titze$aa_yprothese == 2 | Titze$aa_durchbl_bein == 2) & Titze$aa_kontrastm == 2 & Titze$aa_amputat == 2, 2, ifelse(Titze$aa_kontrastm == 1 | Titze$aa_durchbl_bein == 1 | Titze$aa_yprothese == 1 | Titze$aa_amputat == 1, 1, 9999)))
### gesamt kardiovaskulär
Titze <- Titze %>% mutate(cardiovasc = ifelse(Titze$valve == 1 | Titze$aa_aneurysma == 1 | Titze$coronary == 1 | Titze$cerebrovasc == 1 | Titze$pavk == 1, 1, ifelse(Titze$valve == 2 & Titze$aa_aneurysma == 2 & Titze$coronary == 2 & Titze$cerebrovasc == 2 & Titze$pavk== 2, 2, 9999)))

## Bildung: higher = Abitur, medium = Realschule, low = Haupt/Volksschule/kein Abschluss
Titze <- Titze %>% mutate(education = ifelse(Titze$dem_schule == 1 | Titze$dem_schule == 2 | Titze$dem_schule == 5, 1, ifelse(Titze$dem_schule == 3, 2, ifelse(Titze$dem_schule == 4, 3, ifelse(Titze$dem_schule == 6, 4, 9999)))))

## Marking NA (numerical, character)
Titze <- Titze %>% na_if(9999)
Titze <- Titze %>% na_if("9999")

# Table 1: Baseline Characteristics
Titze %>%
  summarise_at(vars(BL_age), list(~ round(mean(., na.rm=TRUE),1)))
Titze %>%
  summarise_at(vars(BL_age), list(~ round(sd(., na.rm=TRUE),1)))
Titze %>%
  summarise_at(vars(aa_stroke, aa_myocard, aa_diabetes, aa_renal), list(~ count(.)))
table(Titze$aa_stroke, useNA = "always")
prop.table(table(Titze$aa_stroke))
table(Titze$aa_myocard, useNA = "always")
prop.table(table(Titze$aa_myocard))
table(Titze$aa_diabetes, useNA = "always")
prop.table(table(Titze$aa_diabetes))
table(Titze$aa_renal, useNA = "always")
prop.table(table(Titze$aa_renal))
table(Titze$aa_renal_stones, useNA = "always")
prop.table(table(Titze$aa_renal_stones))
table(Titze$aa_dialyse, useNA = "always")
prop.table(table(Titze$aa_dialyse))
table(Titze$aa_ntx, useNA = "always")
prop.table(table(Titze$aa_ntx))
table(Titze$smoking, useNA = "always")
prop.table(table(Titze$smoking))
Titze %>%
  summarise_at(vars(BL_ku_height, BL_ku_weight, BL_ku_bmi, BL_ku_ruhepuls, BL_ku_dia, BL_ku_sys, BL_ku_map), list(~ round(mean(., na.rm=TRUE),1)))
Titze %>%
  summarise_at(vars(BL_ku_height, BL_ku_weight, BL_ku_bmi, BL_ku_ruhepuls, BL_ku_dia, BL_ku_sys, BL_ku_map), list(~ round(sd(., na.rm=TRUE),1)))
table(Titze$BL_ku_bmi_cat, useNA = "always")
prop.table(table(Titze$BL_ku_bmi_cat))
table(Titze$BL_ku_rr_130, useNA = "always")
prop.table(table(Titze$BL_ku_rr_130))
table(Titze$BL_ku_rr_140, useNA = "always")
prop.table(table(Titze$BL_ku_rr_140))
Titze %>%
  summarise_at(vars(BL_creavalue, BL_cysvalue, BL_gfr_mdrd, BL_uacr), list(~ round(mean(., na.rm=TRUE),1)))
Titze %>%
  summarise_at(vars(BL_creavalue, BL_cysvalue, BL_gfr_mdrd, BL_uacr), list(~ round(sd(., na.rm=TRUE),1)))
table(Titze$BL_gfr_mdrd_cat, useNA = "always")
prop.table(table(Titze$BL_gfr_mdrd_cat))
table(Titze$BL_uacr_cat, useNA = "always")
prop.table(table(Titze$BL_uacr_cat))
table(Titze$bm_atc_ace, useNA = "always")
prop.table(table(Titze$bm_atc_ace))
table(Titze$bm_atc_arb, useNA = "always")
prop.table(table(Titze$bm_atc_arb))
table(Titze$bm_atc_raas_comb, useNA = "always")
prop.table(table(Titze$bm_atc_raas_comb))
table(Titze$bm_atc_raas, useNA = "always")
prop.table(table(Titze$bm_atc_raas))
table(Titze$bm_atc_diuretics, useNA = "always")
prop.table(table(Titze$bm_atc_diuretics))
table(Titze$bm_atc_thiazide, useNA = "always")
prop.table(table(Titze$bm_atc_thiazide))
table(Titze$bm_atc_aldoster, useNA = "always")
prop.table(table(Titze$bm_atc_aldoster))
table(Titze$bm_atc_loop, useNA = "always")
prop.table(table(Titze$bm_atc_loop))
table(Titze$bm_atc_ccb, useNA = "always")
prop.table(table(Titze$bm_atc_ccb))
table(Titze$bm_atc_bblocker, useNA = "always")
prop.table(table(Titze$bm_atc_bblocker))
table(Titze$bm_atc_diuretics, useNA = "always")
prop.table(table(Titze$bm_atc_diuretics))
table(Titze$biopsy, useNA = "always")
prop.table(table(Titze$biopsy))

## Subsetting
Titze_fem <- Titze %>% subset(dem_sex == "Female")
Titze_male <- Titze %>% subset(dem_sex == "Male")
### Wdh Table 1 für Subsets
Titze_fem %>%
  group_by(diabetes) %>%
  summarise_at(vars(BL_ku_bmi), list(~ mean(., na.rm=TRUE)))

# Figure 1: GFR and albuminuria categories
table(Titze$BL_gfr_mdrd_cat, Titze$BL_uacr_cat, useNA = "always")
x <- table(Titze$BL_gfr_mdrd_cat, Titze$BL_uacr_cat)
round(prop.table(x)*100,1)


# Table 2: table 1 grouped by inclusion criteria 
## Subsetting
Titze_gfr <- Titze %>% subset(BL_gfr_mdrd <= 60)
Titze_protein <- Titze %>% subset(BL_uacr > 300 | BL_ualbuvalue )

# Figure 2: leading causes of CKD
## Subsetting

# Table 3: causes of CKD leading & additional

# Figure 3: patient awareness

# Suppl. table 2: GFR categories


# Missinganalysen
## Anzahl Missing 
### Anzahl Missing gesamt
sum(is.na(Titze))

### Anzahl Missing pro Variable (tabellarisch)
Titze_Miss_Sum <- miss_var_summary(Titze)
View(Titze_Miss_Sum)
### Anthropometric Data (ku) > Nutzen der Messwerte (Missing durch Amputation, Verweigerung, Immobilität etc)
sum(is.na(Titze$BL_ku_gr_pat))
sum(is.na(Titze$BL_ku_groesse))
sum(is.na(Titze$BL_ku_gew_pat))
sum(is.na(Titze$BL_ku_gewicht))
sum(is.na(Titze$BL_ku_bmi))

## Korrelationsmatrix der Missings (tabellarisch)
Titze_Miss <- as.data.frame(abs(is.na(Titze)))
Titze_Miss_Cor <- cor(Titze_Miss)
View(Titze_Miss_Cor)

## Visualisierung
Titze %>% missing_plot()