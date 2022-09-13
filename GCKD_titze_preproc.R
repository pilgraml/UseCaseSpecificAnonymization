# Packages 
pacman::p_load(tidyr, stringr, dplyr, openxlsx, naniar, emmeans, multcomp, 
               plyr, finalfit, ggplot2, tibble, MatchIt, optmatch, lmtest, sandwich,
               mice, tidyverse, tidyselect, summarytools, scales, gridExtra, 
               lubridate, eeptools, kidney.epi, nephro)
# Datensatz
setwd("Z:/GCKD/")
Titze <- as_tibble(read.xlsx("GCKD_df.xlsx", sep = ";"))
dim(Titze)
View(Titze)

# Data Preprocessing
## Alter: Berechnung aus Visitendatum + Geburtsdatum, bei NA Visitendatum = Screeningdatum
Titze_visdat_mis <- Titze[is.na(Titze$ein_visdat),]
Titze <- Titze %>% mutate(ein_visdat = ifelse(is.na(Titze$ein_visdat) == TRUE, scr_egfr_dat, ein_visdat))
Titze$ein_visdat <- as.Date(Titze$ein_visdat, format = "%d.%m.%Y")
Titze$dem_geburt <- as.Date(Titze$dem_geburt, format = "%d.%m.%Y")
Titze$BL_age <- floor(age_calc(Titze$dem_geburt, enddate = Titze$ein_visdat, units = "years"))
## Familienanamnese
### Stroke, Myokardinfarkt, Hypertension, Diabetes, Nierenerkrankung, Nierensteine, Dialyse, NTx
Titze <- Titze %>% mutate(aa_stroke = ifelse(aa_schlag2 > 0, 1, 2))
Titze <- Titze %>% mutate(aa_myocard = ifelse(aa_herzinf2 > 0, 1, 2))
Titze <- Titze %>% mutate(aa_hypertens = ifelse(aa_blutdr2 > 0, 1, 2))
Titze <- Titze %>% mutate(aa_diabetes = ifelse(aa_blutzu2 > 0, 1, 2))
Titze <- Titze %>% mutate(aa_renal = ifelse(aa_nierenerk_1 > 0, 1, 2))
Titze <- Titze %>% mutate(aa_renal_stones = ifelse(is.na(Titze$aa_nie_stein_1) == TRUE & aa_renal == 2, aa_renal, ifelse (aa_nie_stein_1 > 0, 1, 2)))
Titze <- Titze %>% mutate(aa_dialyse = ifelse(is.na(Titze$aa_dialyse_1) == TRUE & aa_renal == 2, aa_renal, ifelse (aa_dialyse_1 > 0, 1, 2)))
Titze <- Titze %>% mutate(aa_ntx = ifelse(is.na(Titze$aa_transpl_1) == TRUE & aa_renal == 2, aa_renal, ifelse (aa_transpl_1 > 0, 1, 2)))
## Geschlecht > unterschiedl. Labeling für GFR Packages
Titze$dem_sex <- mapvalues(Titze$dem_geschl, from = c("1", "2"), to = c("Male", "Female"))
Titze$dem_sex <- factor(Titze$dem_sex, levels = c("Male", "Female"))
Titze$dem_sex_2 <- mapvalues(Titze$dem_geschl, from = c("1", "2"), to = c("1", "0"))
Titze$dem_sex_2 <- as.integer(Titze$dem_sex_2)
## Rauchstatus
Titze <- Titze %>% mutate(smoking = ifelse(BL_pf_je_tabak == 2, 1, ifelse(BL_pf_je_tabak == 1 & BL_pf_akt_tabak == 1, 2, ifelse(BL_pf_je_tabak == 1 & (BL_pf_akt_tabak == 2 | BL_pf_akt_tabak == 3), 3, 9999))))
## Hospitalisierung
Titze <- Titze %>% mutate(hospital = ifelse(aa_beh_kh == 1, 1, ifelse(aa_beh_kh == 2, 2, 9999)))
## BMI: Aggregierte Variable durch Data Management (Missings durch Pat Angaben ersetzt, Korrektur für Amputation)
Titze$BL_ku_groesse_cm <- as.numeric(Titze$BL_ku_groesse)
Titze$BL_ku_gr_pat_cm <- as.numeric(Titze$BL_ku_gr_pat)
Titze <- Titze %>% mutate(BL_ku_height_cm = ifelse(is.na(Titze$BL_ku_groesse_cm) == TRUE, BL_ku_gr_pat_cm, BL_ku_groesse_cm))
Titze <- Titze %>% mutate(BL_ku_weight = ifelse(is.na(Titze$BL_ku_gewicht) == TRUE, BL_ku_gew_pat, BL_ku_gewicht))
Titze$BL_ku_bmi <- as.numeric(Titze$BL_bmi_korr)
#Titze <- Titze %>% mutate(BL_ku_bmi_cat = ifelse(BL_ku_bmi <= 25, 1, ifelse(BL_ku_bmi > 25 & BL_ku_bmi <= 30, 2, 3)))
## Blutdruck
Titze$BL_ku_sys <- as.numeric((Titze$BL_ku_mess1_sys_1 + Titze$BL_ku_mess2_sys_1 + Titze$BL_ku_mess3_sys)/3)
Titze$BL_ku_dia <- as.numeric((Titze$BL_ku_mess1_dia_1 + Titze$BL_ku_mess2_dia_1 + Titze$BL_ku_mess3_dia)/3)
Titze$BL_ku_map <- as.numeric((2*Titze$BL_ku_dia + Titze$BL_ku_sys)/3)
## GFR 
### Datatype und Unit (Kreatinin > mg/dl, Cystatin > mg/l)
Titze$BL_creavalue <- as.numeric(Titze$BL_creavalue)
Titze$BL_cysvalue <- as.numeric(Titze$BL_cysvalue)
### Spalte Ethnicity
Titze$dem_ethn = 0
### Berechnung mit package nephro (package kidney.epi: falsche Berechnung)
#### MRDR (4-Variablen-Formel)
Titze$BL_gfr_mdrd <- MDRD4(creatinine = Titze$BL_creavalue, age = Titze$BL_age, sex = Titze$dem_sex_2, ethnicity = Titze$dem_ethn)
#Titze <- Titze %>% mutate(BL_gfr_mdrd_cat = ifelse(BL_gfr_mdrd >= 90, "G1", ifelse(BL_gfr_mdrd >= 60 & BL_gfr_mdrd < 90, "G2", ifelse(BL_gfr_mdrd >= 45 & BL_gfr_mdrd < 60, "G3a", ifelse(BL_gfr_mdrd >= 30 & BL_gfr_mdrd < 45, "G3b", ifelse(BL_gfr_mdrd >= 15 & BL_gfr_mdrd < 30, "G4", "G5"))))))
#### CKD-EPI Krea
Titze$BL_gfr_ckdepi <- CKDEpi.creat(creatinine = Titze$BL_creavalue, age = Titze$BL_age, sex = Titze$dem_sex_2, ethnicity = Titze$dem_ethn)
#Titze <- Titze %>% mutate(BL_gfr_ckdepi_cat = ifelse(BL_gfr_ckdepi >= 90, "G1", ifelse(BL_gfr_ckdepi >= 60 & BL_gfr_ckdepi < 90, "G2", ifelse(BL_gfr_ckdepi >= 45 & BL_gfr_ckdepi < 60, "G3a", ifelse(BL_gfr_ckdepi >= 30 & BL_gfr_ckdepi < 45, "G3b", ifelse(BL_gfr_ckdepi >= 15 & BL_gfr_ckdepi < 30, "G4", "G5"))))))
#### CKD-EPI Cys
Titze$BL_gfr_ckdepi_cys <- CKDEpi.cys(cystatin = Titze$BL_cysvalue, age = Titze$BL_age, sex = Titze$dem_sex_2)
#Titze <- Titze %>% mutate(BL_gfr_ckdepi_cys_cat = ifelse(BL_gfr_ckdepi_cys >= 90, "G1", ifelse(BL_gfr_ckdepi_cys >= 60 & BL_gfr_ckdepi_cys < 90, "G2", ifelse(BL_gfr_ckdepi_cys >= 45 & BL_gfr_ckdepi_cys < 60, "G3a", ifelse(BL_gfr_ckdepi_cys >= 30 & BL_gfr_ckdepi_cys < 45, "G3b", ifelse(BL_gfr_ckdepi_cys >= 15 & BL_gfr_ckdepi_cys < 30, "G4", "G5"))))))
#### CKD-EPI Cys-Crea
Titze$BL_gfr_ckdepi_creacys <- CKDEpi.creat.cys(creatinine = Titze$BL_creavalue, cystatin = Titze$BL_cysvalue, age = Titze$BL_age, sex = Titze$dem_sex_2, ethnicity = Titze$dem_ethn)
#Titze <- Titze %>% mutate(BL_gfr_ckdepi_creacys_cat = ifelse(BL_gfr_ckdepi_creacys >= 90, "G1", ifelse(BL_gfr_ckdepi_creacys >= 60 & BL_gfr_ckdepi_creacys < 90, "G2", ifelse(BL_gfr_ckdepi_creacys >= 45 & BL_gfr_ckdepi_creacys < 60, "G3a", ifelse(BL_gfr_ckdepi_creacys >= 30 & BL_gfr_ckdepi_creacys < 45, "G3b", ifelse(BL_gfr_ckdepi_creacys >= 15 & BL_gfr_ckdepi_creacys < 30, "G4", "G5"))))))
## Urin Alb/Kreatinin > mg/gKrea: Urin Albumin > mg/l, Urin Kreatinin mg/dl > Umwandlung in g/l
Titze$BL_ucreavalue <- as.numeric(Titze$BL_ucreavalue)
Titze$BL_ualbuvalue <- as.numeric(Titze$BL_ualbuvalue)
Titze$BL_ucreavalue_gl <- as.numeric(Titze$BL_ucreavalue/100)
Titze$BL_uacr <- as.numeric(Titze$BL_ualbuvalue/Titze$BL_ucreavalue_gl)
#Titze <- Titze %>% mutate(BL_uacr_cat = ifelse(BL_uacr < 30, "A1", ifelse(BL_uacr >= 30 & BL_uacr <= 300, "A2", "A3")))
## Medikation: Aggregierte Variable durch Data Management med_
Titze <- Titze %>% mutate(BL_med_antihypert = ifelse(Titze$BL_med_bp == 1, 1, ifelse(Titze$BL_med_bp == 0, 2, 9999)))
Titze <- Titze %>% mutate(BL_med_raas_ace = ifelse(Titze$BL_med_bp_ace == 1, 1, ifelse(Titze$BL_med_bp_ace == 0, 2, 9999)))
Titze <- Titze %>% mutate(BL_med_raas_at1 = ifelse(Titze$BL_med_bp_arb == 1, 1, ifelse(Titze$BL_med_bp_arb == 0, 2, 9999)))
Titze <- Titze %>% mutate(BL_med_caanta = ifelse(Titze$BL_med_bpcalciumanta == 1, 1, ifelse(Titze$BL_med_bpcalciumanta == 0, 2, 9999)))
Titze <- Titze %>% mutate(BL_med_bblocker = ifelse(Titze$BL_med_bpbetablock == 1, 1, ifelse(Titze$BL_med_bpbetablock == 0, 2, 9999)))
Titze <- Titze %>% mutate(BL_med_raas_single= ifelse(Titze$BL_med_einzel_ras == 1, 1, ifelse(Titze$BL_med_einzel_ras == 0, 2, 9999)))
Titze <- Titze %>% mutate(BL_med_raas_double = ifelse(Titze$BL_med_doppel_ras == 1, 1, ifelse(Titze$BL_med_doppel_ras == 0, 2, 9999)))
Titze <- Titze %>% mutate(BL_med_diuretic = ifelse(Titze$BL_med_diuret == 1, 1, ifelse(Titze$BL_med_diuret == 0, 2, 9999)))
Titze <- Titze %>% mutate(BL_med_diuretic_loop = ifelse(Titze$BL_med_diuret_loop == 1, 1, ifelse(Titze$BL_med_diuret_loop == 0, 2, 9999)))
Titze <- Titze %>% mutate(BL_med_diuretic_thiazid = ifelse(Titze$BL_med_diuret_thiazid == 1, 1, ifelse(Titze$BL_med_diuret_thiazid == 0, 2, 9999)))
Titze <- Titze %>% mutate(BL_med_diuretic_ksparing = ifelse(Titze$BL_med_diuret_k_spar == 1, 1, ifelse(Titze$BL_med_diuret == 0, 2, 9999)))
Titze <- Titze %>% mutate(BL_med_diuretic_aldost = ifelse(Titze$BL_med_diuret_aldoanta == 1, 1, ifelse(Titze$BL_med_diuret_aldoanta == 0, 2, 9999)))
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
Titze <- Titze %>% mutate(ckd_uk = ifelse(Titze$BL_ar_unbekannt == 1, 1, 2))
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
## Vorerkrankungen
### Diabetes: aggregierte Variable durch Data Management
Titze <- Titze %>% mutate(diabetes = ifelse(Titze$BL_diabetic == 1, 1, 2))
### Hypertension (Kontrolle möglich über anamnest. aa_blutdr)
Titze <- Titze %>% mutate(hypertension = ifelse(Titze$BL_ku_sys > 140 | Titze$BL_ku_dia > 90 | Titze$BL_med_bp == 1, 1, 2))
Titze <- Titze %>% mutate(hypertension = ifelse(is.na(Titze$hypertension) == TRUE, 2, hypertension))
### Herzklappenersatz
Titze <- Titze %>% mutate(valve = ifelse(Titze$aa_herzkla == 2 | Titze$aa_herzop == 2, 2, ifelse(Titze$aa_herzkla == 1, 1, 9999)))
### KHK: PTCA | Bypass | Infarkt
Titze <- Titze %>% mutate(coronary = ifelse((Titze$aa_herzop == 2 | Titze$aa_byp_op == 2) & Titze$aa_herzkath == 2 & Titze$aa_herzinf == 2, 2, ifelse(Titze$aa_byp_op == 1 | Titze$aa_herzkath == 1 | Titze$aa_herzinf == 1, 1, 9999)))
Titze <- Titze %>% mutate(myocard = ifelse(Titze$aa_herzinf == 1, 1, ifelse(Titze$aa_herzinf == 2, 2, 9999)))
Titze <- Titze %>% mutate(bypass = ifelse(Titze$aa_herzop == 2 | Titze$aa_byp_op == 2, 2, ifelse(Titze$aa_byp_op == 1, 1, 9999)))
Titze <- Titze %>% mutate(ptca = ifelse(Titze$aa_herzkath == 1, 1, ifelse(Titze$aa_herzkath == 2, 2, 9999)))
### Zerebrovask. Erkrankung: Carotisintervention | -operation | Stroke
Titze <- Titze %>% mutate(cerebrovasc = ifelse(Titze$aa_schlag == 1 | Titze$aa_carot_op == 1 | Titze$aa_carot_ball == 1, 1, ifelse(Titze$aa_schlag == 2 & Titze$aa_carot_op == 2 & Titze$aa_carot_ball == 2, 2, 9999)))
Titze <- Titze %>% mutate(stroke = ifelse(Titze$aa_schlag == 1, 1, ifelse(Titze$aa_schlag == 2, 2, 9999)))
Titze <- Titze %>% mutate(carotic_surg = ifelse(Titze$aa_carot_op == 1, 1, ifelse(Titze$aa_carot_op == 2, 2, 9999)))
Titze <- Titze %>% mutate(carotic_interv = ifelse(Titze$aa_carot_ball == 1, 1, ifelse(Titze$aa_carot_ball == 2, 2, 9999)))
### pAVK: PTA | Y-Prothese | operativ Revask | Amputation
Titze <- Titze %>% mutate(pavk = ifelse(Titze$aa_durchbl_bein == 2 & Titze$aa_kontrastm == 2 & Titze$aa_amputat == 2, 2, ifelse(Titze$aa_kontrastm == 1 | Titze$aa_durchbl_bein == 1 | Titze$aa_yprothese == 1 | Titze$aa_amputat == 1, 1, 9999)))
Titze <- Titze %>% mutate(amput = ifelse(Titze$aa_amputat == 1, 1, ifelse(Titze$aa_amputat == 2, 2, 9999)))
Titze <- Titze %>% mutate(ygraft = ifelse(Titze$aa_yprothese == 1, 1, ifelse(Titze$aa_yprothese == 2 | Titze$aa_durchbl_bein == 2, 2, 9999)))
Titze <- Titze %>% mutate(pta = ifelse(Titze$aa_kontrastm == 1, 1, ifelse(Titze$aa_kontrastm == 2, 2, 9999)))
Titze <- Titze %>% mutate(pavk_surgery = ifelse(Titze$aa_bein_op == 1, 1, ifelse(Titze$aa_bein_op == 2 | Titze$aa_durchbl_bein == 2, 2, 9999)))
### gesamt kardiovaskulär: Herklappe | Aneurysma | KHK | zerebrovask | pAVK
Titze <- Titze %>% mutate(cardiovasc = ifelse(Titze$valve == 1 | Titze$aa_aneurysma == 1 | Titze$coronary == 1 | Titze$cerebrovasc == 1 | Titze$pavk == 1, 1, ifelse(Titze$valve == 2 & Titze$aa_aneurysma == 2 & Titze$coronary == 2 & Titze$cerebrovasc == 2 & Titze$pavk== 2, 2, 9999)))
## Behandlungsdauer & Awareness: na_erst_info, ar_nephr_beh
Titze <- Titze %>% mutate(awareness = ifelse(Titze$na_erst_info == 1, "< 6 mo", ifelse(Titze$na_erst_info == 2, "6 mo - < 1 yr", ifelse(Titze$na_erst_info == 3, "1 yr - < 3 yr", ifelse(Titze$na_erst_info == 4, "3 yr - < 5 yr", ifelse (Titze$na_erst_info == 5, ">= 5 yr", "9999"))))))
Titze <- Titze %>% mutate(treatment = ifelse(Titze$BL_ar_nephr_beh == 1, "< 6 mo", ifelse(Titze$BL_ar_nephr_beh == 2, "6 mo - < 1 yr", ifelse(Titze$BL_ar_nephr_beh == 3, "1 yr - < 3 yr", ifelse(Titze$BL_ar_nephr_beh == 4, "3 yr - < 5 yr", ifelse (Titze$BL_ar_nephr_beh == 5, ">= 5 yr", "9999"))))))
## Einschlusskritierien: scr_egfr_gr_60
Titze$scr_egfr_gr_60 <- mapvalues(Titze$scr_egfr_gr_60, from = c("ja <b> > 60 ml/ min/ 1,73 m² </b>", "nein <b> > 60 ml/ min/ 1,73 m² </b>"), to = c("2", "1"))
Titze$scr_egfr_gr_60 <- as.integer(Titze$scr_egfr_gr_60)
Titze$scr_egfr <- as.numeric(Titze$scr_egfr)
Titze <- Titze %>% mutate(incl_egfr = ifelse(is.na(Titze$scr_egfr) == TRUE, scr_egfr_gr_60, ifelse(Titze$scr_egfr > 60, 2, 1)))
## Bildung: higher = Abitur, medium = Realschule, low = Haupt/Volksschule/kein Abschluss
Titze <- Titze %>% mutate(education = ifelse(Titze$dem_schule == 1 | Titze$dem_schule == 2 | Titze$dem_schule == 5, 1, ifelse(Titze$dem_schule == 3, 2, ifelse(Titze$dem_schule == 4, 3, ifelse(Titze$dem_schule == 6, 4, 9999)))))

## Marking NA (numerical, character)
Titze <- Titze %>% na_if(9999)
Titze <- Titze %>% na_if("9999")

# Preprocessed Dataset
Titze_var <- c("BL_age", "dem_sex", "aa_stroke", "aa_myocard", "aa_hypertens", "aa_diabetes", "aa_renal", "aa_renal_stones", "aa_dialyse", 
               "aa_ntx", "smoking", "hospital", "BL_ku_height_cm", "BL_ku_weight", "BL_ku_bmi", "BL_ku_sys", "BL_ku_dia", "BL_ku_map", "BL_ku_ruhepuls",
               "BL_creavalue", "BL_cysvalue", "BL_gfr_mdrd", "BL_gfr_ckdepi", "BL_gfr_ckdepi_cys", "BL_gfr_ckdepi_creacys", "BL_uacr", "BL_med_raas_ace", 
               "BL_med_raas_at1", "BL_med_raas_single", "BL_med_raas_double", "BL_med_caanta", "BL_med_bblocker", "BL_med_diuretic", "BL_med_diuretic_loop", 
               "BL_med_diuretic_thiazid", "BL_med_diuretic_aldost", "biopsy", "ckd_diab", "ckd_vasc", "ckd_syst", "ckd_glom_prim", "ckd_interst", 
               "ckd_heredit", "ckd_aki", "ckd_single", "ckd_obstr", "ckd_oth", "ckd_uk", "ckd_lead", "diabetes", "hypertension",
               "valve", "coronary", "myocard", "bypass", "ptca", "cerebrovasc", "stroke", "carotic_surg", "carotic_interv", "pavk", "amput", "ygraft", "pta", "cardiovasc", 
               "pavk_surgery", "awareness", "treatment", "incl_egfr", "education")
GCKD_df1 <- Titze[,Titze_var]
Titze_var_QIred <- c("BL_age", "dem_sex", "aa_stroke", "aa_myocard", "aa_hypertens", "aa_diabetes", "aa_renal", "aa_renal_stones", "aa_dialyse", 
                     "aa_ntx", "smoking", "hospital", "BL_ku_bmi", "BL_ku_sys", "BL_ku_dia", "BL_ku_map", "BL_ku_ruhepuls",
                     "BL_creavalue", "BL_cysvalue", "BL_gfr_mdrd", "BL_gfr_ckdepi", "BL_gfr_ckdepi_cys", "BL_gfr_ckdepi_creacys", "BL_uacr", "BL_med_raas_ace", 
                     "BL_med_raas_at1", "BL_med_raas_single", "BL_med_raas_double", "BL_med_caanta", "BL_med_bblocker", "BL_med_diuretic", "BL_med_diuretic_loop", 
                     "BL_med_diuretic_thiazid", "BL_med_diuretic_aldost", "biopsy", "ckd_diab", "ckd_vasc", "ckd_syst", "ckd_glom_prim", "ckd_interst", 
                     "ckd_heredit", "ckd_aki", "ckd_single", "ckd_obstr", "ckd_oth", "ckd_uk", "ckd_lead", "diabetes", "hypertension",
                     "valve", "coronary", "myocard", "bypass", "ptca", "cerebrovasc", "stroke", "carotic_surg", "carotic_interv", "pavk", "amput", "ygraft", "pta", "cardiovasc", 
                     "pavk_surgery", "awareness", "treatment", "incl_egfr", "education")
GCKD_df1_QIred <- Titze[,Titze_var_QIred]

setwd("Z:/GCKD/")
write.xlsx(GCKD_df1, "GCKD_df1_origin.xlsx")
write.xlsx(GCKD_df1_QIred, "GCKD_df1_origin_QIred.xlsx")
