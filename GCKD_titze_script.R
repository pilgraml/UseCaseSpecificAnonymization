# Packages 
pacman::p_load(tidyr, stringr, dplyr, openxlsx, naniar, emmeans, multcomp, 
               plyr, finalfit, ggplot2, tibble, lmtest, sandwich,
               tidyverse, tidyselect, summarytools, scales, gridExtra, 
               lubridate, eeptools, gtsummary, flextable)

# Dataset
setwd("Z:/GCKD/")
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_df1_origin.xlsx", sep = ";"))
#GCKD_df1 <- as_tibble(read.xlsx("GCKD_df1_generic_k2s10.xlsx", sep = ";"))
GCKD_df1 <- as_tibble(read.xlsx("GCKD_df1_generic_k5s10.xlsx", sep = ";"))
dim(GCKD_df1)

# !! Anonymized dataset only !! 

## Marking NA
GCKD_df1 <- GCKD_df1 %>% na_if("*")
GCKD_df1 <- GCKD_df1 %>% na_if("NULL")
GCKD_df1 <- GCKD_df1 %>% na_if("null")
## Data Type
GCKD_df1$BL_ku_ruhepuls <- as.integer(GCKD_df1$BL_ku_ruhepuls)
GCKD_df1$BL_ku_dia <- as.numeric(GCKD_df1$BL_ku_dia)
GCKD_df1$BL_ku_sys <- as.numeric(GCKD_df1$BL_ku_sys)
GCKD_df1$BL_ku_map <- as.numeric(GCKD_df1$BL_ku_map)
GCKD_df1$BL_creavalue <- as.numeric(GCKD_df1$BL_creavalue)
GCKD_df1$BL_cysvalue <- as.numeric(GCKD_df1$BL_cysvalue)
GCKD_df1$BL_gfr_mdrd <- as.numeric(GCKD_df1$BL_gfr_mdrd)
GCKD_df1$BL_gfr_ckdepi <- as.numeric(GCKD_df1$BL_gfr_ckdepi)
GCKD_df1$BL_gfr_ckdepi_cys <- as.numeric(GCKD_df1$BL_gfr_ckdepi_cys)
GCKD_df1$BL_gfr_ckdepi_creacys <- as.numeric(GCKD_df1$BL_gfr_ckdepi_creacys)
GCKD_df1$BL_uacr <- as.numeric(GCKD_df1$BL_uacr)
## Distribution of Quasi-Identifiers
### Figure settings: fill="#73A1AA" in anonymized datasets
table(GCKD_df1$BL_age, useNA = "always")
prop.table(table(GCKD_df1$BL_age))
GCKD_df1 %>%
  filter(!is.na(BL_age)) %>% 
  ggplot(aes(x=as.factor(BL_age))) +
  geom_bar(color="white", fill="#73A1AA")
table(GCKD_df1$BL_ku_height_cm, useNA = "always")
prop.table(table(GCKD_df1$BL_ku_height_cm))
GCKD_df1 %>%
  filter(!is.na(BL_ku_height_cm)) %>% 
  ggplot(aes(x=as.factor(BL_ku_height_cm))) +
  geom_bar(color="white", fill="#73A1AA")
GCKD_df1$BL_ku_weight <- factor(GCKD_df1$BL_ku_weight, levels = c("[40, 60[", "[60, 80[", "[80, 100[", "[100, 120[", "[120, 140[", "[140, 160[", "[160, 180[", "[180, 200["))
table(GCKD_df1$BL_ku_weight, useNA = "always")
prop.table(table(GCKD_df1$BL_ku_weight))
GCKD_df1 %>%
  filter(!is.na(BL_ku_weight)) %>% 
  ggplot(aes(x=as.factor(BL_ku_weight))) +
  geom_bar(color="white", fill="#73A1AA")
GCKD_df1$BL_ku_bmi <- factor(GCKD_df1$BL_ku_bmi, levels = c("[5, 18.5[", "[18.5, 25[", "[25, 30[", "[30, 35[", "[35, 40[", "[40, 250["))
table(GCKD_df1$BL_ku_bmi, useNA = "always")
prop.table(table(GCKD_df1$BL_ku_bmi))
GCKD_df1 %>%
  filter(!is.na(BL_ku_bmi)) %>% 
  ggplot(aes(x=as.factor(BL_ku_bmi))) +
  geom_bar(color="white", fill="#73A1AA")
## Table 1
table(GCKD_df1$BL_age, useNA = "always")
prop.table(table(GCKD_df1$BL_age))
table(GCKD_df1$BL_ku_height_cm,  useNA = "always")
prop.table(table(GCKD_df1$BL_ku_height_cm))
table(GCKD_df1$BL_ku_weight,  useNA = "always")
prop.table(table(GCKD_df1$BL_ku_weight))
table(GCKD_df1$BL_ku_bmi,  useNA = "always")
prop.table(table(GCKD_df1$BL_ku_bmi))
table(GCKD_df1$BL_ku_bmi == "[5, 18.5[" | GCKD_df1$BL_ku_bmi == "[18.5, 25[",  useNA = "always")
prop.table(table(GCKD_df1$BL_ku_bmi == "[5, 18.5[" | GCKD_df1$BL_ku_bmi == "[18.5, 25["))
table(GCKD_df1$BL_ku_bmi == "[25, 30[",  useNA = "always")
prop.table(table(GCKD_df1$BL_ku_bmi == "[25, 30["))
table(GCKD_df1$BL_ku_bmi == "[30, 35[" | GCKD_df1$BL_ku_bmi == "[35, 40[" | GCKD_df1$BL_ku_bmi == "[40, 250[",  useNA = "always")
prop.table(table(GCKD_df1$BL_ku_bmi == "[30, 35[" | GCKD_df1$BL_ku_bmi == "[35, 40[" | GCKD_df1$BL_ku_bmi == "[40, 250["))
### Subsetting female
GCKD_df1_fem <- GCKD_df1 %>% subset(dem_sex == "Female")
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_age, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_age), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_height_cm,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_height_cm), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_weight,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_weight), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi == "[5, 18.5[" | GCKD_df1_fem$BL_ku_bmi == "[18.5, 25[",  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi == "[5, 18.5[" | GCKD_df1_fem$BL_ku_bmi == "[18.5, 25["), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi == "[25, 30[",  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi == "[25, 30["), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi == "[30, 35[" | GCKD_df1_fem$BL_ku_bmi == "[35, 40[" | GCKD_df1_fem$BL_ku_bmi == "[40, 250[",  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi == "[30, 35[" | GCKD_df1_fem$BL_ku_bmi == "[35, 40[" | GCKD_df1_fem$BL_ku_bmi == "[40, 250["), 1)
### Subsetting male
GCKD_df1_male <- GCKD_df1 %>% subset(dem_sex == "Male")
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_age, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_age), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_height_cm,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_height_cm), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_weight,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_weight), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi == "[5, 18.5[" | GCKD_df1_male$BL_ku_bmi == "[18.5, 25[",  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi == "[5, 18.5[" | GCKD_df1_male$BL_ku_bmi == "[18.5, 25["), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi == "[25, 30[",  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi == "[25, 30["), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi == "[30, 35[" | GCKD_df1_male$BL_ku_bmi == "[35, 40[" | GCKD_df1_male$BL_ku_bmi == "[40, 250[",  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi == "[30, 35[" | GCKD_df1_male$BL_ku_bmi == "[35, 40[" | GCKD_df1_male$BL_ku_bmi == "[40, 250["), 1)
## Table 2: Characteristics grouped by inclusion criteria 
table(GCKD_df1$incl_egfr, GCKD_df1$BL_age,  useNA = "always")
prop.table(table(GCKD_df1$incl_egfr, GCKD_df1$BL_age), 1)
GCKD_df1_more60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[60, 80[")
table(GCKD_df1_more60$incl_egfr, GCKD_df1_more60$cardiovasc == 1,  useNA = "always")
prop.table(table(GCKD_df1_more60$incl_egfr, GCKD_df1_more60$cardiovasc == 1))
GCKD_df1_less60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age == "[20, 40[" | GCKD_df1$BL_age == "[40, 60[")
table(GCKD_df1_less60$incl_egfr, GCKD_df1_less60$cardiovasc == 1,  useNA = "always")
prop.table(table(GCKD_df1_less60$incl_egfr, GCKD_df1_less60$cardiovasc == 1))
## Table 4: Cardiovascular disease
table(GCKD_df1_more60$cardiovasc == 1,  useNA = "always")
prop.table(table(GCKD_df1_more60$cardiovasc == 1))
table(GCKD_df1_less60$cardiovasc == 1,  useNA = "always")
prop.table(table(GCKD_df1_less60$cardiovasc == 1))
### Subsetting female
GCKD_df1_more60_fem <- GCKD_df1_more60 %>% subset(dem_sex == "Female")
table(GCKD_df1_more60_fem$diabetes, GCKD_df1_more60_fem$cardiovasc == 1,  useNA = "always")
prop.table(table(GCKD_df1_more60_fem$diabetes, GCKD_df1_more60_fem$cardiovasc == 1), 1)
GCKD_df1_less60_fem <- GCKD_df1_less60 %>% subset(dem_sex == "Female")
table(GCKD_df1_less60_fem$diabetes, GCKD_df1_less60_fem$cardiovasc == 1,  useNA = "always")
prop.table(table(GCKD_df1_less60_fem$diabetes, GCKD_df1_less60_fem$cardiovasc == 1), 1)
### Subsetting male
GCKD_df1_more60_male <- GCKD_df1_more60 %>% subset(dem_sex == "Male")
table(GCKD_df1_more60_male$diabetes, GCKD_df1_more60_male$cardiovasc == 1,  useNA = "always")
prop.table(table(GCKD_df1_more60_male$diabetes, GCKD_df1_more60_male$cardiovasc == 1), 1)
GCKD_df1_less60_male <- GCKD_df1_less60 %>% subset(dem_sex == "Male")
table(GCKD_df1_less60_male$diabetes, GCKD_df1_less60_male$cardiovasc == 1,  useNA = "always")
prop.table(table(GCKD_df1_less60_male$diabetes, GCKD_df1_less60_male$cardiovasc == 1), 1)
## Suppl. Table 3: Diabetic nephropathy
GCKD_df1_TableS3 <- GCKD_df1 
GCKD_df1_TableS3 <- GCKD_df1_TableS3 %>% mutate(DM = ifelse(GCKD_df1_TableS3$diabetes == 2, "No DM", ifelse(GCKD_df1_TableS3$ckd_lead == "ckd_diab", "DMwDN", "DMwoDN")))
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_age, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_age), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_height_cm,  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_height_cm), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_weight,  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_weight), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_bmi,  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_bmi), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_bmi == "[5, 18.5[" | GCKD_df1_TableS3$BL_ku_bmi == "[18.5, 25[",  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_bmi == "[5, 18.5[" | GCKD_df1_TableS3$BL_ku_bmi == "[18.5, 25["), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_bmi == "[25, 30[",  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_bmi == "[25, 30["), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_bmi == "[30, 35[" | GCKD_df1_TableS3$BL_ku_bmi == "[35, 40[" | GCKD_df1_TableS3$BL_ku_bmi == "[40, 250[",  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_bmi == "[30, 35[" | GCKD_df1_TableS3$BL_ku_bmi == "[35, 40[" | GCKD_df1_TableS3$BL_ku_bmi == "[40, 250["), 1)
## Suppl. Figure 1: Age distribution stratified
GCKD_df1_FigS1 <- GCKD_df1
GCKD_df1_FigS1 <- GCKD_df1_FigS1 %>% mutate(BL_age_cat = ifelse(
  GCKD_df1_FigS1$BL_age == "[20, 40[", 1, ifelse(GCKD_df1_FigS1$BL_age == "[40, 60[", 2, 3)))
GCKD_df1_FigS1 %>%
  filter(dem_sex == "Male") %>%
  ggplot(aes(x=as.factor(BL_age_cat))) +
  geom_bar(color="white", fill="#73A1AA", width=1.0)  +
  scale_y_continuous(limits=c(0,2100), breaks=c(0,300,600,900,1200,1500,1800,2100)) + 
  coord_flip() +
  theme(aspect.ratio = 3/2)
GCKD_df1_FigS1 %>%
  filter(dem_sex == "Female") %>%
  ggplot(aes(x=as.factor(BL_age_cat))) +
  geom_bar(color="white", fill="#73A1AA", alpha = 0.5, width=1.0)  +
  scale_y_continuous(limits=c(0,2100), breaks=c(0,300,600,900,1200,1500,1800,2100)) + 
  coord_flip() +
  theme(aspect.ratio = 3/2)
GCKD_df1_FigS1 %>%
  filter(!is.na(BL_age_cat)) %>%
  filter (diabetes == 1) %>%
  ggplot(aes(x=as.factor(BL_age_cat))) +
  geom_bar(color="white", fill="#73A1AA", width=1.0)  +
  scale_y_continuous(limits=c(0,2100), breaks=c(0,300,600,900,1200,1500,1800,2100)) + 
  coord_flip() +
  theme(aspect.ratio = 3/2)
GCKD_df1_FigS1 %>%
  filter(!is.na(BL_age_cat)) %>%
  filter(diabetes == 2) %>%
  ggplot(aes(x=as.factor(BL_age_cat))) +
  geom_bar(color="white", fill="#73A1AA", alpha = 0.5, width=1.0)  +
  scale_y_continuous(limits=c(0,2100), breaks=c(0,300,600,900,1200,1500,1800,2100)) + 
  coord_flip() +
  theme(aspect.ratio = 3/2)

# !! Original dataset only !!

## Distribution of Quasi-Identifiers
### Figure settings: fill="#B83674" in original dataset
sum(is.na(GCKD_df1$BL_age))
GCKD_df1 %>%
  summarise_at(vars(BL_age), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1 %>%
  summarise_at(vars(BL_age), list(~ round(sd(., na.rm=TRUE),1)))
ggplot(GCKD_df1, aes(x=BL_age)) +
  geom_histogram(binwidth=1, colour="white", fill="#B83674")
sum(is.na(GCKD_df1$BL_ku_height_cm))
GCKD_df1 %>%
  summarise_at(vars(BL_ku_height_cm), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1 %>%
  summarise_at(vars(BL_ku_height_cm), list(~ round(sd(., na.rm=TRUE),1)))
ggplot(GCKD_df1, aes(x=BL_ku_height_cm)) +
  geom_histogram(binwidth=1, colour="white", fill="#B83674")
sum(is.na(GCKD_df1$BL_ku_weight))
GCKD_df1 %>%
  summarise_at(vars(BL_ku_weight), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1 %>%
  summarise_at(vars(BL_ku_weight), list(~ round(sd(., na.rm=TRUE),1)))
ggplot(GCKD_df1, aes(x=BL_ku_weight)) +
  geom_histogram(binwidth=1, colour="white", fill="#B83674")
sum(is.na(GCKD_df1$BL_ku_bmi))
GCKD_df1 %>%
  summarise_at(vars(BL_ku_bmi), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1 %>%
  summarise_at(vars(BL_ku_bmi), list(~ round(sd(., na.rm=TRUE),1)))
ggplot(GCKD_df1, aes(x=BL_ku_bmi)) +
  geom_histogram(binwidth=1, colour="white", fill="#B83674")
## Table 1
GCKD_df1 %>%
  summarise_at(vars(BL_age), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1 %>%
  summarise_at(vars(BL_age), list(~ round(sd(., na.rm=TRUE),1)))
GCKD_df1 %>%
  summarise_at(vars(BL_ku_height_cm, BL_ku_weight, BL_ku_bmi), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1 %>%
  summarise_at(vars(BL_ku_height_cm, BL_ku_weight, BL_ku_bmi), list(~ round(sd(., na.rm=TRUE),1)))
table(GCKD_df1$BL_ku_bmi > 30,  useNA = "always")
prop.table(table(GCKD_df1$BL_ku_bmi > 30))
table(GCKD_df1$BL_ku_bmi <= 30 & GCKD_df1$BL_ku_bmi > 25,  useNA = "always")
prop.table(table(GCKD_df1$BL_ku_bmi <= 30 & GCKD_df1$BL_ku_bmi > 25))
table(GCKD_df1$BL_ku_bmi <= 25,  useNA = "always")
prop.table(table(GCKD_df1$BL_ku_bmi <= 25))
### Subsetting female
GCKD_df1_fem <- GCKD_df1 %>% subset(dem_sex == "Female")
GCKD_df1_fem %>% group_by(diabetes) %>%
  summarise_at(vars(BL_age), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1_fem %>% group_by(diabetes) %>%
  summarise_at(vars(BL_age), list(~ round(sd(., na.rm=TRUE),1)))
GCKD_df1_fem %>% group_by(diabetes) %>%
  summarise_at(vars(BL_ku_height_cm, BL_ku_weight, BL_ku_bmi), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1_fem %>% group_by(diabetes) %>%
  summarise_at(vars(BL_ku_height_cm, BL_ku_weight, BL_ku_bmi), list(~ round(sd(., na.rm=TRUE),1)))
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi > 30,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi > 30))
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi > 25 & GCKD_df1_fem$BL_ku_bmi <= 30,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi > 25 & GCKD_df1_fem$BL_ku_bmi <= 30))
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi <= 25,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_bmi <= 25))
### Subsetting male
GCKD_df1_male <- GCKD_df1 %>% subset(dem_sex == "Male")
GCKD_df1_male %>% group_by(diabetes) %>%
  summarise_at(vars(BL_age), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1_male %>% group_by(diabetes) %>%
  summarise_at(vars(BL_age), list(~ round(sd(., na.rm=TRUE),1)))
GCKD_df1_male %>% group_by(diabetes) %>%
  summarise_at(vars(BL_ku_height_cm, BL_ku_weight, BL_ku_bmi), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1_male %>% group_by(diabetes) %>%
  summarise_at(vars(BL_ku_height_cm, BL_ku_weight, BL_ku_bmi), list(~ round(sd(., na.rm=TRUE),1)))
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi > 30,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi > 30))
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi > 25 & GCKD_df1_male$BL_ku_bmi <= 30,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi > 25 & GCKD_df1_male$BL_ku_bmi <= 30))
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi <= 25,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_bmi <= 25))
## Table 2: Characteristics grouped by inclusion criteria 
GCKD_df1 %>% group_by(incl_egfr) %>%
  summarise_at(vars(BL_age), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1 %>% group_by(incl_egfr) %>%
  summarise_at(vars(BL_age), list(~ round(sd(., na.rm=TRUE),1)))
GCKD_df1_more60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age > 60)
table(GCKD_df1_more60$incl_egfr, GCKD_df1_more60$cardiovasc == 1,  useNA = "always")
prop.table(table(GCKD_df1_more60$incl_egfr, GCKD_df1_more60$cardiovasc == 1))
GCKD_df1_less60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age <= 60)
table(GCKD_df1_less60$incl_egfr, GCKD_df1_less60$cardiovasc == 1,  useNA = "always")
prop.table(table(GCKD_df1_less60$incl_egfr, GCKD_df1_less60$cardiovasc == 1))
## Table 4: Cardiovascular disease
GCKD_df1_more60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age > 60)
table(GCKD_df1_more60$cardiovasc == 1,  useNA = "always")
prop.table(table(GCKD_df1_more60$cardiovasc == 1))
GCKD_df1_less60 <- GCKD_df1 %>% subset(GCKD_df1$BL_age <= 60)
table(GCKD_df1_less60$cardiovasc == 1,  useNA = "always")
prop.table(table(GCKD_df1_less60$cardiovasc == 1))
### Subsetting female
GCKD_df1_more60_fem <- GCKD_df1_more60 %>% subset(dem_sex == "Female")
table(GCKD_df1_more60_fem$diabetes, GCKD_df1_more60_fem$cardiovasc == 1,  useNA = "always")
prop.table(table(GCKD_df1_more60_fem$diabetes, GCKD_df1_more60_fem$cardiovasc == 1), 1)
GCKD_df1_less60_fem <- GCKD_df1_less60 %>% subset(dem_sex == "Female")
table(GCKD_df1_less60_fem$diabetes, GCKD_df1_less60_fem$cardiovasc == 1,  useNA = "always")
prop.table(table(GCKD_df1_less60_fem$diabetes, GCKD_df1_less60_fem$cardiovasc == 1), 1)
### Subsetting male
GCKD_df1_more60_male <- GCKD_df1_more60 %>% subset(dem_sex == "Male")
table(GCKD_df1_more60_male$diabetes, GCKD_df1_more60_male$cardiovasc == 1,  useNA = "always")
prop.table(table(GCKD_df1_more60_male$diabetes, GCKD_df1_more60_male$cardiovasc == 1), 1)
GCKD_df1_less60_male <- GCKD_df1_less60 %>% subset(dem_sex == "Male")
table(GCKD_df1_less60_male$diabetes, GCKD_df1_less60_male$cardiovasc == 1,  useNA = "always")
prop.table(table(GCKD_df1_less60_male$diabetes, GCKD_df1_less60_male$cardiovasc == 1), 1)
## Suppl. Table 3: Diabetic nephropathy
GCKD_df1_TableS3 <- GCKD_df1 
GCKD_df1_TableS3 <- GCKD_df1_TableS3 %>% mutate(DM = ifelse(GCKD_df1_TableS3$diabetes == 2, "No DM", ifelse(GCKD_df1_TableS3$ckd_lead == "ckd_diab", "DMwDN", "DMwoDN")))
GCKD_df1_TableS3 %>% group_by(DM) %>%
  summarise_at(vars(BL_age), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1_TableS3 %>% group_by(DM) %>%
  summarise_at(vars(BL_age), list(~ round(sd(., na.rm=TRUE),1)))
GCKD_df1_TableS3 %>% group_by(DM) %>%
  summarise_at(vars(BL_ku_height_cm, BL_ku_weight, BL_ku_bmi), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1_TableS3 %>% group_by(DM) %>%
  summarise_at(vars(BL_ku_height_cm, BL_ku_weight, BL_ku_bmi), list(~ round(sd(., na.rm=TRUE),1)))
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_bmi > 30,  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_bmi > 30))
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_bmi > 25 & GCKD_df1_TableS3$BL_ku_bmi <= 30,  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_bmi > 25 & GCKD_df1_TableS3$BL_ku_bmi <= 30))
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_bmi <= 25,  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_bmi <= 25))
## Suppl. Figure 1: Age distribution stratified
GCKD_df1_FigS1 <- GCKD_df1
GCKD_df1_FigS1 <- GCKD_df1_FigS1 %>% mutate(
  BL_age_cat = ifelse(GCKD_df1_FigS1$BL_age < 20, 1, ifelse(GCKD_df1_FigS1$BL_age >= 20 & GCKD_df1_FigS1$BL_age < 30, 2, ifelse(
    GCKD_df1_FigS1$BL_age >= 30 & GCKD_df1_FigS1$BL_age < 40, 3, ifelse(GCKD_df1_FigS1$BL_age >= 40 & GCKD_df1_FigS1$BL_age < 50, 4, ifelse(
      GCKD_df1_FigS1$BL_age >= 50 & GCKD_df1_FigS1$BL_age < 60, 5, ifelse(GCKD_df1_FigS1$BL_age >= 60 & GCKD_df1_FigS1$BL_age < 70, 6, 7)))))))
GCKD_df1_FigS1 %>%
  filter(dem_sex == "Male") %>%
  ggplot(aes(x=as.factor(BL_age_cat))) +
  geom_bar(color="white", fill="#B83674", width=1.0)  +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
  coord_flip() +
  theme(aspect.ratio = 3/2)
GCKD_df1_FigS1 %>%
  filter(dem_sex == "Female") %>%
  ggplot(aes(x=as.factor(BL_age_cat))) +
  geom_bar(color="white", fill="#B83674", alpha = 0.5, width=1.0) +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
  coord_flip() +
  theme(aspect.ratio = 3/2)
GCKD_df1_FigS1 %>%
  filter(diabetes == 1) %>%
  ggplot(aes(x=as.factor(BL_age_cat))) +
  geom_bar(color="white", fill="#B83674", width=1.0)  +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
  coord_flip() +
  theme(aspect.ratio = 3/2)
GCKD_df1_FigS1 %>%
  filter(diabetes == 2) %>%
  ggplot(aes(x=as.factor(BL_age_cat))) +
  geom_bar(color="white", fill="#B83674", alpha = 0.5, width=1.0) +
  scale_y_continuous(limits=c(0,1200), breaks=c(0,300,600,900,1200)) + 
  coord_flip() +
  theme(aspect.ratio = 3/2)


# !! All datasets !!

## Distribution of Quasi-Identifiers
### Figure settings: fill="#B83674" in original dataset, "#73A1AA" in anonymized datasets
table(GCKD_df1$dem_sex, useNA = "always")
prop.table(table(GCKD_df1$dem_sex))
GCKD_df1 %>%
  filter(!is.na(dem_sex)) %>% 
  ggplot(aes(x=as.factor(dem_sex))) +
  geom_bar(color="white", fill="#73A1AA", width=0.5) +
  theme(aspect.ratio = 3/2)
table(GCKD_df1$biopsy, useNA = "always")
prop.table(table(GCKD_df1$biopsy))
GCKD_df1 %>%
  filter(!is.na(biopsy)) %>% 
  ggplot(aes(x=as.factor(biopsy))) +
  geom_bar(color="white", fill="#73A1AA", width=0.5) +
  theme(aspect.ratio = 3/2)
## Table 1: Baseline characteristics
table(GCKD_df1$aa_stroke, useNA = "always")
prop.table(table(GCKD_df1$aa_stroke))
table(GCKD_df1$aa_myocard, useNA = "always")
prop.table(table(GCKD_df1$aa_myocard))
table(GCKD_df1$aa_hypertens, useNA = "always")
prop.table(table(GCKD_df1$aa_hypertens))
table(GCKD_df1$aa_diabetes, useNA = "always")
prop.table(table(GCKD_df1$aa_diabetes))
table(GCKD_df1$aa_renal, useNA = "always")
prop.table(table(GCKD_df1$aa_renal))
table(GCKD_df1$aa_renal_stones, useNA = "always")
prop.table(table(GCKD_df1$aa_renal_stones))
table(GCKD_df1$aa_dialyse, useNA = "always")
prop.table(table(GCKD_df1$aa_dialyse))
table(GCKD_df1$aa_ntx, useNA = "always")
prop.table(table(GCKD_df1$aa_ntx))
table(GCKD_df1$smoking, useNA = "always")
prop.table(table(GCKD_df1$smoking))
table(GCKD_df1$hospital, useNA = "always")
prop.table(table(GCKD_df1$hospital))
GCKD_df1 %>%
  summarise_at(vars(BL_ku_ruhepuls, BL_ku_sys, BL_ku_dia, BL_ku_map), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1 %>%
  summarise_at(vars(BL_ku_ruhepuls, BL_ku_sys, BL_ku_dia, BL_ku_map), list(~ round(sd(., na.rm=TRUE),1)))
table(GCKD_df1$BL_ku_sys < 130 & GCKD_df1$BL_ku_dia < 80,  useNA = "always")
prop.table(table(GCKD_df1$BL_ku_sys < 130 & GCKD_df1$BL_ku_dia < 80))
table(GCKD_df1$BL_ku_sys < 140 & GCKD_df1$BL_ku_dia < 90,  useNA = "always")
prop.table(table(GCKD_df1$BL_ku_sys < 140 & GCKD_df1$BL_ku_dia < 90))
GCKD_df1 %>%
  summarise_at(vars(BL_creavalue, BL_cysvalue, BL_gfr_mdrd), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1 %>%
  summarise_at(vars(BL_creavalue, BL_cysvalue, BL_gfr_mdrd), list(~ round(sd(., na.rm=TRUE),1)))
table(GCKD_df1$BL_gfr_mdrd >= 60,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_mdrd >= 60))
table(GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_mdrd >= 45 & GCKD_df1$BL_gfr_mdrd < 60))
table(GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_mdrd >= 30 & GCKD_df1$BL_gfr_mdrd < 45))
table(GCKD_df1$BL_gfr_mdrd < 30,  useNA = "always")
prop.table(table(GCKD_df1$BL_gfr_mdrd < 30))
summary(GCKD_df1$BL_uacr)
table(GCKD_df1$BL_uacr < 30,  useNA = "always")
prop.table(table(GCKD_df1$BL_uacr < 30))
table(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300,  useNA = "always")
prop.table(table(GCKD_df1$BL_uacr >= 30 & GCKD_df1$BL_uacr <= 300))
table(GCKD_df1$BL_uacr > 300,  useNA = "always")
prop.table(table(GCKD_df1$BL_uacr > 300))
table(GCKD_df1$BL_med_raas_ace, useNA = "always")
prop.table(table(GCKD_df1$BL_med_raas_ace))
table(GCKD_df1$BL_med_raas_at1, useNA = "always")
prop.table(table(GCKD_df1$BL_med_raas_at1))
table(GCKD_df1$BL_med_raas_double, useNA = "always")
prop.table(table(GCKD_df1$BL_med_raas_double))
table(GCKD_df1$BL_med_raas_single, useNA = "always")
prop.table(table(GCKD_df1$BL_med_raas_single))
table(GCKD_df1$BL_med_diuretic, useNA = "always")
prop.table(table(GCKD_df1$BL_med_diuretic))
table(GCKD_df1$BL_med_diuretic_thiazid, useNA = "always")
prop.table(table(GCKD_df1$BL_med_diuretic_thiazid))
table(GCKD_df1$BL_med_diuretic_aldost, useNA = "always")
prop.table(table(GCKD_df1$BL_med_diuretic_aldost))
table(GCKD_df1$BL_med_diuretic_loop, useNA = "always")
prop.table(table(GCKD_df1$BL_med_diuretic_loop))
table(GCKD_df1$BL_med_caanta, useNA = "always")
prop.table(table(GCKD_df1$BL_med_caanta))
table(GCKD_df1$BL_med_bblocker, useNA = "always")
prop.table(table(GCKD_df1$BL_med_bblocker))
table(GCKD_df1$biopsy, useNA = "always")
prop.table(table(GCKD_df1$biopsy))
### Subsetting female
GCKD_df1_fem <- GCKD_df1 %>% subset(dem_sex == "Female")
table(GCKD_df1_fem$diabetes)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$aa_stroke, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$aa_stroke), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$aa_myocard, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$aa_myocard), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$aa_hypertens, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$aa_hypertens), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$aa_diabetes, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$aa_diabetes), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$aa_renal, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$aa_renal), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$aa_renal_stones, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$aa_renal_stones), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$aa_dialyse, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$aa_dialyse), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$aa_ntx, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$aa_ntx), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$smoking, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$smoking), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$hospital, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$hospital), 1)
GCKD_df1_fem %>% group_by(diabetes) %>%
  summarise_at(vars(BL_ku_ruhepuls, BL_ku_sys, BL_ku_dia, BL_ku_map), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1_fem %>% group_by(diabetes) %>%
  summarise_at(vars(BL_ku_ruhepuls, BL_ku_sys, BL_ku_dia, BL_ku_map), list(~ round(sd(., na.rm=TRUE),1)))
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_sys < 130 & GCKD_df1_fem$BL_ku_dia < 80,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_sys < 130 & GCKD_df1_fem$BL_ku_dia < 80), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_sys < 140 & GCKD_df1_fem$BL_ku_dia < 90,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_ku_sys < 140 & GCKD_df1_fem$BL_ku_dia < 90), 1)
GCKD_df1_fem %>% group_by(diabetes) %>%
  summarise_at(vars(BL_creavalue, BL_cysvalue, BL_gfr_mdrd), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1_fem %>% group_by(diabetes) %>%
  summarise_at(vars(BL_creavalue, BL_cysvalue, BL_gfr_mdrd), list(~ round(sd(., na.rm=TRUE),1)))
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd >= 60,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd >= 60), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd >= 45 & GCKD_df1_fem$BL_gfr_mdrd < 60,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd >= 45 & GCKD_df1_fem$BL_gfr_mdrd < 60), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd >= 30 & GCKD_df1_fem$BL_gfr_mdrd < 45,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd >= 30 & GCKD_df1_fem$BL_gfr_mdrd < 45), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd < 30,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_gfr_mdrd < 30), 1)
GCKD_df1_fem %>% group_by(diabetes) %>%
  summarise_at(vars(BL_uacr), list(~ round(median(., na.rm=TRUE), 1)))
GCKD_df1_fem %>% group_by(diabetes) %>%
  summarise_at(vars(BL_uacr), list(~ round(quantile(., na.rm=TRUE), 1)))
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_uacr < 30,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_uacr < 30), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_uacr >= 30 & GCKD_df1_fem$BL_uacr <= 300,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_uacr >= 30 & GCKD_df1_fem$BL_uacr <= 300), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_uacr > 300,  useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_uacr > 300), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_raas_ace, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_raas_ace), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_raas_at1, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_raas_at1), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_raas_double, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_raas_double), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_raas_single, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_raas_single), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_diuretic, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_diuretic), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_diuretic_thiazid, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_diuretic_thiazid), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_diuretic_aldost, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_diuretic_aldost), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_diuretic_loop, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_diuretic_loop), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_caanta, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_caanta), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_bblocker, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$BL_med_bblocker), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$biopsy, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$biopsy), 1)
### Subsetting male
GCKD_df1_male <- GCKD_df1 %>% subset(dem_sex == "Male")
table(GCKD_df1_male$diabetes)
table(GCKD_df1_male$diabetes, GCKD_df1_male$aa_stroke, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$aa_stroke), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$aa_myocard, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$aa_myocard), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$aa_hypertens, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$aa_hypertens), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$aa_diabetes, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$aa_diabetes), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$aa_renal, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$aa_renal), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$aa_renal_stones, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$aa_renal_stones), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$aa_dialyse, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$aa_dialyse), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$aa_ntx, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$aa_ntx), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$smoking, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$smoking), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$hospital, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$hospital), 1)
GCKD_df1_male %>% group_by(diabetes) %>%
  summarise_at(vars(BL_ku_ruhepuls, BL_ku_sys, BL_ku_dia, BL_ku_map), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1_male %>% group_by(diabetes) %>%
  summarise_at(vars(BL_ku_ruhepuls, BL_ku_sys, BL_ku_dia, BL_ku_map), list(~ round(sd(., na.rm=TRUE),1)))
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_sys < 130 & GCKD_df1_male$BL_ku_dia < 80,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_sys < 130 & GCKD_df1_male$BL_ku_dia < 80), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_sys < 140 & GCKD_df1_male$BL_ku_dia < 90,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_ku_sys < 140 & GCKD_df1_male$BL_ku_dia < 90), 1)
GCKD_df1_male %>% group_by(diabetes) %>%
  summarise_at(vars(BL_creavalue, BL_cysvalue, BL_gfr_mdrd), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1_male %>% group_by(diabetes) %>%
  summarise_at(vars(BL_creavalue, BL_cysvalue, BL_gfr_mdrd), list(~ round(sd(., na.rm=TRUE),1)))
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd >= 60,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd >= 60), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd >= 45 & GCKD_df1_male$BL_gfr_mdrd < 60,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd >= 45 & GCKD_df1_male$BL_gfr_mdrd < 60), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd >= 30 & GCKD_df1_male$BL_gfr_mdrd < 45,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd >= 30 & GCKD_df1_male$BL_gfr_mdrd < 45), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd < 30,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_gfr_mdrd < 30), 1)
GCKD_df1_male %>% group_by(diabetes) %>%
  summarise_at(vars(BL_uacr), list(~ round(median(., na.rm=TRUE), 1)))
GCKD_df1_male %>% group_by(diabetes) %>%
  summarise_at(vars(BL_uacr), list(~ round(quantile(., na.rm=TRUE), 1)))
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_uacr < 30,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_uacr < 30), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_uacr >= 30 & GCKD_df1_male$BL_uacr <= 300,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_uacr >= 30 & GCKD_df1_male$BL_uacr <= 300), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_uacr > 300,  useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_uacr > 300), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_raas_ace, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_raas_ace), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_raas_at1, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_raas_at1), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_raas_double, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_raas_double), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_raas_single, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_raas_single), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_diuretic, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_diuretic), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_diuretic_thiazid, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_diuretic_thiazid), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_diuretic_aldost, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_diuretic_aldost), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_diuretic_loop, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_diuretic_loop), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_caanta, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_caanta), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_bblocker, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$BL_med_bblocker), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$biopsy, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$biopsy), 1)

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
table(GCKD_df1$incl_egfr, GCKD_df1$dem_sex, useNA = "always")
prop.table(table(GCKD_df1$incl_egfr, GCKD_df1$dem_sex), 1)
table(GCKD_df1$incl_egfr, GCKD_df1$aa_stroke, useNA = "always")
prop.table(table(GCKD_df1$incl_egfr, GCKD_df1$aa_stroke), 1)
table(GCKD_df1$incl_egfr, GCKD_df1$aa_myocard, useNA = "always")
prop.table(table(GCKD_df1$incl_egfr, GCKD_df1$aa_myocard), 1)
table(GCKD_df1$incl_egfr, GCKD_df1$aa_hypertens, useNA = "always")
prop.table(table(GCKD_df1$incl_egfr, GCKD_df1$aa_hypertens), 1)
table(GCKD_df1$incl_egfr, GCKD_df1$aa_diabetes, useNA = "always")
prop.table(table(GCKD_df1$incl_egfr, GCKD_df1$aa_diabetes), 1)
table(GCKD_df1$incl_egfr, GCKD_df1$aa_renal, useNA = "always")
prop.table(table(GCKD_df1$incl_egfr, GCKD_df1$aa_renal), 1)
table(GCKD_df1$incl_egfr, GCKD_df1$aa_renal_stones, useNA = "always")
prop.table(table(GCKD_df1$incl_egfr, GCKD_df1$aa_renal_stones), 1)
table(GCKD_df1$incl_egfr, GCKD_df1$aa_dialyse, useNA = "always")
prop.table(table(GCKD_df1$incl_egfr, GCKD_df1$aa_dialyse), 1)
table(GCKD_df1$incl_egfr, GCKD_df1$aa_ntx, useNA = "always")
prop.table(table(GCKD_df1$incl_egfr, GCKD_df1$aa_ntx), 1)
table(GCKD_df1$incl_egfr, GCKD_df1$smoking, useNA = "always")
prop.table(table(GCKD_df1$incl_egfr, GCKD_df1$smoking), 1)
table(GCKD_df1$incl_egfr, GCKD_df1$diabetes, useNA = "always")
prop.table(table(GCKD_df1$incl_egfr, GCKD_df1$diabetes), 1)
GCKD_df1 %>% group_by(incl_egfr) %>%
  summarise_at(vars(BL_creavalue, BL_cysvalue, BL_gfr_mdrd), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1 %>% group_by(incl_egfr) %>%
  summarise_at(vars(BL_creavalue, BL_cysvalue, BL_gfr_mdrd), list(~ round(sd(., na.rm=TRUE),1)))
GCKD_df1 %>% group_by(incl_egfr) %>%
  summarise_at(vars(BL_uacr), list(~ round(median(., na.rm=TRUE), 1)))
GCKD_df1 %>% group_by(incl_egfr) %>%
  summarise_at(vars(BL_uacr), list(~ round(quantile(., na.rm=TRUE), 1)))
table(GCKD_df1$incl_egfr, GCKD_df1$biopsy, useNA = "always")
prop.table(table(GCKD_df1$incl_egfr, GCKD_df1$biopsy), 1)
table(GCKD_df1$incl_egfr, GCKD_df1$ckd_lead, useNA = "always")
prop.table(table(GCKD_df1$incl_egfr, GCKD_df1$ckd_lead), 1)

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
table(GCKD_df1$ckd_lead, useNA = "always")
prop.table(table(GCKD_df1$ckd_lead))
table(GCKD_df1$ckd_diab, GCKD_df1$ckd_lead, useNA = "always")
table(GCKD_df1$ckd_vasc, GCKD_df1$ckd_lead, useNA = "always")
table(GCKD_df1$ckd_syst, GCKD_df1$ckd_lead, useNA = "always")
table(GCKD_df1$ckd_glom_prim, GCKD_df1$ckd_lead, useNA = "always")
table(GCKD_df1$ckd_interst, GCKD_df1$ckd_lead, useNA = "always")
table(GCKD_df1$ckd_aki, GCKD_df1$ckd_lead, useNA = "always")
table(GCKD_df1$ckd_single, GCKD_df1$ckd_lead, useNA = "always")
table(GCKD_df1$ckd_heredit, GCKD_df1$ckd_lead, useNA = "always")
table(GCKD_df1$ckd_obstr, GCKD_df1$ckd_lead, useNA = "always")
table(GCKD_df1$ckd_oth, GCKD_df1$ckd_lead, useNA = "always")
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
prop.table(table(GCKD_df1$biopsy, GCKD_df1$ckd_lead), 2)

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
table(GCKD_df1$cardiovasc, useNA = "always")
prop.table(table(GCKD_df1$cardiovasc))
table(GCKD_df1$hypertension, useNA = "always")
prop.table(table(GCKD_df1$hypertension))
table(GCKD_df1$valve, useNA = "always")
prop.table(table(GCKD_df1$valve))
table(GCKD_df1$coronary, useNA = "always")
prop.table(table(GCKD_df1$coronary))
table(GCKD_df1$myocard, useNA = "always")
prop.table(table(GCKD_df1$myocard))
table(GCKD_df1$bypass, useNA = "always")
prop.table(table(GCKD_df1$bypass))
table(GCKD_df1$ptca, useNA = "always")
prop.table(table(GCKD_df1$ptca))
table(GCKD_df1$cerebrovasc, useNA = "always")
prop.table(table(GCKD_df1$cerebrovasc))
table(GCKD_df1$stroke, useNA = "always")
prop.table(table(GCKD_df1$stroke))
table(GCKD_df1$carotic_surg, useNA = "always")
prop.table(table(GCKD_df1$carotic_surg))
table(GCKD_df1$carotic_interv, useNA = "always")
prop.table(table(GCKD_df1$carotic_interv))
table(GCKD_df1$pavk, useNA = "always")
prop.table(table(GCKD_df1$pavk))
table(GCKD_df1$amput, useNA = "always")
prop.table(table(GCKD_df1$amput))
table(GCKD_df1$ygraft, useNA = "always")
prop.table(table(GCKD_df1$ygraft))
table(GCKD_df1$pavk_surgery, useNA = "always")
prop.table(table(GCKD_df1$pavk_surgery))
table(GCKD_df1$pta, useNA = "always")
prop.table(table(GCKD_df1$pta))
### Subsetting female
table(GCKD_df1_fem$diabetes)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$cardiovasc, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$cardiovasc), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$hypertension, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$hypertension), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$valve, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$valve), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$coronary, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$coronary), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$myocard, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$myocard), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$bypass, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$bypass), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$ptca, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$ptca), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$cerebrovasc, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$cerebrovasc), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$stroke, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$stroke), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$carotic_surg, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$carotic_surg), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$carotic_interv, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$carotic_interv), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$pavk, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$pavk), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$amput, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$amput), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$ygraft, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$ygraft), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$pavk_surgery, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$pavk_surgery), 1)
table(GCKD_df1_fem$diabetes, GCKD_df1_fem$pta, useNA = "always")
prop.table(table(GCKD_df1_fem$diabetes, GCKD_df1_fem$pta), 1)
### Subsetting male
table(GCKD_df1_male$diabetes)
table(GCKD_df1_male$diabetes, GCKD_df1_male$cardiovasc, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$cardiovasc), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$hypertension, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$hypertension), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$valve, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$valve), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$coronary, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$coronary), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$myocard, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$myocard), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$bypass, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$bypass), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$ptca, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$ptca), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$cerebrovasc, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$cerebrovasc), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$stroke, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$stroke), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$carotic_surg, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$carotic_surg), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$carotic_interv, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$carotic_interv), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$pavk, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$pavk), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$amput, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$amput), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$ygraft, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$ygraft), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$pavk_surgery, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$pavk_surgery), 1)
table(GCKD_df1_male$diabetes, GCKD_df1_male$pta, useNA = "always")
prop.table(table(GCKD_df1_male$diabetes, GCKD_df1_male$pta), 1)

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
GCKD_df1_TableS3 <- GCKD_df1 
GCKD_df1_TableS3 <- GCKD_df1_TableS3 %>% mutate(DM = ifelse(GCKD_df1_TableS3$diabetes == 2, "No DM", ifelse(GCKD_df1_TableS3$ckd_lead == "ckd_diab", "DMwDN", "DMwoDN")))
table(GCKD_df1_TableS3$DM)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$education, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$education), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$aa_stroke, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$aa_stroke), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$aa_myocard, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$aa_myocard), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$aa_hypertens, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$aa_hypertens), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$aa_diabetes, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$aa_diabetes), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$aa_renal, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$aa_renal), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$aa_renal_stones, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$aa_renal_stones), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$aa_dialyse, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$aa_dialyse), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$aa_ntx, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$aa_ntx), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$smoking, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$smoking), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$hospital, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$hospital), 1)
GCKD_df1_TableS3 %>% group_by(DM) %>%
  summarise_at(vars(BL_ku_ruhepuls, BL_ku_sys, BL_ku_dia, BL_ku_map), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1_TableS3 %>% group_by(DM) %>%
  summarise_at(vars(BL_ku_ruhepuls, BL_ku_sys, BL_ku_dia, BL_ku_map), list(~ round(sd(., na.rm=TRUE),1)))
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_sys < 130 & GCKD_df1_TableS3$BL_ku_dia < 80,  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_sys < 130 & GCKD_df1_TableS3$BL_ku_dia < 80), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_sys < 140 & GCKD_df1_TableS3$BL_ku_dia < 90,  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_ku_sys < 140 & GCKD_df1_TableS3$BL_ku_dia < 90), 1)
GCKD_df1_TableS3 %>% group_by(DM) %>%
  summarise_at(vars(BL_creavalue, BL_cysvalue, BL_gfr_mdrd), list(~ round(mean(., na.rm=TRUE),1)))
GCKD_df1_TableS3 %>% group_by(DM) %>%
  summarise_at(vars(BL_creavalue, BL_cysvalue, BL_gfr_mdrd), list(~ round(sd(., na.rm=TRUE),1)))
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_gfr_mdrd >= 60,  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_gfr_mdrd >= 60), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_gfr_mdrd >= 45 & GCKD_df1_TableS3$BL_gfr_mdrd < 60,  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_gfr_mdrd >= 45 & GCKD_df1_TableS3$BL_gfr_mdrd < 60), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_gfr_mdrd >= 30 & GCKD_df1_TableS3$BL_gfr_mdrd < 45,  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_gfr_mdrd >= 30 & GCKD_df1_TableS3$BL_gfr_mdrd < 45), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_gfr_mdrd < 30,  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_gfr_mdrd < 30), 1)
GCKD_df1_TableS3 %>% group_by(DM) %>%
  summarise_at(vars(BL_uacr), list(~ round(median(., na.rm=TRUE), 1)))
GCKD_df1_TableS3 %>% group_by(DM) %>%
  summarise_at(vars(BL_uacr), list(~ round(quantile(., na.rm=TRUE), 1)))
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_uacr < 30,  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_uacr < 30), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_uacr >= 30 & GCKD_df1_TableS3$BL_uacr <= 300,  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_uacr >= 30 & GCKD_df1_TableS3$BL_uacr <= 300), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_uacr > 300,  useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_uacr > 300), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_raas_ace, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_raas_ace), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_raas_at1, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_raas_at1), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_raas_double, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_raas_double), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_raas_single, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_raas_single), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_diuretic, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_diuretic), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_diuretic_thiazid, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_diuretic_thiazid), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_diuretic_aldost, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_diuretic_aldost), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_diuretic_loop, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_diuretic_loop), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_caanta, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_caanta), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_bblocker, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$BL_med_bblocker), 1)
table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$biopsy, useNA = "always")
prop.table(table(GCKD_df1_TableS3$DM, GCKD_df1_TableS3$biopsy), 1)

