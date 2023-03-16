# Packages 
pacman::p_load(tidyr, stringr, dplyr, openxlsx, naniar, emmeans, multcomp, 
               plyr, finalfit, ggplot2, tibble, lmtest, sandwich,
               tidyverse, tidyselect, summarytools, scales, gridExtra, 
               lubridate, eeptools, gtsummary, flextable, boot, mosaic, patchwork, rms, coxed, DescTools, PropCIs)

# Dataset
setwd(path_data_origin)
df_origin <- as_tibble(read.csv("GCKD_df1_origin.csv", sep = ";"))
setwd(path_data)
df_anonym_GENERIC_strictaverage_2 <- as_tibble(read.csv("AnonymizedDataset_GENERIC_strictaverage_2.csv", sep = ";"))
df_anonym_GENERIC_strictaverage_33 <- as_tibble(read.csv("AnonymizedDataset_GENERIC_strictaverage_33.csv", sep = ";"))
df_anonym_GENERIC_kanonymity_11 <- as_tibble(read.csv("AnonymizedDataset_GENERIC_kanonymity_11.csv", sep = ";"))
df_anonym_GENERIC_kanonymity_33 <- as_tibble(read.csv("AnonymizedDataset_GENERIC_kanonymity_33.csv", sep = ";"))
df_anonym_USECASE_4QI_strictaverage_2 <- as_tibble(read.csv("AnonymizedDataset_USECASE_4QI_strictaverage_2.csv", sep = ";"))
df_anonym_USECASE_4QI_strictaverage_33 <- as_tibble(read.csv("AnonymizedDataset_USECASE_4QI_strictaverage_33.csv", sep = ";"))
df_anonym_USECASE_4QI_kanonymity_11 <- as_tibble(read.csv("AnonymizedDataset_USECASE_4QI_kanonymity_11.csv", sep = ";"))
df_anonym_USECASE_4QI_kanonymity_33 <- as_tibble(read.csv("AnonymizedDataset_USECASE_4QI_kanonymity_33.csv", sep = ";"))

# Data Cleansing
## Marking NA
df_anonym_GENERIC_strictaverage_2 <- df_anonym_GENERIC_strictaverage_2 %>% mutate(across(where(is.character), ~na_if(., "*")))
df_anonym_GENERIC_strictaverage_33 <- df_anonym_GENERIC_strictaverage_33 %>% mutate(across(where(is.character), ~na_if(., "*")))
df_anonym_GENERIC_kanonymity_11 <- df_anonym_GENERIC_kanonymity_11 %>% mutate(across(where(is.character), ~na_if(., "*")))
df_anonym_GENERIC_kanonymity_33 <- df_anonym_GENERIC_kanonymity_33 %>% mutate(across(where(is.character), ~na_if(., "*")))
df_anonym_USECASE_4QI_strictaverage_2 <- df_anonym_USECASE_4QI_strictaverage_2 %>% mutate(across(where(is.character), ~na_if(., "*")))
df_anonym_USECASE_4QI_strictaverage_33 <- df_anonym_USECASE_4QI_strictaverage_33 %>% mutate(across(where(is.character), ~na_if(., "*")))
df_anonym_USECASE_4QI_kanonymity_11 <- df_anonym_USECASE_4QI_kanonymity_11 %>% mutate(across(where(is.character), ~na_if(., "*")))
df_anonym_USECASE_4QI_kanonymity_33 <- df_anonym_USECASE_4QI_kanonymity_33 %>% mutate(across(where(is.character), ~na_if(., "*")))
df_anonym_GENERIC_strictaverage_2 <- df_anonym_GENERIC_strictaverage_2 %>% mutate(across(where(is.character), ~na_if(., "NULL")))
df_anonym_GENERIC_strictaverage_33 <- df_anonym_GENERIC_strictaverage_33 %>% mutate(across(where(is.character), ~na_if(., "NULL")))
df_anonym_GENERIC_kanonymity_11 <- df_anonym_GENERIC_kanonymity_11 %>% mutate(across(where(is.character), ~na_if(., "NULL")))
df_anonym_GENERIC_kanonymity_33 <- df_anonym_GENERIC_kanonymity_33 %>% mutate(across(where(is.character), ~na_if(., "NULL")))
df_anonym_USECASE_4QI_strictaverage_2 <- df_anonym_USECASE_4QI_strictaverage_2 %>% mutate(across(where(is.character), ~na_if(., "NULL")))
df_anonym_USECASE_4QI_strictaverage_33 <- df_anonym_USECASE_4QI_strictaverage_33 %>% mutate(across(where(is.character), ~na_if(., "NULL")))
df_anonym_USECASE_4QI_kanonymity_11 <- df_anonym_USECASE_4QI_kanonymity_11 %>% mutate(across(where(is.character), ~na_if(., "NULL")))
df_anonym_USECASE_4QI_kanonymity_33 <- df_anonym_USECASE_4QI_kanonymity_33 %>% mutate(across(where(is.character), ~na_if(., "NULL")))
df_anonym_GENERIC_strictaverage_2 <- df_anonym_GENERIC_strictaverage_2 %>% mutate(across(where(is.character), ~na_if(., "null")))
df_anonym_GENERIC_strictaverage_33 <- df_anonym_GENERIC_strictaverage_33 %>% mutate(across(where(is.character), ~na_if(., "null")))
df_anonym_GENERIC_kanonymity_11 <- df_anonym_GENERIC_kanonymity_11 %>% mutate(across(where(is.character), ~na_if(., "null")))
df_anonym_GENERIC_kanonymity_33 <- df_anonym_GENERIC_kanonymity_33 %>% mutate(across(where(is.character), ~na_if(., "null")))
df_anonym_USECASE_4QI_strictaverage_2 <- df_anonym_USECASE_4QI_strictaverage_2 %>% mutate(across(where(is.character), ~na_if(., "null")))
df_anonym_USECASE_4QI_strictaverage_33 <- df_anonym_USECASE_4QI_strictaverage_33 %>% mutate(across(where(is.character), ~na_if(., "null")))
df_anonym_USECASE_4QI_kanonymity_11 <- df_anonym_USECASE_4QI_kanonymity_11 %>% mutate(across(where(is.character), ~na_if(., "null")))
df_anonym_USECASE_4QI_kanonymity_33 <- df_anonym_USECASE_4QI_kanonymity_33 %>% mutate(across(where(is.character), ~na_if(., "null")))

## Relevant variables
var <- c("BL_age", "BL_ku_height_cm", "BL_ku_weight", "BL_ku_bmi")
df_origin <- df_origin %>% mutate(across(all_of(var), as.numeric))
df_anonym_GENERIC_strictaverage_2 <- df_anonym_GENERIC_strictaverage_2 %>% mutate(across(all_of(var), as.character))
df_anonym_GENERIC_strictaverage_33 <- df_anonym_GENERIC_strictaverage_33 %>% mutate(across(all_of(var), as.character))
df_anonym_GENERIC_kanonymity_11 <- df_anonym_GENERIC_kanonymity_11 %>% mutate(across(all_of(var), as.character))
df_anonym_GENERIC_kanonymity_33 <- df_anonym_GENERIC_kanonymity_33 %>% mutate(across(all_of(var), as.character))
df_anonym_USECASE_4QI_strictaverage_2 <- df_anonym_USECASE_4QI_strictaverage_2 %>% mutate(across(all_of(var), as.character))
df_anonym_USECASE_4QI_strictaverage_33 <- df_anonym_USECASE_4QI_strictaverage_33 %>% mutate(across(all_of(var), as.character))
df_anonym_USECASE_4QI_kanonymity_11 <- df_anonym_USECASE_4QI_kanonymity_11 %>% mutate(across(all_of(var), as.character))
df_anonym_USECASE_4QI_kanonymity_33 <- df_anonym_USECASE_4QI_kanonymity_33 %>% mutate(across(all_of(var), as.character))
df_origin$diabetes <- mapvalues(df_origin$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_anonym_GENERIC_strictaverage_2$diabetes <- mapvalues(df_anonym_GENERIC_strictaverage_2$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_anonym_GENERIC_strictaverage_33$diabetes <- mapvalues(df_anonym_GENERIC_strictaverage_33$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_anonym_GENERIC_kanonymity_11$diabetes <- mapvalues(df_anonym_GENERIC_kanonymity_11$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_anonym_GENERIC_kanonymity_33$diabetes <- mapvalues(df_anonym_GENERIC_kanonymity_33$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_anonym_USECASE_4QI_strictaverage_2$diabetes <- mapvalues(df_anonym_USECASE_4QI_strictaverage_2$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_anonym_USECASE_4QI_strictaverage_33$diabetes <- mapvalues(df_anonym_USECASE_4QI_strictaverage_33$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_anonym_USECASE_4QI_kanonymity_11$diabetes <- mapvalues(df_anonym_USECASE_4QI_kanonymity_11$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_anonym_USECASE_4QI_kanonymity_33$diabetes <- mapvalues(df_anonym_USECASE_4QI_kanonymity_33$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_origin$cardiovasc <- mapvalues(df_origin$cardiovasc , from = c("1", "2"), to = c("cardiovasc_yes", "cardiovasc_no"))
df_anonym_GENERIC_strictaverage_2$cardiovasc <- mapvalues(df_anonym_GENERIC_strictaverage_2$cardiovasc , from = c("1", "2"), to = c("cardiovasc_yes", "cardiovasc_no"))
df_anonym_GENERIC_strictaverage_33$cardiovasc <- mapvalues(df_anonym_GENERIC_strictaverage_33$cardiovasc , from = c("1", "2"), to = c("cardiovasc_yes", "cardiovasc_no"))
df_anonym_GENERIC_kanonymity_11$cardiovasc <- mapvalues(df_anonym_GENERIC_kanonymity_11$cardiovasc , from = c("1", "2"), to = c("cardiovasc_yes", "cardiovasc_no"))
df_anonym_GENERIC_kanonymity_33$cardiovasc <- mapvalues(df_anonym_GENERIC_kanonymity_33$cardiovasc , from = c("1", "2"), to = c("cardiovasc_yes", "cardiovasc_no"))
df_anonym_USECASE_4QI_strictaverage_2$cardiovasc <- mapvalues(df_anonym_USECASE_4QI_strictaverage_2$cardiovasc , from = c("1", "2"), to = c("cardiovasc_yes", "cardiovasc_no"))
df_anonym_USECASE_4QI_strictaverage_33$cardiovasc <- mapvalues(df_anonym_USECASE_4QI_strictaverage_33$cardiovasc , from = c("1", "2"), to = c("cardiovasc_yes", "cardiovasc_no"))
df_anonym_USECASE_4QI_kanonymity_11$cardiovasc <- mapvalues(df_anonym_USECASE_4QI_kanonymity_11$cardiovasc , from = c("1", "2"), to = c("cardiovasc_yes", "cardiovasc_no"))
df_anonym_USECASE_4QI_kanonymity_33$cardiovasc <- mapvalues(df_anonym_USECASE_4QI_kanonymity_33$cardiovasc , from = c("1", "2"), to = c("cardiovasc_yes", "cardiovasc_no"))

# Table 1 subset female non diabetics
## Subsetting
df_origin_femnd <- df_origin %>% subset(dem_sex == "Female" & diabetes == "diabetes_no")
df_anonym_GENERIC_strictaverage_2_femnd <- df_anonym_GENERIC_strictaverage_2 %>% subset(dem_sex == "Female" & diabetes == "diabetes_no")
df_anonym_GENERIC_strictaverage_33_femnd <- df_anonym_GENERIC_strictaverage_33 %>% subset(dem_sex == "Female" & diabetes == "diabetes_no")
df_anonym_GENERIC_kanonymity_11_femnd <- df_anonym_GENERIC_kanonymity_11 %>% subset(dem_sex == "Female" & diabetes == "diabetes_no")
df_anonym_GENERIC_kanonymity_33_femnd <- df_anonym_GENERIC_kanonymity_33 %>% subset(dem_sex == "Female" & diabetes == "diabetes_no")
df_anonym_USECASE_4QI_strictaverage_2_femnd <- df_anonym_USECASE_4QI_strictaverage_2 %>% subset(dem_sex == "Female" & diabetes == "diabetes_no")
df_anonym_USECASE_4QI_strictaverage_33_femnd <- df_anonym_USECASE_4QI_strictaverage_33 %>% subset(dem_sex == "Female" & diabetes == "diabetes_no")
df_anonym_USECASE_4QI_kanonymity_11_femnd <- df_anonym_USECASE_4QI_kanonymity_11 %>% subset(dem_sex == "Female" & diabetes == "diabetes_no")
df_anonym_USECASE_4QI_kanonymity_33_femnd <- df_anonym_USECASE_4QI_kanonymity_33 %>% subset(dem_sex == "Female" & diabetes == "diabetes_no")
## Age
df_anonym_GENERIC_strictaverage_2_femnd$BL_age <- mapvalues(df_anonym_GENERIC_strictaverage_2_femnd$BL_age, from = c("[20, 30[", "[30, 40[", "[40, 50[", "[50, 60[", "[60, 70[", 
                                                                                                                     "[70, 80["), to = c("25", "35", "45", "55", "65", "75"))
df_anonym_GENERIC_strictaverage_33_femnd$BL_age <- mapvalues(df_anonym_GENERIC_strictaverage_33_femnd$BL_age, from = c("[20, 40[", "[40, 60[", "[60, 80["), to = c("30", "50", "70"))
df_anonym_GENERIC_kanonymity_11_femnd$BL_age <- mapvalues(df_anonym_GENERIC_kanonymity_11_femnd$BL_age, from = c("[20, 40[", "[40, 60[", "[60, 80["), to = c("30", "50", "70"))
df_anonym_GENERIC_kanonymity_33_femnd$BL_age <- mapvalues(df_anonym_GENERIC_kanonymity_33_femnd$BL_age, from = c("[20, 40[", "[40, 60[", "[60, 80["), to = c("30", "50", "70"))
df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_age <- mapvalues(df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_age, from = c("[18, 20[", "[20, 25[", "[25, 30[", "[30, 35[", "[35, 40[",
                                                                                                                             "[40, 45[", "[45, 50[", "[50, 55[", "[55, 60[", "[60, 65[", 
                                                                                                                             "[65, 70[", "[70, 75[", "[75, 80["), 
                                                                to = c("19", "22.5", "27.5", "32.5", "37.5","42.5", "47.5", "52.5", "57.5", "62.5", "67.5", "72.5", "77.5"))
df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_age <- mapvalues(df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_age, from = c("[18, 20[", "[20, 30[", "[30, 40[", "[40, 50[", "[50, 60[", 
                                                                                                                               "[60, 70[", "[70, 80["), 
                                                                 to = c("19", "25", "35", "45", "55", "65", "75"))
df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_age <- mapvalues(df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_age, from = c("[20, 30[", "[30, 40[", "[40, 50[", "[50, 60[", "[60, 70[", 
                                                                                                                         "[70, 80["), to = c("25", "35", "45", "55", "65", "75"))
df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_age <- mapvalues(df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_age, from = c("[30, 40[", "[40, 50[", "[50, 60[", "[60, 70[", "[70, 80["), 
                                                              to = c("35", "45", "55", "65", "75"))
df_anonym_GENERIC_strictaverage_2_femnd$BL_age <- as.numeric(df_anonym_GENERIC_strictaverage_2_femnd$BL_age)
df_anonym_GENERIC_strictaverage_33_femnd$BL_age <- as.numeric(df_anonym_GENERIC_strictaverage_33_femnd$BL_age)
df_anonym_GENERIC_kanonymity_11_femnd$BL_age <- as.numeric(df_anonym_GENERIC_kanonymity_11_femnd$BL_age)
df_anonym_GENERIC_kanonymity_33_femnd$BL_age <- as.numeric(df_anonym_GENERIC_kanonymity_33_femnd$BL_age)
df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_age <- as.numeric(df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_age)
df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_age <- as.numeric(df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_age)
df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_age <- as.numeric(df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_age)
df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_age <- as.numeric(df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_age)
### all
ggplot() + 
  geom_density(data = df_origin_femnd, aes(BL_age, y = ..density..*20000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_age)), aes(BL_age), colour="indianred4", width = 10, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_age)), aes(BL_age), colour="indianred", width = 20, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_age)), aes(BL_age), colour="goldenrod4", width = 20, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33_femnd, !is.na(df_anonym_GENERIC_kanonymity_33_femnd$BL_age)), aes(BL_age), colour="goldenrod", width = 20, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_age)), aes(BL_age), colour="darkseagreen4", width = 5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_age)), aes(BL_age), colour="darkseagreen", width = 10, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_age)), aes(BL_age), colour="skyblue4", width = 10, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_age)), aes(BL_age), colour="skyblue", width = 10, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### GENERIC
ggplot() + 
  geom_density(data = df_origin_femnd, aes(BL_age, y = ..density..*20000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_age)), aes(BL_age), colour="indianred4", width = 10, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_age)), aes(BL_age), colour="indianred", width = 20, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_age)), aes(BL_age), colour="goldenrod4", width = 20, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33_femnd, !is.na(df_anonym_GENERIC_kanonymity_33_femnd$BL_age)), aes(BL_age), colour="goldenrod", width = 20, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1)
### USECASE
ggplot() + 
  geom_density(data = df_origin_femnd, aes(BL_age, y = ..density..*20000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_age)), aes(BL_age), colour="darkseagreen4", width = 5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_age)), aes(BL_age), colour="darkseagreen", width = 10, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_age)), aes(BL_age), colour="skyblue4", width = 10, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_age)), aes(BL_age), colour="skyblue", width = 10, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### strict average risk
ggplot() + 
  geom_density(data = df_origin_femnd, aes(BL_age, y = ..density..*20000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_age)), aes(BL_age), colour="indianred4", width = 10, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_age)), aes(BL_age), colour="indianred", width = 20, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_age)), aes(BL_age), colour="darkseagreen4", width = 5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_age)), aes(BL_age), colour="darkseagreen", width = 10, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
### k anonymity
ggplot() + 
  geom_density(data = df_origin_femnd, aes(BL_age, y = ..density..*20000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_age)), aes(BL_age), colour="goldenrod4", width = 20, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33_femnd, !is.na(df_anonym_GENERIC_kanonymity_33_femnd$BL_age)), aes(BL_age), colour="goldenrod", width = 20, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_age)), aes(BL_age), colour="skyblue4", width = 10, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_age)), aes(BL_age), colour="skyblue", width = 10, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
## Weight (only GENERIC)
df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_weight_lab <- mapvalues(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_weight, from = c("[40, 50[", "[50, 60[", "[60, 70[", 
                                                                                                                                     "[70, 80[", "[80, 90[", "[90, 100[", "[100, 110[", 
                                                                                                                                     "[110, 120[", "[120, 130[", "[130, 140["), 
                                                                      to = c("45", "55", "65", "75", "85", "95", "105", "115", "125", "135"))
df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_weight_lab <- mapvalues(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_weight, from = c("[40, 60[", "[60, 80[", "[80, 100[", "[100, 120[", 
                                                                                                                                       "[120, 140[", "[140, 160["), 
                                                                       to = c("50", "70", "90", "110", "130", "150"))
df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_weight_lab <- mapvalues(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_weight, from = c("[40, 80[", "[80, 120[", "[120, 160["), 
                                                                    to = c("60", "100", "140"))
df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_weight_lab <- mapvalues(df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_weight, from = c("[0, 80[", "[80, 160["), to = c("40", "120"))
df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_weight_lab <- as.numeric(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_weight_lab)
df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_weight_lab <- as.numeric(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_weight_lab)
df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_weight_lab <- as.numeric(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_weight_lab)
df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_weight_lab <- as.numeric(df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_weight_lab)
### GENERIC
ggplot() + 
  geom_density(data = df_origin_femnd, aes(BL_ku_weight, y = ..density..*40000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_weight_lab)), aes(BL_ku_weight_lab), colour="indianred4", width = 10, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_weight_lab)), aes(BL_ku_weight_lab), colour="indianred", width = 20, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_weight_lab)), aes(BL_ku_weight_lab), colour="goldenrod4", width = 40, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33_femnd, !is.na(df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_weight_lab)), aes(BL_ku_weight_lab), colour="goldenrod", width = 80, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
## Height (only GENERIC)
df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_height_cm_lab <- mapvalues(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_height_cm, from = c("[140, 150[", "[150, 160[", "[160, 170[", 
                                                                                                                                           "[170, 180[", "[180, 190["), to = c("145", "155", "165", "175", "185"))
df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_height_cm_lab <- mapvalues(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_height_cm, from = c("[140, 150[", "[150, 160[", "[160, 170[", 
                                                                                                                                             "[170, 180[", "[180, 190["), to = c("145", "155", "165", "175", "185"))
df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_height_cm_lab <- mapvalues(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_height_cm, from = c("[140, 150[", "[150, 160[", "[160, 170[", 
                                                                                                                                       "[170, 180["), to = c("145", "155", "165", "175"))
df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_height_cm_lab <- mapvalues(df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_height_cm, from = c("[140, 160[", "[160, 180["), to = c("150", "170"))
df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_height_cm_lab <- as.numeric(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_height_cm_lab)
df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_height_cm_lab <- as.numeric(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_height_cm_lab)
df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_height_cm_lab <- as.numeric(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_height_cm_lab)
df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_height_cm_lab <- as.numeric(df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_height_cm_lab)
### GENERIC
ggplot() + 
  geom_density(data = df_origin_femnd, aes(BL_ku_height_cm, y = ..density..*20000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_height_cm_lab)), aes(BL_ku_height_cm_lab), colour="indianred4", width = 10, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_height_cm_lab)), aes(BL_ku_height_cm_lab), colour="indianred", width = 10, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_height_cm_lab)), aes(BL_ku_height_cm_lab), colour="goldenrod4", width = 10, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33_femnd, !is.na(df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_height_cm_lab)), aes(BL_ku_height_cm_lab), colour="goldenrod", width = 20, fill="transparent") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "2nd")) + 
  scale_x_continuous(sec.axis = sec_axis(~., name = "2nd")) +
  geom_hline(yintercept=0, linetype="solid", color="white", size=1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
## BMI
### USECASE
df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi <- mapvalues(df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi, from = c("[15, 18.5[", "[18.5, 21[", "[21, 23[", "[23, 25[", 
                                                                                                                                   "[25, 27[", "[27, 30[", "[30, 32[", "[32, 35[", 
                                                                                                                                   "[35, 37[", "[37, 40[", "[40, 70["), 
                                                                   to = c("16.75", "19.75", "22", "24", "26", "28.5","31", "33.5", "36", "38.5", "55"))
df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi <- mapvalues(df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi, from = c("[15, 18.5[", "[18.5, 25[", "[25, 30[", "[30, 35[", 
                                                                                                                                     "[35, 40[", "[40, 70["), 
                                                                    to = c("16.75", "21.75", "27.5", "32.5","37.5", "55"))
df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi <- mapvalues(df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi, from = c("[18.5, 25[", "[25, 30[", "[30, 35[", "[35, 40[", 
                                                                                                                               "[40, 70["), to = c("21.75", "27.5", "32.5","37.5", "55"))
df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi <- mapvalues(df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi, from = c("[15, 25[", "[25, 30[", "[30, 70["), 
                                                                 to = c("20", "27.5", "50"))
df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi <- as.numeric(df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi)
df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi <- as.numeric(df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi)
df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi <- as.numeric(df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi)
df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi <- as.numeric(df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi)
### GENERIC: Calculation of BMI
#### (2,2)
df_anonym_GENERIC_strictaverage_2_femnd[c("height_cm_low", "height_cm_up")] <- str_split_fixed(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_height_cm, ", ", 2)
df_anonym_GENERIC_strictaverage_2_femnd$height_cm_low <- gsub("^.", "", as.character(df_anonym_GENERIC_strictaverage_2_femnd$height_cm_low))
df_anonym_GENERIC_strictaverage_2_femnd$height_cm_low <- as.numeric(df_anonym_GENERIC_strictaverage_2_femnd$height_cm_low)
df_anonym_GENERIC_strictaverage_2_femnd$height_cm_up <- gsub(".$", "", as.character(df_anonym_GENERIC_strictaverage_2_femnd$height_cm_up))
df_anonym_GENERIC_strictaverage_2_femnd$height_cm_up <- as.numeric(df_anonym_GENERIC_strictaverage_2_femnd$height_cm_up)
df_anonym_GENERIC_strictaverage_2_femnd[c("weight_low", "weight_up")] <- str_split_fixed(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_weight, ", ", 2)
df_anonym_GENERIC_strictaverage_2_femnd$weight_low <- gsub("^.", "", as.character(df_anonym_GENERIC_strictaverage_2_femnd$weight_low))
df_anonym_GENERIC_strictaverage_2_femnd$weight_low <- as.numeric(df_anonym_GENERIC_strictaverage_2_femnd$weight_low)
df_anonym_GENERIC_strictaverage_2_femnd$weight_up <- gsub(".$", "", as.character(df_anonym_GENERIC_strictaverage_2_femnd$weight_up))
df_anonym_GENERIC_strictaverage_2_femnd$weight_up <- as.numeric(df_anonym_GENERIC_strictaverage_2_femnd$weight_up)
#### replace arbitrary min/max (</>) hierarchy bounds with NA
df_anonym_GENERIC_strictaverage_2_femnd <- df_anonym_GENERIC_strictaverage_2_femnd %>% mutate_at(c("height_cm_low"), ~na_if(., 20))
df_anonym_GENERIC_strictaverage_2_femnd <- df_anonym_GENERIC_strictaverage_2_femnd %>% mutate_at(c("height_cm_up"), ~na_if(., 280))
df_anonym_GENERIC_strictaverage_2_femnd <- df_anonym_GENERIC_strictaverage_2_femnd %>% mutate_at(c("weight_low"), ~na_if(., 0))
df_anonym_GENERIC_strictaverage_2_femnd <- df_anonym_GENERIC_strictaverage_2_femnd %>% mutate_at(c("weight_up"), ~na_if(., 300))
#### calculate and illustrate BMI out of weight & height
df_anonym_GENERIC_strictaverage_2_femnd$BMI_low_calc <- as.numeric(df_anonym_GENERIC_strictaverage_2_femnd$weight_low/(df_anonym_GENERIC_strictaverage_2_femnd$height_cm_up/100)^2)
df_anonym_GENERIC_strictaverage_2_femnd$BMI_up_calc <- as.numeric(df_anonym_GENERIC_strictaverage_2_femnd$weight_up/(df_anonym_GENERIC_strictaverage_2_femnd$height_cm_low/100)^2)
df_anonym_GENERIC_strictaverage_2_femnd$BMI_up_calc <- round(df_anonym_GENERIC_strictaverage_2_femnd$BMI_up_calc ,digit=1)
df_anonym_GENERIC_strictaverage_2_femnd$BMI_low_calc <- round(df_anonym_GENERIC_strictaverage_2_femnd$BMI_low_calc ,digit=1)
df_anonym_GENERIC_strictaverage_2_femnd <- df_anonym_GENERIC_strictaverage_2_femnd %>% 
  mutate(BL_ku_bmi_calc = BMI_low_calc + ((BMI_up_calc-BMI_low_calc)/2))
df_anonym_GENERIC_strictaverage_2_femnd <- df_anonym_GENERIC_strictaverage_2_femnd %>% 
  mutate(BL_ku_bmi_calc_diff = BMI_up_calc - BMI_low_calc)
df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc <- round(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc, digit=2)
#### (33,2)
df_anonym_GENERIC_strictaverage_33_femnd[c("height_cm_low", "height_cm_up")] <- str_split_fixed(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_height_cm, ", ", 2)
df_anonym_GENERIC_strictaverage_33_femnd$height_cm_low <- gsub("^.", "", as.character(df_anonym_GENERIC_strictaverage_33_femnd$height_cm_low))
df_anonym_GENERIC_strictaverage_33_femnd$height_cm_low <- as.numeric(df_anonym_GENERIC_strictaverage_33_femnd$height_cm_low)
df_anonym_GENERIC_strictaverage_33_femnd$height_cm_up <- gsub(".$", "", as.character(df_anonym_GENERIC_strictaverage_33_femnd$height_cm_up))
df_anonym_GENERIC_strictaverage_33_femnd$height_cm_up <- as.numeric(df_anonym_GENERIC_strictaverage_33_femnd$height_cm_up)
df_anonym_GENERIC_strictaverage_33_femnd[c("weight_low", "weight_up")] <- str_split_fixed(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_weight, ", ", 2)
df_anonym_GENERIC_strictaverage_33_femnd$weight_low <- gsub("^.", "", as.character(df_anonym_GENERIC_strictaverage_33_femnd$weight_low))
df_anonym_GENERIC_strictaverage_33_femnd$weight_low <- as.numeric(df_anonym_GENERIC_strictaverage_33_femnd$weight_low)
df_anonym_GENERIC_strictaverage_33_femnd$weight_up <- gsub(".$", "", as.character(df_anonym_GENERIC_strictaverage_33_femnd$weight_up))
df_anonym_GENERIC_strictaverage_33_femnd$weight_up <- as.numeric(df_anonym_GENERIC_strictaverage_33_femnd$weight_up)
#### replace arbitrary min/max (</>) hierarchy bounds with NA
df_anonym_GENERIC_strictaverage_33_femnd <- df_anonym_GENERIC_strictaverage_33_femnd %>% mutate_at(c("height_cm_low"), ~na_if(., 20))
df_anonym_GENERIC_strictaverage_33_femnd <- df_anonym_GENERIC_strictaverage_33_femnd %>% mutate_at(c("height_cm_up"), ~na_if(., 280))
df_anonym_GENERIC_strictaverage_33_femnd <- df_anonym_GENERIC_strictaverage_33_femnd %>% mutate_at(c("weight_low"), ~na_if(., 0))
df_anonym_GENERIC_strictaverage_33_femnd <- df_anonym_GENERIC_strictaverage_33_femnd %>% mutate_at(c("weight_up"), ~na_if(., 300))
#### calculate and illustrate BMI out of weight & height
df_anonym_GENERIC_strictaverage_33_femnd$BMI_low_calc <- as.numeric(df_anonym_GENERIC_strictaverage_33_femnd$weight_low/(df_anonym_GENERIC_strictaverage_33_femnd$height_cm_up/100)^2)
df_anonym_GENERIC_strictaverage_33_femnd$BMI_up_calc <- as.numeric(df_anonym_GENERIC_strictaverage_33_femnd$weight_up/(df_anonym_GENERIC_strictaverage_33_femnd$height_cm_low/100)^2)
df_anonym_GENERIC_strictaverage_33_femnd$BMI_up_calc <- round(df_anonym_GENERIC_strictaverage_33_femnd$BMI_up_calc ,digit=1)
df_anonym_GENERIC_strictaverage_33_femnd$BMI_low_calc <- round(df_anonym_GENERIC_strictaverage_33_femnd$BMI_low_calc ,digit=1)
df_anonym_GENERIC_strictaverage_33_femnd <- df_anonym_GENERIC_strictaverage_33_femnd %>% 
  mutate(BL_ku_bmi_calc = BMI_low_calc + ((BMI_up_calc-BMI_low_calc)/2))
df_anonym_GENERIC_strictaverage_33_femnd <- df_anonym_GENERIC_strictaverage_33_femnd %>% 
  mutate(BL_ku_bmi_calc_diff = BMI_up_calc - BMI_low_calc)
df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc <- round(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc, digit=2)
#### 11-anonymity
df_anonym_GENERIC_kanonymity_11_femnd[c("height_cm_low", "height_cm_up")] <- str_split_fixed(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_height_cm, ", ", 2)
df_anonym_GENERIC_kanonymity_11_femnd$height_cm_low <- gsub("^.", "", as.character(df_anonym_GENERIC_kanonymity_11_femnd$height_cm_low))
df_anonym_GENERIC_kanonymity_11_femnd$height_cm_low <- as.numeric(df_anonym_GENERIC_kanonymity_11_femnd$height_cm_low)
df_anonym_GENERIC_kanonymity_11_femnd$height_cm_up <- gsub(".$", "", as.character(df_anonym_GENERIC_kanonymity_11_femnd$height_cm_up))
df_anonym_GENERIC_kanonymity_11_femnd$height_cm_up <- as.numeric(df_anonym_GENERIC_kanonymity_11_femnd$height_cm_up)
df_anonym_GENERIC_kanonymity_11_femnd[c("weight_low", "weight_up")] <- str_split_fixed(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_weight, ", ", 2)
df_anonym_GENERIC_kanonymity_11_femnd$weight_low <- gsub("^.", "", as.character(df_anonym_GENERIC_kanonymity_11_femnd$weight_low))
df_anonym_GENERIC_kanonymity_11_femnd$weight_low <- as.numeric(df_anonym_GENERIC_kanonymity_11_femnd$weight_low)
df_anonym_GENERIC_kanonymity_11_femnd$weight_up <- gsub(".$", "", as.character(df_anonym_GENERIC_kanonymity_11_femnd$weight_up))
df_anonym_GENERIC_kanonymity_11_femnd$weight_up <- as.numeric(df_anonym_GENERIC_kanonymity_11_femnd$weight_up)
#### replace arbitrary min/max (</>) hierarchy bounds with NA
df_anonym_GENERIC_kanonymity_11_femnd <- df_anonym_GENERIC_kanonymity_11_femnd %>% mutate_at(c("height_cm_low"), ~na_if(., 20))
df_anonym_GENERIC_kanonymity_11_femnd <- df_anonym_GENERIC_kanonymity_11_femnd %>% mutate_at(c("height_cm_up"), ~na_if(., 280))
df_anonym_GENERIC_kanonymity_11_femnd <- df_anonym_GENERIC_kanonymity_11_femnd %>% mutate_at(c("weight_low"), ~na_if(., 0))
df_anonym_GENERIC_kanonymity_11_femnd <- df_anonym_GENERIC_kanonymity_11_femnd %>% mutate_at(c("weight_up"), ~na_if(., 300))
#### calculate and illustrate BMI out of weight & height
df_anonym_GENERIC_kanonymity_11_femnd$BMI_low_calc <- as.numeric(df_anonym_GENERIC_kanonymity_11_femnd$weight_low/(df_anonym_GENERIC_kanonymity_11_femnd$height_cm_up/100)^2)
df_anonym_GENERIC_kanonymity_11_femnd$BMI_up_calc <- as.numeric(df_anonym_GENERIC_kanonymity_11_femnd$weight_up/(df_anonym_GENERIC_kanonymity_11_femnd$height_cm_low/100)^2)
df_anonym_GENERIC_kanonymity_11_femnd$BMI_up_calc <- round(df_anonym_GENERIC_kanonymity_11_femnd$BMI_up_calc ,digit=1)
df_anonym_GENERIC_kanonymity_11_femnd$BMI_low_calc <- round(df_anonym_GENERIC_kanonymity_11_femnd$BMI_low_calc ,digit=1)
df_anonym_GENERIC_kanonymity_11_femnd <- df_anonym_GENERIC_kanonymity_11_femnd %>% 
  mutate(BL_ku_bmi_calc = BMI_low_calc + ((BMI_up_calc-BMI_low_calc)/2))
df_anonym_GENERIC_kanonymity_11_femnd <- df_anonym_GENERIC_kanonymity_11_femnd %>% 
  mutate(BL_ku_bmi_calc_diff = BMI_up_calc - BMI_low_calc)
df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc <- round(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc, digit=2)
#### 33-anonymity
df_anonym_GENERIC_kanonymity_33_femnd[c("height_cm_low", "height_cm_up")] <- str_split_fixed(df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_height_cm, ", ", 2)
df_anonym_GENERIC_kanonymity_33_femnd$height_cm_low <- gsub("^.", "", as.character(df_anonym_GENERIC_kanonymity_33_femnd$height_cm_low))
df_anonym_GENERIC_kanonymity_33_femnd$height_cm_low <- as.numeric(df_anonym_GENERIC_kanonymity_33_femnd$height_cm_low)
df_anonym_GENERIC_kanonymity_33_femnd$height_cm_up <- gsub(".$", "", as.character(df_anonym_GENERIC_kanonymity_33_femnd$height_cm_up))
df_anonym_GENERIC_kanonymity_33_femnd$height_cm_up <- as.numeric(df_anonym_GENERIC_kanonymity_33_femnd$height_cm_up)
df_anonym_GENERIC_kanonymity_33_femnd[c("weight_low", "weight_up")] <- str_split_fixed(df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_weight, ", ", 2)
df_anonym_GENERIC_kanonymity_33_femnd$weight_low <- gsub("^.", "", as.character(df_anonym_GENERIC_kanonymity_33_femnd$weight_low))
df_anonym_GENERIC_kanonymity_33_femnd$weight_low <- as.numeric(df_anonym_GENERIC_kanonymity_33_femnd$weight_low)
df_anonym_GENERIC_kanonymity_33_femnd$weight_up <- gsub(".$", "", as.character(df_anonym_GENERIC_kanonymity_33_femnd$weight_up))
df_anonym_GENERIC_kanonymity_33_femnd$weight_up <- as.numeric(df_anonym_GENERIC_kanonymity_33_femnd$weight_up)
#### replace arbitrary min/max (</>) hierarchy bounds with NA
df_anonym_GENERIC_kanonymity_33_femnd <- df_anonym_GENERIC_kanonymity_33_femnd %>% mutate_at(c("height_cm_low"), ~na_if(., 20))
df_anonym_GENERIC_kanonymity_33_femnd <- df_anonym_GENERIC_kanonymity_33_femnd %>% mutate_at(c("height_cm_up"), ~na_if(., 280))
df_anonym_GENERIC_kanonymity_33_femnd <- df_anonym_GENERIC_kanonymity_33_femnd %>% mutate_at(c("weight_low"), ~na_if(., 0))
df_anonym_GENERIC_kanonymity_33_femnd <- df_anonym_GENERIC_kanonymity_33_femnd %>% mutate_at(c("weight_up"), ~na_if(., 300))
#### calculate and illustrate BMI out of weight & height
df_anonym_GENERIC_kanonymity_33_femnd$BMI_low_calc <- as.numeric(df_anonym_GENERIC_kanonymity_33_femnd$weight_low/(df_anonym_GENERIC_kanonymity_33_femnd$height_cm_up/100)^2)
df_anonym_GENERIC_kanonymity_33_femnd$BMI_up_calc <- as.numeric(df_anonym_GENERIC_kanonymity_33_femnd$weight_up/(df_anonym_GENERIC_kanonymity_33_femnd$height_cm_low/100)^2)
df_anonym_GENERIC_kanonymity_33_femnd$BMI_up_calc <- round(df_anonym_GENERIC_kanonymity_33_femnd$BMI_up_calc ,digit=1)
df_anonym_GENERIC_kanonymity_33_femnd$BMI_low_calc <- round(df_anonym_GENERIC_kanonymity_33_femnd$BMI_low_calc ,digit=1)
df_anonym_GENERIC_kanonymity_33_femnd <- df_anonym_GENERIC_kanonymity_33_femnd %>% 
  mutate(BL_ku_bmi_calc = BMI_low_calc + ((BMI_up_calc-BMI_low_calc)/2))
df_anonym_GENERIC_kanonymity_33_femnd <- df_anonym_GENERIC_kanonymity_33_femnd %>% 
  mutate(BL_ku_bmi_calc_diff = BMI_up_calc - BMI_low_calc)
df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_bmi_calc <- round(df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_bmi_calc, digit=2)
### Figure
#### all

#### GENERIC
ggplot() + 
  geom_density(data = df_origin_femnd, aes(BL_ku_bmi, y = ..density..*8000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 18.1), aes(BL_ku_bmi_calc), colour="indianred4", width = 5.4, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 25), aes(BL_ku_bmi_calc), colour="indianred4", width = 5.6, fill="transparent")  +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 16.65 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 21.35)), aes(BL_ku_bmi_calc), colour="indianred4", width = 5.7, fill="transparent")  +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 20.35 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 24.65)), aes(BL_ku_bmi_calc), colour="indianred4", width = 6.1, fill="transparent")  +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 27.9), aes(BL_ku_bmi_calc), colour="indianred4", width = 6.4, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 24.05 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 33.75)), aes(BL_ku_bmi_calc), colour="indianred4", width = 6.5, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 18.9), aes(BL_ku_bmi_calc), colour="indianred4", width = 6.6, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 31.2), aes(BL_ku_bmi_calc), colour="indianred4", width = 6.8, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 27.7), aes(BL_ku_bmi_calc), colour="indianred4", width = 7, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 23.1 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 34.5)), aes(BL_ku_bmi_calc), colour="indianred4", width = 7.2, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 31.45 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 37.75)), aes(BL_ku_bmi_calc), colour="indianred4", width = 7.5, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 21.65 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 27.25)), aes(BL_ku_bmi_calc), colour="indianred4", width = 7.7, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 35.1 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 41)), aes(BL_ku_bmi_calc), colour="indianred4", width = 8, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 26.4 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 38.8)), aes(BL_ku_bmi_calc), colour="indianred4", width = 8.4, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 35.6 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 42.5)), aes(BL_ku_bmi_calc), colour="indianred4", width = 8.8, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 39.8), aes(BL_ku_bmi_calc), colour="indianred4", width = 9.2, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 46.15), aes(BL_ku_bmi_calc), colour="indianred4", width = 9.3, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 35.95 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 49.85)), aes(BL_ku_bmi_calc), colour="indianred4", width = 9.7, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 44), aes(BL_ku_bmi_calc), colour="indianred4", width = 9.8, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 48.15), aes(BL_ku_bmi_calc), colour="indianred4", width = 10.3, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 52.35), aes(BL_ku_bmi_calc), colour="indianred4", width = 10.9, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 45.5), aes(BL_ku_bmi_calc), colour="indianred4", width = 11, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 20.65), aes(BL_ku_bmi_calc), colour="indianred", width = 8.1, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 16.55), aes(BL_ku_bmi_calc), colour="indianred", width = 8.5, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 26.55), aes(BL_ku_bmi_calc), colour="indianred", width = 8.7, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 23.1), aes(BL_ku_bmi_calc), colour="indianred", width = 9.2, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 18.6), aes(BL_ku_bmi_calc), colour="indianred", width = 9.6, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 29.65), aes(BL_ku_bmi_calc), colour="indianred", width = 9.9, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 26), aes(BL_ku_bmi_calc), colour="indianred", width = 10.4, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 36.2), aes(BL_ku_bmi_calc), colour="indianred", width = 10.6, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 21.15), aes(BL_ku_bmi_calc), colour="indianred", width = 11.1, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 33.4 | df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 42.7)), aes(BL_ku_bmi_calc), colour="indianred", width = 11.4, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 29.5), aes(BL_ku_bmi_calc), colour="indianred", width = 12.2, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 40.75), aes(BL_ku_bmi_calc), colour="indianred", width = 12.3, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 24.2), aes(BL_ku_bmi_calc), colour="indianred", width = 12.8, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 37.8 | df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 48.1)), aes(BL_ku_bmi_calc), colour="indianred", width = 13.2, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 33.75 | df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 55.45)), aes(BL_ku_bmi_calc), colour="indianred", width = 14.1, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 46.2), aes(BL_ku_bmi_calc), colour="indianred", width = 14.2, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 54.55), aes(BL_ku_bmi_calc), colour="indianred", width = 15.3, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 43.3), aes(BL_ku_bmi_calc), colour="indianred", width = 15.4, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc == 20), aes(BL_ku_bmi_calc), colour="goldenrod4", width = 15.4, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc == 33.1), aes(BL_ku_bmi_calc), colour="goldenrod4", width = 16.8, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc == 22.5), aes(BL_ku_bmi_calc), colour="goldenrod4", width = 17.4, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc == 37.3), aes(BL_ku_bmi_calc), colour="goldenrod4", width = 19.2, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc == 25.6), aes(BL_ku_bmi_calc), colour="goldenrod4", width = 20, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc == 52), aes(BL_ku_bmi_calc), colour="goldenrod4", width = 21, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc == 42.25), aes(BL_ku_bmi_calc), colour="goldenrod4", width = 22.1, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc == 29.3), aes(BL_ku_bmi_calc), colour="goldenrod4", width = 23, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33_femnd, !is.na(df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_bmi_calc == 43.6), aes(BL_ku_bmi_calc), colour="goldenrod", width = 13.1, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33_femnd, !is.na(df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_bmi_calc == 56.4), aes(BL_ku_bmi_calc), colour="goldenrod", width = 19.2, fill="transparent") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
#### USECASE
ggplot() + 
  geom_density(data = df_origin_femnd, aes(BL_ku_bmi, y = ..density..*8000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 16.75 & (df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 22 & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 24 & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 26 & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 28.5 & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 33.5 & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 38.5 & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 55 & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 31 & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 36)), aes(BL_ku_bmi), colour="darkseagreen4", width = 2.5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 16.75), aes(BL_ku_bmi), colour="darkseagreen4", width = 3.5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi) & (df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 22 | df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 24 | df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 26 | df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 31 | df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 36)), aes(BL_ku_bmi), colour="darkseagreen4", width = 2, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi) & (df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 28.5 | df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 33.5 | df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 38.5)), aes(BL_ku_bmi), colour="darkseagreen4", width = 3, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 55), aes(BL_ku_bmi), colour="darkseagreen4", width = 30, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi) & (df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi == 32.5 | df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi == 37.5 | df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi == 27.5)), aes(BL_ku_bmi), colour="darkseagreen", width = 5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi == 21.75), aes(BL_ku_bmi), colour="darkseagreen", width = 6.5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi == 16.75), aes(BL_ku_bmi), colour="darkseagreen", width = 3.5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi == 55), aes(BL_ku_bmi), colour="darkseagreen", width = 30, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi) & (df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi == 27.5 | df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi == 32.5 | df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi == 37.5)), aes(BL_ku_bmi), colour="skyblue4", width = 5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi == 21.75), aes(BL_ku_bmi), colour="skyblue4", width = 6.5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi == 55), aes(BL_ku_bmi), colour="skyblue4", width = 30, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi == 20), aes(BL_ku_bmi), colour="skyblue", width = 10, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi == 27.5), aes(BL_ku_bmi), colour="skyblue", width = 5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi == 50), aes(BL_ku_bmi), colour="skyblue", width = 40, fill="transparent") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
#### strictaverage
ggplot() + 
  geom_density(data = df_origin_femnd, aes(BL_ku_bmi, y = ..density..*8000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 18.1), aes(BL_ku_bmi_calc), colour="indianred4", width = 5.4, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 25), aes(BL_ku_bmi_calc), colour="indianred4", width = 5.6, fill="transparent")  +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 16.65 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 21.35)), aes(BL_ku_bmi_calc), colour="indianred4", width = 5.7, fill="transparent")  +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 20.35 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 24.65)), aes(BL_ku_bmi_calc), colour="indianred4", width = 6.1, fill="transparent")  +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 27.9), aes(BL_ku_bmi_calc), colour="indianred4", width = 6.4, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 24.05 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 33.75)), aes(BL_ku_bmi_calc), colour="indianred4", width = 6.5, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 18.9), aes(BL_ku_bmi_calc), colour="indianred4", width = 6.6, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 31.2), aes(BL_ku_bmi_calc), colour="indianred4", width = 6.8, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 27.7), aes(BL_ku_bmi_calc), colour="indianred4", width = 7, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 23.1 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 34.5)), aes(BL_ku_bmi_calc), colour="indianred4", width = 7.2, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 31.45 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 37.75)), aes(BL_ku_bmi_calc), colour="indianred4", width = 7.5, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 21.65 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 27.25)), aes(BL_ku_bmi_calc), colour="indianred4", width = 7.7, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 35.1 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 41)), aes(BL_ku_bmi_calc), colour="indianred4", width = 8, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 26.4 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 38.8)), aes(BL_ku_bmi_calc), colour="indianred4", width = 8.4, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 35.6 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 42.5)), aes(BL_ku_bmi_calc), colour="indianred4", width = 8.8, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 39.8), aes(BL_ku_bmi_calc), colour="indianred4", width = 9.2, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 46.15), aes(BL_ku_bmi_calc), colour="indianred4", width = 9.3, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 35.95 | df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 49.85)), aes(BL_ku_bmi_calc), colour="indianred4", width = 9.7, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 44), aes(BL_ku_bmi_calc), colour="indianred4", width = 9.8, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 48.15), aes(BL_ku_bmi_calc), colour="indianred4", width = 10.3, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 52.35), aes(BL_ku_bmi_calc), colour="indianred4", width = 10.9, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2_femnd, !is.na(df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_2_femnd$BL_ku_bmi_calc == 45.5), aes(BL_ku_bmi_calc), colour="indianred4", width = 11, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 20.65), aes(BL_ku_bmi_calc), colour="indianred", width = 8.1, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 16.55), aes(BL_ku_bmi_calc), colour="indianred", width = 8.5, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 26.55), aes(BL_ku_bmi_calc), colour="indianred", width = 8.7, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 23.1), aes(BL_ku_bmi_calc), colour="indianred", width = 9.2, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 18.6), aes(BL_ku_bmi_calc), colour="indianred", width = 9.6, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 29.65), aes(BL_ku_bmi_calc), colour="indianred", width = 9.9, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 26), aes(BL_ku_bmi_calc), colour="indianred", width = 10.4, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 36.2), aes(BL_ku_bmi_calc), colour="indianred", width = 10.6, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 21.15), aes(BL_ku_bmi_calc), colour="indianred", width = 11.1, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 33.4 | df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 42.7)), aes(BL_ku_bmi_calc), colour="indianred", width = 11.4, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 29.5), aes(BL_ku_bmi_calc), colour="indianred", width = 12.2, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 40.75), aes(BL_ku_bmi_calc), colour="indianred", width = 12.3, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 24.2), aes(BL_ku_bmi_calc), colour="indianred", width = 12.8, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 37.8 | df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 48.1)), aes(BL_ku_bmi_calc), colour="indianred", width = 13.2, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & (df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 33.75 | df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 55.45)), aes(BL_ku_bmi_calc), colour="indianred", width = 14.1, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 46.2), aes(BL_ku_bmi_calc), colour="indianred", width = 14.2, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 54.55), aes(BL_ku_bmi_calc), colour="indianred", width = 15.3, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 43.3), aes(BL_ku_bmi_calc), colour="indianred", width = 15.4, fill="transparent")  +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 16.75 & (df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 22 & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 24 & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 26 & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 28.5 & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 33.5 & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 38.5 & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 55 & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 31 & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi != 36)), aes(BL_ku_bmi), colour="darkseagreen4", width = 2.5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 16.75), aes(BL_ku_bmi), colour="darkseagreen4", width = 3.5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi) & (df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 22 | df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 24 | df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 26 | df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 31 | df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 36)), aes(BL_ku_bmi), colour="darkseagreen4", width = 2, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi) & (df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 28.5 | df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 33.5 | df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 38.5)), aes(BL_ku_bmi), colour="darkseagreen4", width = 3, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_strictaverage_2_femnd$BL_ku_bmi == 55), aes(BL_ku_bmi), colour="darkseagreen4", width = 30, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi) & (df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi == 32.5 | df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi == 37.5 | df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi == 27.5)), aes(BL_ku_bmi), colour="darkseagreen", width = 5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi == 21.75), aes(BL_ku_bmi), colour="darkseagreen", width = 6.5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi == 16.75), aes(BL_ku_bmi), colour="darkseagreen", width = 3.5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33_femnd, !is.na(df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_strictaverage_33_femnd$BL_ku_bmi == 55), aes(BL_ku_bmi), colour="darkseagreen", width = 30, fill="transparent")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#### kanonymity
ggplot() + 
  geom_density(data = df_origin_femnd, aes(BL_ku_bmi, y = ..density..*8000), colour="white", alpha = 0.5, fill="azure4") +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33_femnd, !is.na(df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_strictaverage_33_femnd$BL_ku_bmi_calc == 43.3), aes(BL_ku_bmi_calc), colour="indianred", width = 15.4, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc == 20), aes(BL_ku_bmi_calc), colour="goldenrod4", width = 15.4, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc == 33.1), aes(BL_ku_bmi_calc), colour="goldenrod4", width = 16.8, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc == 22.5), aes(BL_ku_bmi_calc), colour="goldenrod4", width = 17.4, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc == 37.3), aes(BL_ku_bmi_calc), colour="goldenrod4", width = 19.2, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc == 25.6), aes(BL_ku_bmi_calc), colour="goldenrod4", width = 20, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc == 52), aes(BL_ku_bmi_calc), colour="goldenrod4", width = 21, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc == 42.25), aes(BL_ku_bmi_calc), colour="goldenrod4", width = 22.1, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11_femnd, !is.na(df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_11_femnd$BL_ku_bmi_calc == 29.3), aes(BL_ku_bmi_calc), colour="goldenrod4", width = 23, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33_femnd, !is.na(df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_bmi_calc == 43.6), aes(BL_ku_bmi_calc), colour="goldenrod", width = 13.1, fill="transparent") +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33_femnd, !is.na(df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_bmi_calc) & df_anonym_GENERIC_kanonymity_33_femnd$BL_ku_bmi_calc == 56.4), aes(BL_ku_bmi_calc), colour="goldenrod", width = 19.2, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi) & (df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi == 27.5 | df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi == 32.5 | df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi == 37.5)), aes(BL_ku_bmi), colour="skyblue4", width = 5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi == 21.75), aes(BL_ku_bmi), colour="skyblue4", width = 6.5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi == 55), aes(BL_ku_bmi), colour="skyblue4", width = 30, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi == 20), aes(BL_ku_bmi), colour="skyblue", width = 10, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi == 27.5), aes(BL_ku_bmi), colour="skyblue", width = 5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi == 50), aes(BL_ku_bmi), colour="skyblue", width = 40, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi) & (df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi == 27.5 | df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi == 32.5 | df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi == 37.5)), aes(BL_ku_bmi), colour="skyblue4", width = 5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi == 21.75), aes(BL_ku_bmi), colour="skyblue4", width = 6.5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_kanonymity_11_femnd$BL_ku_bmi == 55), aes(BL_ku_bmi), colour="skyblue4", width = 30, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi == 20), aes(BL_ku_bmi), colour="skyblue", width = 10, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi == 27.5), aes(BL_ku_bmi), colour="skyblue", width = 5, fill="transparent") +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33_femnd, !is.na(df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi) & df_anonym_USECASE_4QI_kanonymity_33_femnd$BL_ku_bmi == 50), aes(BL_ku_bmi), colour="skyblue", width = 40, fill="transparent")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

### illustration of intervals
#### strictaverage 2
df_anonym_GENERIC_strictaverage_2_femnd <- df_anonym_GENERIC_strictaverage_2_femnd %>% 
  mutate(BMI_group = ifelse(BMI_low_calc == 13.8, 1, 
                            ifelse(BMI_low_calc == 15.4, 2, 
                                   ifelse(BMI_low_calc == 15.6, 3, 
                                          ifelse(BMI_low_calc == 17.3, 4, 
                                                 ifelse(BMI_low_calc == 17.8, 5,
                                                        ifelse(BMI_low_calc == 18.5, 6,
                                                               ifelse(BMI_low_calc == 19.5, 7,
                                                                      ifelse(BMI_low_calc == 20.8, 8,
                                                                             ifelse(BMI_low_calc == 21.6, 9,
                                                                                    ifelse(BMI_low_calc == 22.2 & BMI_up_calc == 27.8, 10, 
                                                                                           ifelse(BMI_low_calc == 22.2 & BMI_up_calc == 30.6, 11, 
                                                                                                  ifelse(BMI_low_calc == 23.4, 12,
                                                                                                         ifelse(BMI_low_calc == 24.2, 13,
                                                                                                                ifelse(BMI_low_calc == 24.7, 14,
                                                                                                                       ifelse(BMI_low_calc == 26.7, 15,
                                                                                                                              ifelse(BMI_low_calc == 27.3, 16, 
                                                                                                                                     ifelse(BMI_low_calc == 27.7, 17, 
                                                                                                                                            ifelse(BMI_low_calc == 27.8, 18, 
                                                                                                                                                   ifelse(BMI_low_calc == 30.5, 19, 
                                                                                                                                                          ifelse(BMI_low_calc == 30.9, 20, 
                                                                                                                                                                 ifelse(BMI_low_calc == 31.1 & BMI_up_calc == 39.1, 21, 
                                                                                                                                                                        ifelse(BMI_low_calc == 31.1 & BMI_up_calc == 40.8, 22, 
                                                                                                                                                                               ifelse(BMI_low_calc == 31.2, 23, 
                                                                                                                                                                                      ifelse(BMI_low_calc == 34, 24, 
                                                                                                                                                                                             ifelse(BMI_low_calc == 34.6, 25, 
                                                                                                                                                                                                    ifelse(BMI_low_calc == 35.2, 26,
                                                                                                                                                                                                           ifelse(BMI_low_calc == 37, 27, 
                                                                                                                                                                                                                  ifelse(BMI_low_calc == 38.1, 28, 
                                                                                                                                                                                                                         ifelse(BMI_low_calc == 39.1, 29,
                                                                                                                                                                                                                                ifelse(BMI_low_calc == 40, 30,
                                                                                                                                                                                                                                       ifelse(BMI_low_calc == 41.5, 31,
                                                                                                                                                                                                                                              ifelse(BMI_low_calc == 43, 32,
                                                                                                                                                                                                                                                     ifelse(BMI_low_calc == 45, 33, 
                                                                                                                                                                                                                                                            ifelse(BMI_low_calc == 46.9, 34,
                                                                                                                                                                                                                                                                   9999)))))))))))))))))))))))))))))))))))
df_anonym_GENERIC_strictaverage_2_femnd$ID <- seq.int(nrow(df_anonym_GENERIC_strictaverage_2_femnd))
df_anonym_GENERIC_strictaverage_2_femnd_low <- df_anonym_GENERIC_strictaverage_2_femnd[,c("BMI_low_calc", "ID", "BMI_group")]
df_anonym_GENERIC_strictaverage_2_femnd_low$bound = "low"
names(df_anonym_GENERIC_strictaverage_2_femnd_low)[names(df_anonym_GENERIC_strictaverage_2_femnd_low) == "BMI_low_calc"] <- "BMI"
df_anonym_GENERIC_strictaverage_2_femnd_up <- df_anonym_GENERIC_strictaverage_2_femnd[,c("BMI_up_calc", "ID", "BMI_group")]
df_anonym_GENERIC_strictaverage_2_femnd_up$bound = "up"
names(df_anonym_GENERIC_strictaverage_2_femnd_up)[names(df_anonym_GENERIC_strictaverage_2_femnd_up) == "BMI_up_calc"] <- "BMI"
df_anonym_GENERIC_strictaverage_2_femnd_bmi <- rbind(df_anonym_GENERIC_strictaverage_2_femnd_low, df_anonym_GENERIC_strictaverage_2_femnd_up)
ggplot(data=df_anonym_GENERIC_strictaverage_2_femnd_bmi, aes(x = BMI, y = BMI_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3) +
  ylim(0, 35) +
  xlim(0, 85) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
#### strictaverage 33
df_anonym_GENERIC_strictaverage_33_femnd <- df_anonym_GENERIC_strictaverage_33_femnd %>% 
  mutate(BMI_group = ifelse(BMI_low_calc == 12.3, 1, 
                            ifelse(BMI_low_calc == 13.8, 2, 
                                   ifelse(BMI_low_calc == 15.6, 3, 
                                          ifelse(BMI_low_calc == 16.6, 4, 
                                                 ifelse(BMI_low_calc == 17.8, 5,
                                                        ifelse(BMI_low_calc == 18.5, 6,
                                                               ifelse(BMI_low_calc == 20.8, 7,
                                                                      ifelse(BMI_low_calc == 22.2, 8,
                                                                             ifelse(BMI_low_calc == 23.4, 9,
                                                                                    ifelse(BMI_low_calc == 24.7, 10, 
                                                                                           ifelse(BMI_low_calc == 26.7, 11, 
                                                                                                  ifelse(BMI_low_calc == 27.7, 12,
                                                                                                         ifelse(BMI_low_calc == 30.9, 13,
                                                                                                                ifelse(BMI_low_calc == 31.2, 14,
                                                                                                                       ifelse(BMI_low_calc == 34.6, 15,
                                                                                                                              ifelse(BMI_low_calc == 35.6, 16, 
                                                                                                                                     ifelse(BMI_low_calc == 37, 17, 
                                                                                                                                            ifelse(BMI_low_calc == 39.1, 18, 
                                                                                                                                                   ifelse(BMI_low_calc == 41.5, 19, 
                                                                                                                                                          ifelse(BMI_low_calc == 46.9, 20, 
                                                                                                                                                                 ifelse(BMI_low_calc == 48.4, 21,
                                                                                                                                                                        9999))))))))))))))))))))))
df_anonym_GENERIC_strictaverage_33_femnd$ID <- seq.int(nrow(df_anonym_GENERIC_strictaverage_33_femnd))
df_anonym_GENERIC_strictaverage_33_femnd_low <- df_anonym_GENERIC_strictaverage_33_femnd[,c("BMI_low_calc", "ID", "BMI_group")]
df_anonym_GENERIC_strictaverage_33_femnd_low$bound = "low"
names(df_anonym_GENERIC_strictaverage_33_femnd_low)[names(df_anonym_GENERIC_strictaverage_33_femnd_low) == "BMI_low_calc"] <- "BMI"
df_anonym_GENERIC_strictaverage_33_femnd_up <- df_anonym_GENERIC_strictaverage_33_femnd[,c("BMI_up_calc", "ID", "BMI_group")]
df_anonym_GENERIC_strictaverage_33_femnd_up$bound = "up"
names(df_anonym_GENERIC_strictaverage_33_femnd_up)[names(df_anonym_GENERIC_strictaverage_33_femnd_up) == "BMI_up_calc"] <- "BMI"
df_anonym_GENERIC_strictaverage_33_femnd_bmi <- rbind(df_anonym_GENERIC_strictaverage_33_femnd_low, df_anonym_GENERIC_strictaverage_33_femnd_up)
ggplot(data=df_anonym_GENERIC_strictaverage_33_femnd_bmi, aes(x = BMI, y = BMI_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3) +
  ylim(0, 35) +
  xlim(0, 85) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
#### kanonymity 11
df_anonym_GENERIC_kanonymity_11_femnd <- df_anonym_GENERIC_kanonymity_11_femnd %>% 
  mutate(BMI_group = ifelse(BMI_low_calc == 12.3, 1, 
                            ifelse(BMI_low_calc == 13.8, 2, 
                                   ifelse(BMI_low_calc == 15.6, 3, 
                                          ifelse(BMI_low_calc == 17.8, 4, 
                                                 ifelse(BMI_low_calc == 24.7, 5,
                                                        ifelse(BMI_low_calc == 27.7, 6,
                                                               ifelse(BMI_low_calc == 31.2, 7,
                                                                      ifelse(BMI_low_calc == 41.5, 8, 
                                                                             9999)))))))))
df_anonym_GENERIC_kanonymity_11_femnd$ID <- seq.int(nrow(df_anonym_GENERIC_kanonymity_11_femnd))
df_anonym_GENERIC_kanonymity_11_femnd_low <- df_anonym_GENERIC_kanonymity_11_femnd[,c("BMI_low_calc", "ID", "BMI_group")]
df_anonym_GENERIC_kanonymity_11_femnd_low$bound = "low"
names(df_anonym_GENERIC_kanonymity_11_femnd_low)[names(df_anonym_GENERIC_kanonymity_11_femnd_low) == "BMI_low_calc"] <- "BMI"
df_anonym_GENERIC_kanonymity_11_femnd_up <- df_anonym_GENERIC_kanonymity_11_femnd[,c("BMI_up_calc", "ID", "BMI_group")]
df_anonym_GENERIC_kanonymity_11_femnd_up$bound = "up"
names(df_anonym_GENERIC_kanonymity_11_femnd_up)[names(df_anonym_GENERIC_kanonymity_11_femnd_up) == "BMI_up_calc"] <- "BMI"
df_anonym_GENERIC_kanonymity_11_femnd_bmi <- rbind(df_anonym_GENERIC_kanonymity_11_femnd_low, df_anonym_GENERIC_kanonymity_11_femnd_up)
ggplot(data=df_anonym_GENERIC_kanonymity_11_femnd_bmi, aes(x = BMI, y = BMI_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3)+
  ylim(0, 35) +
  xlim(0, 85) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
#### kanonymity 33
df_anonym_GENERIC_kanonymity_33_femnd <- df_anonym_GENERIC_kanonymity_33_femnd %>% 
  mutate(BMI_group = ifelse(BMI_low_calc == 24.7, 1, 
                            ifelse(BMI_low_calc == 31.2, 2,
                                   9999)))
df_anonym_GENERIC_kanonymity_33_femnd$ID <- seq.int(nrow(df_anonym_GENERIC_kanonymity_33_femnd))
df_anonym_GENERIC_kanonymity_33_femnd_low <- df_anonym_GENERIC_kanonymity_33_femnd[,c("BMI_low_calc", "ID", "BMI_group")]
df_anonym_GENERIC_kanonymity_33_femnd_low$bound = "low"
names(df_anonym_GENERIC_kanonymity_33_femnd_low)[names(df_anonym_GENERIC_kanonymity_33_femnd_low) == "BMI_low_calc"] <- "BMI"
df_anonym_GENERIC_kanonymity_33_femnd_up <- df_anonym_GENERIC_kanonymity_33_femnd[,c("BMI_up_calc", "ID", "BMI_group")]
df_anonym_GENERIC_kanonymity_33_femnd_up$bound = "up"
names(df_anonym_GENERIC_kanonymity_33_femnd_up)[names(df_anonym_GENERIC_kanonymity_33_femnd_up) == "BMI_up_calc"] <- "BMI"
df_anonym_GENERIC_kanonymity_33_femnd_bmi <- rbind(df_anonym_GENERIC_kanonymity_33_femnd_low, df_anonym_GENERIC_kanonymity_33_femnd_up)
ggplot(data=df_anonym_GENERIC_kanonymity_33_femnd_bmi, aes(x = BMI, y = BMI_group, group = ID)) + 
  geom_line() +
  geom_point(shape=18, size=3) +
  ylim(0, 35) +
  xlim(0, 85) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())



