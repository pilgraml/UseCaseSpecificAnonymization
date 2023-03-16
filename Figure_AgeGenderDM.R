# Packages 
pacman::p_load(tidyr, stringr, dplyr, openxlsx, naniar, emmeans, multcomp, 
               plyr, finalfit, ggplot2, tibble, lmtest, sandwich,
               tidyverse, tidyselect, summarytools, scales, gridExtra, 
               lubridate, eeptools, gtsummary, flextable, boot, mosaic, 
               patchwork, rms, coxed, DescTools, PropCIs)

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
var <- c("BL_age", "diabetes", "dem_sex")
df_origin$diabetes <- mapvalues(df_origin$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_anonym_GENERIC_strictaverage_2$diabetes <- mapvalues(df_anonym_GENERIC_strictaverage_2$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_anonym_GENERIC_strictaverage_33$diabetes <- mapvalues(df_anonym_GENERIC_strictaverage_33$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_anonym_GENERIC_kanonymity_11$diabetes <- mapvalues(df_anonym_GENERIC_kanonymity_11$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_anonym_GENERIC_kanonymity_33$diabetes <- mapvalues(df_anonym_GENERIC_kanonymity_33$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_anonym_USECASE_4QI_strictaverage_2$diabetes <- mapvalues(df_anonym_USECASE_4QI_strictaverage_2$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_anonym_USECASE_4QI_strictaverage_33$diabetes <- mapvalues(df_anonym_USECASE_4QI_strictaverage_33$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_anonym_USECASE_4QI_kanonymity_11$diabetes <- mapvalues(df_anonym_USECASE_4QI_kanonymity_11$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_anonym_USECASE_4QI_kanonymity_33$diabetes <- mapvalues(df_anonym_USECASE_4QI_kanonymity_33$diabetes , from = c("1", "2"), to = c("diabetes_yes", "diabetes_no"))
df_origin <- df_origin %>% mutate(
  BL_age_cat = ifelse(df_origin$BL_age < 20, 1, ifelse(df_origin$BL_age >= 20 & df_origin$BL_age < 30, 2, ifelse(
    df_origin$BL_age >= 30 & df_origin$BL_age < 40, 3, ifelse(df_origin$BL_age >= 40 & df_origin$BL_age < 50, 4, ifelse(
      df_origin$BL_age >= 50 & df_origin$BL_age < 60, 5, ifelse(df_origin$BL_age >= 60 & df_origin$BL_age < 70, 6, 7)))))))#### kanonymity
### 11-anonymity
df_anonym_GENERIC_kanonymity_11 <- df_anonym_GENERIC_kanonymity_11 %>% mutate(
  BL_age_cat = ifelse(df_anonym_GENERIC_kanonymity_11$BL_age == "[20,40[", 2.5, ifelse(df_anonym_GENERIC_kanonymity_11$BL_age == "[40, 60[", 4.5, 6.5)))
df_anonym_USECASE_4QI_kanonymity_11 <- df_anonym_USECASE_4QI_kanonymity_11 %>% mutate(
  BL_age_cat = ifelse(df_anonym_USECASE_4QI_kanonymity_11$BL_age == "[20, 30[", 2, ifelse(
    df_anonym_USECASE_4QI_kanonymity_11$BL_age == "[30, 40[", 3, ifelse(df_anonym_USECASE_4QI_kanonymity_11$BL_age == "[40, 50[", 4, ifelse(
      df_anonym_USECASE_4QI_kanonymity_11$BL_age == "[50, 60[", 5, ifelse(df_anonym_USECASE_4QI_kanonymity_11$BL_age == "[60, 70[", 6, 7))))))
### 33-anonymity
df_anonym_GENERIC_kanonymity_33 <- df_anonym_GENERIC_kanonymity_33 %>% mutate(
  BL_age_cat = ifelse(df_anonym_GENERIC_kanonymity_33$BL_age == "[20,40[", 2.5, ifelse(df_anonym_GENERIC_kanonymity_33$BL_age == "[40, 60[", 4.5, 6.5)))
df_anonym_USECASE_4QI_kanonymity_33 <- df_anonym_USECASE_4QI_kanonymity_33 %>% mutate(
  BL_age_cat = ifelse(df_anonym_USECASE_4QI_kanonymity_33$BL_age == "[30, 40[", 3, ifelse(df_anonym_USECASE_4QI_kanonymity_33$BL_age == "[40, 50[", 4, ifelse(
    df_anonym_USECASE_4QI_kanonymity_33$BL_age == "[50, 60[", 5, ifelse(df_anonym_USECASE_4QI_kanonymity_33$BL_age == "[60, 70[", 6, 7)))))
### 2,2 strict average
df_anonym_GENERIC_strictaverage_2 <- df_anonym_GENERIC_strictaverage_2 %>% mutate(
  BL_age_cat = ifelse(df_anonym_GENERIC_strictaverage_2$BL_age == "[20, 30[", 2, ifelse(
    df_anonym_GENERIC_strictaverage_2$BL_age == "[30, 40[", 3, ifelse(df_anonym_GENERIC_strictaverage_2$BL_age == "[40, 50[", 4, ifelse(
      df_anonym_GENERIC_strictaverage_2$BL_age == "[50, 60[", 5, ifelse(df_anonym_GENERIC_strictaverage_2$BL_age == "[60, 70[", 6, 7))))))
df_anonym_USECASE_4QI_strictaverage_2 <- df_anonym_USECASE_4QI_strictaverage_2 %>% mutate(
  BL_age_cat = ifelse(df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[18, 20[", 1, ifelse(df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[20, 25[" | df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[25, 30[", 2, ifelse(
    df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[30, 35[" | df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[35, 40[", 3, ifelse(df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[40, 45[" | df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[45, 50[", 4, ifelse(
      df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[50, 55[" | df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[55, 60[", 5, ifelse(df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[60, 65[" | df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[65, 70[", 6, 7)))))))
df_anonym_USECASE_4QI_strictaverage_2 <- df_anonym_USECASE_4QI_strictaverage_2 %>% mutate(
  BL_age_cat_fine = ifelse(df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[18, 20[", 1.25, ifelse(df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[20, 25[", 1.75, ifelse(df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[25, 30[", 2.25, ifelse(
    df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[30, 35[", 2.75, ifelse(df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[35, 40[", 3.25, ifelse(df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[40, 45[", 3.75, ifelse(df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[45, 50[", 4.25, ifelse(
      df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[50, 55[", 4.75, ifelse(df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[55, 60[", 5.25, ifelse(df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[60, 65[", 5.75, ifelse(df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[65, 70[", 6.25, ifelse(
        df_anonym_USECASE_4QI_strictaverage_2$BL_age == "[70, 75[", 6.75, 7.25)))))))))))))
### 33,2 strict average
df_anonym_GENERIC_strictaverage_33 <- df_anonym_GENERIC_strictaverage_33 %>% mutate(
  BL_age_cat = ifelse(df_anonym_GENERIC_strictaverage_33$BL_age == "[20,40[", 2.5, ifelse(df_anonym_GENERIC_strictaverage_33$BL_age == "[40, 60[", 4.5, 6.5)))
df_anonym_USECASE_4QI_strictaverage_33 <- df_anonym_USECASE_4QI_strictaverage_33 %>% mutate(
  BL_age_cat = ifelse(df_anonym_USECASE_4QI_strictaverage_33$BL_age == "[18, 20[", 1, ifelse(df_anonym_USECASE_4QI_strictaverage_33$BL_age == "[20, 30[", 2, ifelse(
    df_anonym_USECASE_4QI_strictaverage_33$BL_age == "[30, 40[", 3, ifelse(df_anonym_USECASE_4QI_strictaverage_33$BL_age == "[40, 50[", 4, ifelse(
      df_anonym_USECASE_4QI_strictaverage_33$BL_age == "[50, 60[", 5, ifelse(df_anonym_USECASE_4QI_strictaverage_33$BL_age == "[60, 70[", 6, 7)))))))

## figures
### Stratification by gender
#### original
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
#### strict average risk single
ggplot() +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2, df_anonym_GENERIC_strictaverage_2$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "indianred4", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2, df_anonym_GENERIC_strictaverage_2$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "indianred4", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "darkseagreen4", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "darkseagreen4", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$dem_sex == "Male"), 
           aes(BL_age_cat_fine), fill = "darkseagreen4", colour = "white", width = 0.5) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$dem_sex == "Female"), 
           aes(BL_age_cat_fine), fill = "darkseagreen4", colour = "white", width = 0.5) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "indianred", colour = "white", width = 2.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "indianred", colour = "white", width = 2.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "darkseagreen", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "darkseagreen", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
#### k anonymity single 
ggplot() +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11, df_anonym_GENERIC_kanonymity_11$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "goldenrod4", colour = "white", width = 2.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11, df_anonym_GENERIC_kanonymity_11$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "goldenrod4", colour = "white", width = 2.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11, df_anonym_USECASE_4QI_kanonymity_11$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "skyblue4", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11, df_anonym_USECASE_4QI_kanonymity_11$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "skyblue4", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "goldenrod", colour = "white", width = 2.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "goldenrod", colour = "white", width = 2.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "skyblue", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "skyblue", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
#### strict average risk
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2, df_anonym_GENERIC_strictaverage_2$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred4", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen4", width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred", width = 2.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2) 
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2, df_anonym_GENERIC_strictaverage_2$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred4", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen4", width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred", width = 2.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2, df_anonym_GENERIC_strictaverage_2$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred4", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$dem_sex == "Male"), 
           aes(BL_age_cat_fine), fill = "transparent", colour = "darkseagreen4", width = 0.5) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred", width = 2.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2, df_anonym_GENERIC_strictaverage_2$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred4", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$dem_sex == "Female"), 
           aes(BL_age_cat_fine), fill = "transparent", colour = "darkseagreen4", width = 0.5) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred", width = 2.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)
#### kanonymity
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11, df_anonym_GENERIC_kanonymity_11$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "goldenrod4", width = 2.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11, df_anonym_USECASE_4QI_kanonymity_11$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "skyblue4", width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "goldenrod", width = 2.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "skyblue", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11, df_anonym_GENERIC_kanonymity_11$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "goldenrod4", width = 2.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11, df_anonym_USECASE_4QI_kanonymity_11$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "skyblue4", width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "goldenrod", width = 2.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "skyblue", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)
#### GENERIC
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2, df_anonym_GENERIC_strictaverage_2$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred4", width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33, df_anonym_GENERIC_strictaverage_33$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred", width = 2.0) +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11, df_anonym_GENERIC_kanonymity_11$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "goldenrod4", width = 2.0) +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33, df_anonym_GENERIC_kanonymity_33$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "goldenrod", width = 2.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2, df_anonym_GENERIC_strictaverage_2$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred4", width = 1.0)+
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33, df_anonym_GENERIC_strictaverage_33$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred", width = 2.0) +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11, df_anonym_GENERIC_kanonymity_11$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "goldenrod4", width = 2.0) +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33, df_anonym_GENERIC_kanonymity_33$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "goldenrod", width = 2.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
#### USECASE
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen4", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11, df_anonym_USECASE_4QI_kanonymity_11$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "skyblue4", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$dem_sex == "Male"), 
           aes(BL_age_cat), fill = "transparent", colour = "skyblue", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen4", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11, df_anonym_USECASE_4QI_kanonymity_11$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "skyblue4", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$dem_sex == "Female"), 
           aes(BL_age_cat), fill = "transparent", colour = "skyblue", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")

### Stratification by diabetes
#### original
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
#### strict average risk single
ggplot() +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2, df_anonym_GENERIC_strictaverage_2$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "indianred4", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2, df_anonym_GENERIC_strictaverage_2$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "indianred4", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "darkseagreen4", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "darkseagreen4", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$diabetes == "diabetes_yes"), 
           aes(BL_age_cat_fine), fill = "darkseagreen4", colour = "white", width = 0.5) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$diabetes == "diabetes_no"), 
           aes(BL_age_cat_fine), fill = "darkseagreen4", colour = "white", width = 0.5) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "indianred", colour = "white", width = 2.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "indianred", colour = "white", width = 2.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "darkseagreen", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "darkseagreen", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
#### k anonymity single 
ggplot() +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11, df_anonym_GENERIC_kanonymity_11$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "goldenrod4", colour = "white", width = 2.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11, df_anonym_GENERIC_kanonymity_11$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "goldenrod4", colour = "white", width = 2.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11, df_anonym_USECASE_4QI_kanonymity_11$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "skyblue4", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11, df_anonym_USECASE_4QI_kanonymity_11$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "skyblue4", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "goldenrod", colour = "white", width = 2.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "goldenrod", colour = "white", width = 2.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "skyblue", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "skyblue", colour = "white", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
#### strict average risk
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2, df_anonym_GENERIC_strictaverage_2$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred4", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen4", width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred", width = 2.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2, df_anonym_GENERIC_strictaverage_2$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred4", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen4", width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred", width = 2.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")

ggplot() +
  geom_bar(data = subset(df_origin, df_origin$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2, df_anonym_GENERIC_strictaverage_2$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred4", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$diabetes == "diabetes_yes"), 
           aes(BL_age_cat_fine), fill = "transparent", colour = "darkseagreen4", width = 0.5) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred", width = 2.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2, df_anonym_GENERIC_strictaverage_2$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred4", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$diabetes == "diabetes_no"), 
           aes(BL_age_cat_fine), fill = "transparent", colour = "darkseagreen4", width = 0.5) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred", width = 2.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")

#### kanonymity
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11, df_anonym_GENERIC_kanonymity_11$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "goldenrod4", width = 2.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11, df_anonym_USECASE_4QI_kanonymity_11$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "skyblue4", width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "goldenrod", width = 2.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "skyblue", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2)
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11, df_anonym_GENERIC_kanonymity_11$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "goldenrod4", width = 2.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11, df_anonym_USECASE_4QI_kanonymity_11$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "skyblue4", width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "goldenrod", width = 2.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "skyblue", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2) 
#### GENERIC
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2, df_anonym_GENERIC_strictaverage_2$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred4", width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33, df_anonym_GENERIC_strictaverage_33$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred", width = 2.0) +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11, df_anonym_GENERIC_kanonymity_11$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "goldenrod4", width = 2.0) +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33, df_anonym_GENERIC_kanonymity_33$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "goldenrod", width = 2.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_2, df_anonym_GENERIC_strictaverage_2$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred4", width = 1.0)+
  geom_bar(data = subset(df_anonym_GENERIC_strictaverage_33, df_anonym_GENERIC_strictaverage_33$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "indianred", width = 2.0) +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_11, df_anonym_GENERIC_kanonymity_11$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "goldenrod4", width = 2.0) +
  geom_bar(data = subset(df_anonym_GENERIC_kanonymity_33, df_anonym_GENERIC_kanonymity_33$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "goldenrod", width = 2.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
#### USECASE
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen4", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11, df_anonym_USECASE_4QI_kanonymity_11$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "skyblue4", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$diabetes == "diabetes_yes"), 
           aes(BL_age_cat), fill = "transparent", colour = "skyblue", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")
ggplot() +
  geom_bar(data = subset(df_origin, df_origin$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "azure4", colour = "white", alpha = 0.5, width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_2, df_anonym_USECASE_4QI_strictaverage_2$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen4", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_strictaverage_33, df_anonym_USECASE_4QI_strictaverage_33$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "darkseagreen", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_11, df_anonym_USECASE_4QI_kanonymity_11$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "skyblue4", width = 1.0) +
  geom_bar(data = subset(df_anonym_USECASE_4QI_kanonymity_33, df_anonym_USECASE_4QI_kanonymity_33$diabetes == "diabetes_no"), 
           aes(BL_age_cat), fill = "transparent", colour = "skyblue", width = 1.0) +
  scale_y_continuous(limits=c(0,2250), breaks=c(0,500,1000,1500,2000)) + 
  scale_x_continuous(limits=c(0,7.5), breaks=c(0,1,2,3,4,5,6,7)) +
  coord_flip() +
  theme(aspect.ratio = 3/2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = "none")