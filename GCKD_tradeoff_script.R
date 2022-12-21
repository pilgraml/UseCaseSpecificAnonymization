# Packages 
pacman::p_load(tidyr, stringr, dplyr, openxlsx, naniar, emmeans, multcomp, 
               plyr, finalfit, ggplot2, tibble, lmtest, sandwich,
               tidyverse, tidyselect, summarytools, scales, gridExtra, 
               lubridate, eeptools, gtsummary, flextable, boot, mosaic, 
               patchwork, rms, coxed, DescTools, PropCIs)

# Dataset
path_tbl = "C:/Users/User/OneDrive/Documents/PRIVAT/Charite/Forschung/Projekt Computerbasierte Anonymisierung/Titzeetal/Ergebnisse/Tbl_perc_CI"
setwd(path_tbl)

# Comparing anonymization approaches tbl1
tbl1_CIoverlap_kanonymity_generic_11 <- as_tibble(read.xlsx("tbl1_CIoverlap_kanonymity_generic_11.xlsx", sep = ";"))
tbl1_CIoverlap_strictaverage_generic_11 <- as_tibble(read.xlsx("tbl1_CIoverlap_strictaverage_generic_11.xlsx", sep = ";"))
tbl1_CIoverlap_kanonymity_usecase_11 <- as_tibble(read.xlsx("tbl1_CIoverlap_kanonymity_usecase_11.xlsx", sep = ";"))
tbl1_CIoverlap_strictaverage_usecase_11 <- as_tibble(read.xlsx("tbl1_CIoverlap_strictaverage_usecase_11.xlsx", sep = ";"))
DatasetLevelStatistics_GCKD <- as_tibble(read.xlsx("DatasetLevelStatistics_GCKD.xlsx", sep = ";"))

## Avg per variable: needed for calculation of avg per table
tbl1_CIoverlap_kanonymity_generic_11$Variable_avg <-apply(tbl1_CIoverlap_kanonymity_generic_11[sapply(tbl1_CIoverlap_kanonymity_generic_11, is.numeric)],1,mean)
tbl1_CIoverlap_strictaverage_generic_11$Variable_avg <-apply(tbl1_CIoverlap_strictaverage_generic_11[sapply(tbl1_CIoverlap_strictaverage_generic_11, is.numeric)],1,mean)
tbl1_CIoverlap_kanonymity_usecase_11$Variable_avg <-apply(tbl1_CIoverlap_kanonymity_usecase_11[sapply(tbl1_CIoverlap_kanonymity_usecase_11, is.numeric)],1,mean)
tbl1_CIoverlap_strictaverage_usecase_11$Variable_avg <-apply(tbl1_CIoverlap_strictaverage_usecase_11[sapply(tbl1_CIoverlap_strictaverage_usecase_11, is.numeric)],1,mean)
## Avg per subset: currently not needed
#colMeans(tbl1_CIoverlap_kanonymity_generic_11[sapply(tbl1_CIoverlap_kanonymity_generic_11, is.numeric)])
#colMeans(tbl1_CIoverlap_strictaverage_generic_11[sapply(tbl1_CIoverlap_strictaverage_generic_11, is.numeric)])
#colMeans(tbl1_CIoverlap_kanonymity_usecase_11[sapply(tbl1_CIoverlap_kanonymity_usecase_11, is.numeric)])
#colMeans(tbl1_CIoverlap_strictaverage_usecase_11[sapply(tbl1_CIoverlap_strictaverage_usecase_11, is.numeric)])
## Avg female non diabetics tbl1
DatasetLevelStatistics_GCKD$tbl1_female_nd_avg[DatasetLevelStatistics_GCKD$setup == "GENERIC" & 
                                                 DatasetLevelStatistics_GCKD$risk_model == "kanonymity" &
                                                 DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl1_CIoverlap_kanonymity_generic_11$CI_overlap_female_nd)
DatasetLevelStatistics_GCKD$tbl1_female_nd_avg[DatasetLevelStatistics_GCKD$setup == "GENERIC" & 
                                                 DatasetLevelStatistics_GCKD$risk_model == "strictaverage" &
                                                 DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl1_CIoverlap_strictaverage_generic_11$CI_overlap_female_nd)
DatasetLevelStatistics_GCKD$tbl1_female_nd_avg[DatasetLevelStatistics_GCKD$setup == "SPECIFIC" & 
                                                 DatasetLevelStatistics_GCKD$risk_model == "kanonymity" &
                                                 DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl1_CIoverlap_kanonymity_usecase_11$CI_overlap_female_nd)
DatasetLevelStatistics_GCKD$tbl1_female_nd_avg[DatasetLevelStatistics_GCKD$setup == "SPECIFIC" & 
                                                 DatasetLevelStatistics_GCKD$risk_model == "strictaverage" &
                                                 DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl1_CIoverlap_strictaverage_usecase_11$CI_overlap_female_nd)
## Avg tbl1
DatasetLevelStatistics_GCKD$tbl1_avg[DatasetLevelStatistics_GCKD$setup == "GENERIC" & 
                                       DatasetLevelStatistics_GCKD$risk_model == "kanonymity" &
                                       DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl1_CIoverlap_kanonymity_generic_11$Variable_avg)
DatasetLevelStatistics_GCKD$tbl1_avg[DatasetLevelStatistics_GCKD$setup == "GENERIC" & 
                                       DatasetLevelStatistics_GCKD$risk_model == "strictaverage" &
                                       DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl1_CIoverlap_strictaverage_generic_11$Variable_avg)
DatasetLevelStatistics_GCKD$tbl1_avg[DatasetLevelStatistics_GCKD$setup == "SPECIFIC" & 
                                       DatasetLevelStatistics_GCKD$risk_model == "kanonymity" &
                                       DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl1_CIoverlap_kanonymity_usecase_11$Variable_avg)
DatasetLevelStatistics_GCKD$tbl1_avg[DatasetLevelStatistics_GCKD$setup == "SPECIFIC" & 
                                       DatasetLevelStatistics_GCKD$risk_model == "strictaverage" &
                                       DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl1_CIoverlap_strictaverage_usecase_11$Variable_avg)
# Comparing anonymization approaches tbl2
tbl2_CIoverlap_kanonymity_generic_11 <- as_tibble(read.xlsx("tbl2_CIoverlap_kanonymity_generic_11.xlsx", sep = ";"))
tbl2_CIoverlap_strictaverage_generic_11 <- as_tibble(read.xlsx("tbl2_CIoverlap_strictaverage_generic_11.xlsx", sep = ";"))
tbl2_CIoverlap_kanonymity_usecase_11 <- as_tibble(read.xlsx("tbl2_CIoverlap_kanonymity_usecase_11.xlsx", sep = ";"))
tbl2_CIoverlap_strictaverage_usecase_11 <- as_tibble(read.xlsx("tbl2_CIoverlap_strictaverage_usecase_11.xlsx", sep = ";"))
## Avg per variable: needed for calculation of avg per table
tbl2_CIoverlap_kanonymity_generic_11$Variable_avg <-apply(tbl2_CIoverlap_kanonymity_generic_11[sapply(tbl2_CIoverlap_kanonymity_generic_11, is.numeric)],1,mean)
tbl2_CIoverlap_strictaverage_generic_11$Variable_avg <-apply(tbl2_CIoverlap_strictaverage_generic_11[sapply(tbl2_CIoverlap_strictaverage_generic_11, is.numeric)],1,mean)
tbl2_CIoverlap_kanonymity_usecase_11$Variable_avg <-apply(tbl2_CIoverlap_kanonymity_usecase_11[sapply(tbl2_CIoverlap_kanonymity_usecase_11, is.numeric)],1,mean)
tbl2_CIoverlap_strictaverage_usecase_11$Variable_avg <-apply(tbl2_CIoverlap_strictaverage_usecase_11[sapply(tbl2_CIoverlap_strictaverage_usecase_11, is.numeric)],1,mean)
## Avg tbl2
DatasetLevelStatistics_GCKD$tbl2_avg[DatasetLevelStatistics_GCKD$setup == "GENERIC" & 
                                       DatasetLevelStatistics_GCKD$risk_model == "kanonymity" &
                                       DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl2_CIoverlap_kanonymity_generic_11$Variable_avg)
DatasetLevelStatistics_GCKD$tbl2_avg[DatasetLevelStatistics_GCKD$setup == "GENERIC" & 
                                       DatasetLevelStatistics_GCKD$risk_model == "strictaverage" &
                                       DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl2_CIoverlap_strictaverage_generic_11$Variable_avg)
DatasetLevelStatistics_GCKD$tbl2_avg[DatasetLevelStatistics_GCKD$setup == "SPECIFIC" & 
                                       DatasetLevelStatistics_GCKD$risk_model == "kanonymity" &
                                       DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl2_CIoverlap_kanonymity_usecase_11$Variable_avg)
DatasetLevelStatistics_GCKD$tbl2_avg[DatasetLevelStatistics_GCKD$setup == "SPECIFIC" & 
                                       DatasetLevelStatistics_GCKD$risk_model == "strictaverage" &
                                       DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl2_CIoverlap_strictaverage_usecase_11$Variable_avg)
# Comparing anonymization approaches tbl3
tbl3_CIoverlap_kanonymity_generic_11 <- as_tibble(read.xlsx("tbl3_CIoverlap_kanonymity_generic_11.xlsx", sep = ";"))
tbl3_CIoverlap_strictaverage_generic_11 <- as_tibble(read.xlsx("tbl3_CIoverlap_strictaverage_generic_11.xlsx", sep = ";"))
tbl3_CIoverlap_kanonymity_usecase_11 <- as_tibble(read.xlsx("tbl3_CIoverlap_kanonymity_usecase_11.xlsx", sep = ";"))
tbl3_CIoverlap_strictaverage_usecase_11 <- as_tibble(read.xlsx("tbl3_CIoverlap_strictaverage_usecase_11.xlsx", sep = ";"))
## Avg per variable: needed for calculation of avg per table
tbl3_CIoverlap_kanonymity_generic_11$Variable_avg <-apply(tbl3_CIoverlap_kanonymity_generic_11[sapply(tbl3_CIoverlap_kanonymity_generic_11, is.numeric)],1,mean)
tbl3_CIoverlap_strictaverage_generic_11$Variable_avg <-apply(tbl3_CIoverlap_strictaverage_generic_11[sapply(tbl3_CIoverlap_strictaverage_generic_11, is.numeric)],1,mean)
tbl3_CIoverlap_kanonymity_usecase_11$Variable_avg <-apply(tbl3_CIoverlap_kanonymity_usecase_11[sapply(tbl3_CIoverlap_kanonymity_usecase_11, is.numeric)],1,mean)
tbl3_CIoverlap_strictaverage_usecase_11$Variable_avg <-apply(tbl3_CIoverlap_strictaverage_usecase_11[sapply(tbl3_CIoverlap_strictaverage_usecase_11, is.numeric)],1,mean)
## Avg tbl3
DatasetLevelStatistics_GCKD$tbl3_avg[DatasetLevelStatistics_GCKD$setup == "GENERIC" & 
                                       DatasetLevelStatistics_GCKD$risk_model == "kanonymity" &
                                       DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl3_CIoverlap_kanonymity_generic_11$Variable_avg)
DatasetLevelStatistics_GCKD$tbl3_avg[DatasetLevelStatistics_GCKD$setup == "GENERIC" & 
                                       DatasetLevelStatistics_GCKD$risk_model == "strictaverage" &
                                       DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl3_CIoverlap_strictaverage_generic_11$Variable_avg)
DatasetLevelStatistics_GCKD$tbl3_avg[DatasetLevelStatistics_GCKD$setup == "SPECIFIC" & 
                                       DatasetLevelStatistics_GCKD$risk_model == "kanonymity" &
                                       DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl3_CIoverlap_kanonymity_usecase_11$Variable_avg)
DatasetLevelStatistics_GCKD$tbl3_avg[DatasetLevelStatistics_GCKD$setup == "SPECIFIC" & 
                                       DatasetLevelStatistics_GCKD$risk_model == "strictaverage" &
                                       DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl3_CIoverlap_strictaverage_usecase_11$Variable_avg)
# Comparing anonymization approaches tbl4
tbl4_CIoverlap_kanonymity_generic_11 <- as_tibble(read.xlsx("tbl4_CIoverlap_kanonymity_generic_11.xlsx", sep = ";"))
tbl4_CIoverlap_strictaverage_generic_11 <- as_tibble(read.xlsx("tbl4_CIoverlap_strictaverage_generic_11.xlsx", sep = ";"))
tbl4_CIoverlap_kanonymity_usecase_11 <- as_tibble(read.xlsx("tbl4_CIoverlap_kanonymity_usecase_11.xlsx", sep = ";"))
tbl4_CIoverlap_strictaverage_usecase_11 <- as_tibble(read.xlsx("tbl4_CIoverlap_strictaverage_usecase_11.xlsx", sep = ";"))
## Avg per variable: needed for calculation of avg per table
tbl4_CIoverlap_kanonymity_generic_11$Variable_avg <-apply(tbl4_CIoverlap_kanonymity_generic_11[sapply(tbl4_CIoverlap_kanonymity_generic_11, is.numeric)],1,mean)
tbl4_CIoverlap_strictaverage_generic_11$Variable_avg <-apply(tbl4_CIoverlap_strictaverage_generic_11[sapply(tbl4_CIoverlap_strictaverage_generic_11, is.numeric)],1,mean)
tbl4_CIoverlap_kanonymity_usecase_11$Variable_avg <-apply(tbl4_CIoverlap_kanonymity_usecase_11[sapply(tbl4_CIoverlap_kanonymity_usecase_11, is.numeric)],1,mean)
tbl4_CIoverlap_strictaverage_usecase_11$Variable_avg <-apply(tbl4_CIoverlap_strictaverage_usecase_11[sapply(tbl4_CIoverlap_strictaverage_usecase_11, is.numeric)],1,mean)
## Avg tbl4
DatasetLevelStatistics_GCKD$tbl4_avg[DatasetLevelStatistics_GCKD$setup == "GENERIC" & 
                                       DatasetLevelStatistics_GCKD$risk_model == "kanonymity" &
                                       DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl4_CIoverlap_kanonymity_generic_11$Variable_avg)
DatasetLevelStatistics_GCKD$tbl4_avg[DatasetLevelStatistics_GCKD$setup == "GENERIC" & 
                                       DatasetLevelStatistics_GCKD$risk_model == "strictaverage" &
                                       DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl4_CIoverlap_strictaverage_generic_11$Variable_avg)
DatasetLevelStatistics_GCKD$tbl4_avg[DatasetLevelStatistics_GCKD$setup == "SPECIFIC" & 
                                       DatasetLevelStatistics_GCKD$risk_model == "kanonymity" &
                                       DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl4_CIoverlap_kanonymity_usecase_11$Variable_avg)
DatasetLevelStatistics_GCKD$tbl4_avg[DatasetLevelStatistics_GCKD$setup == "SPECIFIC" & 
                                       DatasetLevelStatistics_GCKD$risk_model == "strictaverage" &
                                       DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbl4_CIoverlap_strictaverage_usecase_11$Variable_avg)
# Comparing anonymization approaches tbls3
tbls3_CIoverlap_kanonymity_generic_11 <- as_tibble(read.xlsx("tbls3_CIoverlap_kanonymity_generic_11.xlsx", sep = ";"))
tbls3_CIoverlap_strictaverage_generic_11 <- as_tibble(read.xlsx("tbls3_CIoverlap_strictaverage_generic_11.xlsx", sep = ";"))
tbls3_CIoverlap_kanonymity_usecase_11 <- as_tibble(read.xlsx("tbls3_CIoverlap_kanonymity_usecase_11.xlsx", sep = ";"))
tbls3_CIoverlap_strictaverage_usecase_11 <- as_tibble(read.xlsx("tbls3_CIoverlap_strictaverage_usecase_11.xlsx", sep = ";"))
## Avg per variable: needed for calculation of avg per table
tbls3_CIoverlap_kanonymity_generic_11$Variable_avg <-apply(tbls3_CIoverlap_kanonymity_generic_11[sapply(tbls3_CIoverlap_kanonymity_generic_11, is.numeric)],1,mean)
tbls3_CIoverlap_strictaverage_generic_11$Variable_avg <-apply(tbls3_CIoverlap_strictaverage_generic_11[sapply(tbls3_CIoverlap_strictaverage_generic_11, is.numeric)],1,mean)
tbls3_CIoverlap_kanonymity_usecase_11$Variable_avg <-apply(tbls3_CIoverlap_kanonymity_usecase_11[sapply(tbls3_CIoverlap_kanonymity_usecase_11, is.numeric)],1,mean)
tbls3_CIoverlap_strictaverage_usecase_11$Variable_avg <-apply(tbls3_CIoverlap_strictaverage_usecase_11[sapply(tbls3_CIoverlap_strictaverage_usecase_11, is.numeric)],1,mean)
## Avg tbls3
DatasetLevelStatistics_GCKD$tbls3_avg[DatasetLevelStatistics_GCKD$setup == "GENERIC" & 
                                        DatasetLevelStatistics_GCKD$risk_model == "kanonymity" &
                                        DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbls3_CIoverlap_kanonymity_generic_11$Variable_avg)
DatasetLevelStatistics_GCKD$tbls3_avg[DatasetLevelStatistics_GCKD$setup == "GENERIC" & 
                                        DatasetLevelStatistics_GCKD$risk_model == "strictaverage" &
                                        DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbls3_CIoverlap_strictaverage_generic_11$Variable_avg)
DatasetLevelStatistics_GCKD$tbls3_avg[DatasetLevelStatistics_GCKD$setup == "SPECIFIC" & 
                                        DatasetLevelStatistics_GCKD$risk_model == "kanonymity" &
                                        DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbls3_CIoverlap_kanonymity_usecase_11$Variable_avg)
DatasetLevelStatistics_GCKD$tbls3_avg[DatasetLevelStatistics_GCKD$setup == "SPECIFIC" & 
                                        DatasetLevelStatistics_GCKD$risk_model == "strictaverage" &
                                        DatasetLevelStatistics_GCKD$k == "11"] <- mean(tbls3_CIoverlap_strictaverage_usecase_11$Variable_avg)
# Combining: Avg all
## exclusively affected tbls
DatasetLevelStatistics_GCKD$all_affected_avg <- rowMeans(subset(DatasetLevelStatistics_GCKD, select = c(tbl1_avg, tbl2_avg, tbl3_avg, tbl4_avg, tbls3_avg)))
## including all tbls (+ fig2 (= GFR), + tbls2)
DatasetLevelStatistics_GCKD <- DatasetLevelStatistics_GCKD %>% mutate(fig2_avg = 100.0)
DatasetLevelStatistics_GCKD <- DatasetLevelStatistics_GCKD %>% mutate(tbls2_avg = 100.0)
DatasetLevelStatistics_GCKD$all_avg <- rowMeans(subset(DatasetLevelStatistics_GCKD, select = c(tbl1_avg, tbl2_avg, fig2_avg, tbl3_avg, tbl4_avg, tbls2_avg, tbls3_avg)))


## Formatting final output
DatasetLevelStatistics_GCKD <- DatasetLevelStatistics_GCKD %>% mutate(across(where(is.numeric), ~round(., 1)))

# Comparing generic purpose utility and privacy metrics
DatasetLevelStatistics_GCKD <- DatasetLevelStatistics_GCKD %>% mutate(privacy_marketer = 100-Risk_Marketer)
DatasetLevelStatistics_GCKD <- DatasetLevelStatistics_GCKD %>% mutate(privacy_prosecuter = 100-Risk_Prosecuter)
DatasetLevelStatistics_GCKD$id <- paste(DatasetLevelStatistics_GCKD$setup, DatasetLevelStatistics_GCKD$risk_model, DatasetLevelStatistics_GCKD$k, sep = "_")
ggplot() +
  geom_point(data = DatasetLevelStatistics_GCKD, aes(privacy_marketer, Granularity, colour = factor(id)), shape = 16, size = 5) +
  geom_point(data = DatasetLevelStatistics_GCKD, aes(privacy_marketer, Discernibility, colour = factor(id)), shape = 17, size = 5) +
  geom_point(data = DatasetLevelStatistics_GCKD, aes(privacy_marketer, Entropy, colour = factor(id)), shape = 15, size = 5) +
  scale_color_manual(values = c("GENERIC_kanonymity_11" = "indianred4", "GENERIC_strictaverage_11" = "gold", "SPECIFIC_kanonymity_11" = "darkseagreen3", "SPECIFIC_strictaverage_11" = "lightblue4")) +
  scale_y_continuous(limits = c(0, 100)) + 
  scale_x_continuous(limits = c(0, 100))
ggplot() +
  geom_point(data = DatasetLevelStatistics_GCKD, aes(privacy_marketer, all_avg, colour = factor(id)), shape = 16, size = 2) +
  scale_color_manual(values = c("GENERIC_kanonymity_11" = "indianred4", "GENERIC_strictaverage_11" = "gold", "SPECIFIC_kanonymity_11" = "darkseagreen3", "SPECIFIC_strictaverage_11" = "lightblue4")) +
  scale_y_continuous(limits = c(0, 100)) + 
  scale_x_continuous(limits = c(0, 100))
ggplot() +
  geom_point(data = DatasetLevelStatistics_GCKD, aes(privacy_marketer, tbl1_avg, colour = factor(id)), shape = 16, size = 2) +
  scale_color_manual(values = c("GENERIC_kanonymity_11" = "indianred4", "GENERIC_strictaverage_11" = "gold", "SPECIFIC_kanonymity_11" = "darkseagreen3", "SPECIFIC_strictaverage_11" = "lightblue4")) +
  scale_y_continuous(limits = c(0, 100)) + 
  scale_x_continuous(limits = c(0, 100))
ggplot() +
  geom_point(data = DatasetLevelStatistics_GCKD, aes(privacy_prosecuter, all_avg, colour = factor(id)), shape = 16, size = 2) +
  scale_color_manual(values = c("GENERIC_kanonymity_11" = "indianred4", "GENERIC_strictaverage_11" = "gold", "SPECIFIC_kanonymity_11" = "darkseagreen3", "SPECIFIC_strictaverage_11" = "lightblue4")) +
  scale_y_continuous(limits = c(0, 100)) + 
  scale_x_continuous(limits = c(0, 100))



