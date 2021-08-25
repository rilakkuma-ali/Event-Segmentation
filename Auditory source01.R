##### Libraries #####
rm(list=ls())
dev.off()

library(dplyr)
library(nortest)
library(effsize)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(gridExtra) 
library(car)
library(rstatix)
library(ggpubr)


##### Data #####
# Set the directory path # Paste your path
setwd("C:/Projects/Event-Segmentation")
MyData <- read.csv("C:/Projects/Event-Segmentation/SourceMemory_Results_All_kopia_bez3.csv")



Data_SameContext <- subset.data.frame(MyData, Condition == "SameContext")
Data_Boundary <- subset.data.frame(MyData, Condition == "Boundary")
Data_First <- subset.data.frame(MyData, Condition == "First")
Data_Last <- subset.data.frame(MyData, Condition == "Last")

##### Lilliefors normality test #####
# Preparing data for Lilliefors test
# Condition SameContext, variable
Subject_SourceAcc_SameContext <-
  Data_SameContext %>%
  group_by(Subject) %>%
  summarise_at(vars(SourceAcc),
               list(SourceAcc = mean))
Subject_SourceAcc_SameContext$Condition <- "SameContext"
Subject_SourceAcc_SameContext$Condition <- as.factor(Subject_SourceAcc_SameContext$Condition)

# Condition Boundary, variable 
Subject_SourceAcc_Boundary <-
  Data_Boundary %>%
  group_by(Subject) %>%
  summarise_at(vars(SourceAcc),
               list(SourceAcc = mean))
Subject_SourceAcc_Boundary$Condition <- "Boundary"
Subject_SourceAcc_Boundary$Condition <- as.factor(Subject_SourceAcc_Boundary$Condition)

# Condition Boundary, variable 
Subject_SourceAcc_First <-
  Data_First %>%
  group_by(Subject) %>%
  summarise_at(vars(SourceAcc),
               list(SourceAcc = mean))
Subject_SourceAcc_First$Condition <- "First"
Subject_SourceAcc_First$Condition <- as.factor(Subject_SourceAcc_First$Condition)

# Condition Boundary, variable 
Subject_SourceAcc_Last <-
  Data_Last %>%
  group_by(Subject) %>%
  summarise_at(vars(SourceAcc),
               list(SourceAcc = mean))
Subject_SourceAcc_Last$Condition <- "Last"
Subject_SourceAcc_Last$Condition <- as.factor(Subject_SourceAcc_Last$Condition)

Subject_SourceAcc <- rbind(Subject_SourceAcc_SameContext, Subject_SourceAcc_Boundary, Subject_SourceAcc_First, Subject_SourceAcc_Last)
is.factor(Subject_SourceAcc$Condition)

#Lilliefors normality test #
lillie_SourceAcc_SameContext <- lillie.test(Subject_SourceAcc_SameContext$SourceAcc)
lillie_SourceAcc_Boundary <- lillie.test(Subject_SourceAcc_Boundary$SourceAcc)
##### qq plot #####
# ggqqplot(Subject_SourceAcc_SameContext, Subject_SourceAcc_SameContext$SourceAcc)
# ggqqplot(Subject_SourceAcc_Boundary, Subject_SourceAcc_Boundary$SourceAcc)
##### Histograms #####


# Bar colors
# if (lillie_SourceAcc_SameContext$p.value > 0.05) {
#   col1 <- "green"
#   } else {
#   col1 <- "red"
#   }
# if (lillie_SourceAcc_Boundary$p.value > 0.05) {
#   col2 <- "green"
#   } else {
#   col2 <- "red"
#   }
# # Histogram bin width
bw <- 0.05
# Histograms:
hist1 <- Subject_SourceAcc_SameContext %>%
  ggplot(aes(x=SourceAcc), stat="count") +
  geom_histogram(binwidth=bw, color="black")

hist2 <- Subject_SourceAcc_Boundary %>%
  ggplot(aes(x=SourceAcc), stat="count") +
  geom_histogram(binwidth=bw, color="black")

hist3 <- Subject_SourceAcc_First %>%
  ggplot(aes(x=SourceAcc), stat="count") +
  geom_histogram(binwidth=bw, color="black")

hist4 <- Subject_SourceAcc_Last %>%
  ggplot(aes(x=SourceAcc), stat="count") +
  geom_histogram(binwidth=bw, color="black")

##### Box plots #####
# Subject_SourceAcc %>%
#   group_by(Condition)
head(Subject_SourceAcc)
bxp <- ggplot(Subject_SourceAcc, 
              aes(x = Condition, y = SourceAcc)) +
                geom_boxplot(notch = F)
bxp
##### Outliers #####
Subject_SourceAcc %>%
  group_by(Condition) %>%
  identify_outliers(SourceAcc)



##### Homoscedasticity - Levene Test#####
Levene_test <- leveneTest(Subject_SourceAcc$SourceAcc, Subject_SourceAcc$Condition)

##### Paired t-tests #####
Subject_SourceAcc <- rbind(Subject_SourceAcc_SameContext, Subject_SourceAcc_Boundary)

Condition_SourceAcc_paired_ttest <- t.test(Subject_SourceAcc_SameContext$SourceAcc, Subject_SourceAcc_Boundary$SourceAcc, paired = T)
Condition_SourceAcc_CohenD <- cohen.d(Subject_SourceAcc_SameContext$SourceAcc, Subject_SourceAcc_Boundary$SourceAcc)
##### Two-sample t-Test #####

Condition_SourceAcc_ttest <- t.test(Subject_SourceAcc$SourceAcc ~ Subject_SourceAcc$Condition, var.equal = TRUE)

##### ANOVA ####

# is.factor(Subject_SourceAcc$Condition)

# Subject_SourceAcc$Condition2 <- factor(Subject_SourceAcc$Condition, 
#                                               levels = c(1,2), labels = c("one", "two"))
# levels(Subject_SourceAcc$Condition2)
# #Subject_SourceAcc$ConditionFactor <- factor(Subject_SourceAcc$Condition)
# is.factor(Subject_SourceAcc$Condition2)
# 
#   # factor(Subject_SourceAcc$Condition, levels = c(1, 2), +
#   #          labels = c("SameContext", "Boundary"))

# ANOVA
anova1 <- aov(SourceAcc ~ Condition, data = Subject_SourceAcc)
anova1_summary <- summary(anova1)

# anova_formula <- y ~ w1*w2 + Error(id/(w1*w2))
# anova2 <- anova_test(data = Subject_SourceAcc, 
#                      formula = anova_formula, 
#                      dv = Subject_SourceAcc$SourceAcc, 
#                      wid = Subject_SourceAcc$Subject, 
#                      within = Subject_SourceAcc$Condition)
# 
# anova2
##### Performing statistical tests #####

lillie_SourceAcc_SameContext 
lillie_SourceAcc_Boundary 
Levene_test
Histograms <- grid.arrange(hist1, hist2, ncol = 2)  
Condition_SourceAcc_paired_ttest
Condition_SourceAcc_CohenD
Condition_SourceAcc_ttest
anova1
anova1_summary
