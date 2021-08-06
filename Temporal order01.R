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
MyData <- read.csv("C:/Projects/Event-Segmentation/TemporalMemory_Results_All_kopia_bez3.csv")
#MyData <- read.csv("C:/Projects/Event-Segmentation/TemporalMemory_Results_All.csv")


Data_SameContext <- subset.data.frame(MyData, Condition == "SameContext")
Data_Boundary <- subset.data.frame(MyData, Condition == "Boundary")

#Performance_SameContext <- sum(Data_SameContext$RecencyAcc)/nrow(Data_SameContext)
#Performance_Boundary <- sum(Data_Boundary$RecencyAcc)/nrow(Data_Boundary)

##### Lilliefors normality test #####
# Preparing data for Lilliefors test
# Condition SameContext, variable recency accuracy
Subject_Performance_SameContext <-
  Data_SameContext %>%
  group_by(Subject) %>%
  summarise_at(vars(RecencyAcc),
               list(Performance = mean))
Subject_Performance_SameContext$Condition <- "SameContext"
Subject_Performance_SameContext$Condition <- as.factor(Subject_Performance_SameContext$Condition)

# Condition Boundary, variable recency accuracy
Subject_Performance_Boundary <-
  Data_Boundary %>%
  group_by(Subject) %>%
  summarise_at(vars(RecencyAcc),
               list(Performance = mean))
Subject_Performance_Boundary$Condition <- "Boundary"
Subject_Performance_Boundary$Condition <- as.factor(Subject_Performance_Boundary$Condition)

Subject_Performance <- rbind(Subject_Performance_SameContext, Subject_Performance_Boundary)
is.factor(Subject_Performance$Condition)
#Lilliefors normality test #
lillie_Performance_SameContext <- lillie.test(Subject_Performance_SameContext$Performance)
lillie_Performance_Boundary <- lillie.test(Subject_Performance_Boundary$Performance)
##### qq plot #####
# ggqqplot(Subject_Performance_SameContext, Subject_Performance_SameContext$Performance)
# ggqqplot(Subject_Performance_Boundary, Subject_Performance_Boundary$Performance)
##### Histograms #####


# Bar colors
if (lillie_Performance_SameContext$p.value > 0.05) {
  col1 <- "green"
  } else {
  col1 <- "red"
  }
if (lillie_Performance_Boundary$p.value > 0.05) {
  col2 <- "green"
  } else {
  col2 <- "red"
  }
# Histogram bin width
bw <- 0.05
# Histograms:
hist1 <- Subject_Performance_SameContext %>%
  ggplot(aes(x=Performance), stat="count") +
  geom_histogram(binwidth=bw, color="black", fill=col1)

hist2 <- Subject_Performance_Boundary %>%
  ggplot(aes(x=Performance), stat="count") +
  geom_histogram(binwidth=bw, color="black", fill=col2)



##### Box plots #####
# Subject_Performance %>%
#   group_by(Condition)
head(Subject_Performance)
bxp <- ggplot(Subject_Performance, 
              aes(x = Condition, y = Performance)) +
                geom_boxplot(notch = T)
bxp
##### Outliers #####
Subject_Performance %>%
  group_by(Condition) %>%
  identify_outliers(Performance)
##### Homoscedasticity - Levene Test#####
Levene_test <- leveneTest(Subject_Performance$Performance, Subject_Performance$Condition)

##### Paired t-tests #####
Subject_Performance <- rbind(Subject_Performance_SameContext, Subject_Performance_Boundary)

Condition_Performance_paired_ttest <- t.test(Subject_Performance_SameContext$Performance, Subject_Performance_Boundary$Performance, paired = T)
Condition_Performance_CohenD <- cohen.d(Subject_Performance_SameContext$Performance, Subject_Performance_Boundary$Performance)
##### Two-sample t-Test #####

Condition_Performance_ttest <- t.test(Subject_Performance$Performance ~ Subject_Performance$Condition, var.equal = TRUE)

##### ANOVA ####

# is.factor(Subject_Performance$Condition)

# Subject_Performance$Condition2 <- factor(Subject_Performance$Condition, 
#                                               levels = c(1,2), labels = c("one", "two"))
# levels(Subject_Performance$Condition2)
# #Subject_Performance$ConditionFactor <- factor(Subject_Performance$Condition)
# is.factor(Subject_Performance$Condition2)
# 
#   # factor(Subject_Performance$Condition, levels = c(1, 2), +
#   #          labels = c("SameContext", "Boundary"))

# ANOVA
anova1 <- aov(Performance ~ Condition, data = Subject_Performance)
anova1_summary <- summary(anova1)

# anova_formula <- y ~ w1*w2 + Error(id/(w1*w2))
# anova2 <- anova_test(data = Subject_Performance, 
#                      formula = anova_formula, 
#                      dv = Subject_Performance$Performance, 
#                      wid = Subject_Performance$Subject, 
#                      within = Subject_Performance$Condition)
# 
# anova2
##### Performing statistical tests #####

lillie_Performance_SameContext 
lillie_Performance_Boundary 
Levene_test
Histograms <- grid.arrange(hist1, hist2, ncol = 2)  
Condition_Performance_paired_ttest
Condition_Performance_CohenD
Condition_Performance_ttest
anova1
anova1_summary
