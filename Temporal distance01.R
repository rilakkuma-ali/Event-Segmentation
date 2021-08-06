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

#Distance_SameContext <- sum(Data_SameContext$DurationRESP)/nrow(Data_SameContext)
#Distance_Boundary <- sum(Data_Boundary$DurationRESP)/nrow(Data_Boundary)

##### Lilliefors normality test #####
# Preparing data for Lilliefors test
# Condition SameContext, variable recency accuracy
Subject_Distance_SameContext <-
  Data_SameContext %>%
  group_by(Subject) %>%
  summarise_at(vars(DurationRESP),
               list(Distance = mean))
Subject_Distance_SameContext$Condition <- "SameContext"
Subject_Distance_SameContext$Condition <- as.factor(Subject_Distance_SameContext$Condition)

# Condition Boundary, variable recency accuracy
Subject_Distance_Boundary <-
  Data_Boundary %>%
  group_by(Subject) %>%
  summarise_at(vars(DurationRESP),
               list(Distance = mean))
Subject_Distance_Boundary$Condition <- "Boundary"
Subject_Distance_Boundary$Condition <- as.factor(Subject_Distance_Boundary$Condition)

Subject_Distance <- rbind(Subject_Distance_SameContext, Subject_Distance_Boundary)
is.factor(Subject_Distance$Condition)
#Lilliefors normality test #
lillie_Distance_SameContext <- lillie.test(Subject_Distance_SameContext$Distance)
lillie_Distance_Boundary <- lillie.test(Subject_Distance_Boundary$Distance)
##### qq plot #####
# ggqqplot(Subject_Distance_SameContext, Subject_Distance_SameContext$Distance)
# ggqqplot(Subject_Distance_Boundary, Subject_Distance_Boundary$Distance)
##### Histograms #####


# Bar colors
if (lillie_Distance_SameContext$p.value > 0.05) {
  col1 <- "green"
  } else {
  col1 <- "red"
  }
if (lillie_Distance_Boundary$p.value > 0.05) {
  col2 <- "green"
  } else {
  col2 <- "red"
  }
# Histogram bin width
bw <- 0.05
# Histograms:
hist1 <- Subject_Distance_SameContext %>%
  ggplot(aes(x=Distance), stat="count") +
  geom_histogram(binwidth=bw, color="black", fill=col1)

hist2 <- Subject_Distance_Boundary %>%
  ggplot(aes(x=Distance), stat="count") +
  geom_histogram(binwidth=bw, color="black", fill=col2)



##### Box plots #####
# Subject_Distance %>%
#   group_by(Condition)
head(Subject_Distance)
bxp <- ggplot(Subject_Distance, 
              aes(x = Condition, y = Distance)) +
                geom_boxplot(notch = F)
bxp
##### Outliers #####
Subject_Distance %>%
  group_by(Condition) %>%
  identify_outliers(Distance)
##### Homoscedasticity - Levene Test#####
Levene_test <- leveneTest(Subject_Distance$Distance, Subject_Distance$Condition)

##### Paired t-tests #####
Subject_Distance <- rbind(Subject_Distance_SameContext, Subject_Distance_Boundary)

Condition_Distance_paired_ttest <- t.test(Subject_Distance_SameContext$Distance, Subject_Distance_Boundary$Distance, paired = T)
Condition_Distance_CohenD <- cohen.d(Subject_Distance_SameContext$Distance, Subject_Distance_Boundary$Distance)
##### Two-sample t-Test #####

Condition_Distance_ttest <- t.test(Subject_Distance$Distance ~ Subject_Distance$Condition, var.equal = TRUE)

##### ANOVA ####

# is.factor(Subject_Distance$Condition)

# Subject_Distance$Condition2 <- factor(Subject_Distance$Condition, 
#                                               levels = c(1,2), labels = c("one", "two"))
# levels(Subject_Distance$Condition2)
# #Subject_Distance$ConditionFactor <- factor(Subject_Distance$Condition)
# is.factor(Subject_Distance$Condition2)
# 
#   # factor(Subject_Distance$Condition, levels = c(1, 2), +
#   #          labels = c("SameContext", "Boundary"))

# ANOVA
anova1 <- aov(Distance ~ Condition, data = Subject_Distance)
anova1_summary <- summary(anova1)

# anova_formula <- y ~ w1*w2 + Error(id/(w1*w2))
# anova2 <- anova_test(data = Subject_Distance, 
#                      formula = anova_formula, 
#                      dv = Subject_Distance$Distance, 
#                      wid = Subject_Distance$Subject, 
#                      within = Subject_Distance$Condition)
# 
# anova2
##### Performing statistical tests #####

lillie_Distance_SameContext 
lillie_Distance_Boundary 
Levene_test
Histograms <- grid.arrange(hist1, hist2, ncol = 2)  
Condition_Distance_paired_ttest
Condition_Distance_CohenD
Condition_Distance_ttest
anova1
anova1_summary
