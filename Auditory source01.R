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
library(ggstatsplot)


##### Data #####
# Set the directory path # Paste your path
setwd("C:/Projects/Event-Segmentation")
MyData <- read.csv("C:/Projects/Event-Segmentation/SourceMemory_Results_All_kopia_bez3_b.csv", sep = ";")



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
lillie_SourceAcc_First <- lillie.test(Subject_SourceAcc_First$SourceAcc)
lillie_SourceAcc_Last <- lillie.test(Subject_SourceAcc_Last$SourceAcc)

lillie_SourceAcc_SameContext
lillie_SourceAcc_Boundary
lillie_SourceAcc_First
lillie_SourceAcc_Last

# ##### qq plot #####
# ggqqplot(Subject_SourceAcc_SameContext, Subject_SourceAcc_SameContext$SourceAcc)
# ggqqplot(Subject_SourceAcc_Boundary, Subject_SourceAcc_Boundary$SourceAcc)
# ggqqplot(Subject_SourceAcc_First, Subject_SourceAcc_First$SourceAcc)
# ggqqplot(Subject_SourceAcc_Last, Subject_SourceAcc_Last$SourceAcc)

##### Histograms #####


# # Bar colors
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
# if (lillie_SourceAcc_First$p.value > 0.05) {
#   col3 <- "green"
# } else {
#   col3 <- "red"
# }
# if (lillie_SourceAcc_Last$p.value > 0.05) {
#   col4 <- "green"
# } else {
#   col4 <- "red"
# }


# # Histogram bin width
bw <- 0.05
# Histograms:
hist1 <- Subject_SourceAcc_SameContext %>%
  ggplot(aes(x=SourceAcc), stat="count") +
  geom_histogram(binwidth=bw) + 
  labs(title = "Condition: SameContext")

hist2 <- Subject_SourceAcc_Boundary %>%
  ggplot(aes(x=SourceAcc), stat="count") +
  geom_histogram(binwidth=bw) +
  labs(title = "Condition: Boundary")

hist3 <- Subject_SourceAcc_First %>%
  ggplot(aes(x=SourceAcc), stat="count") +
  geom_histogram(binwidth=bw) +
  labs(title = "Condition: First")

hist4 <- Subject_SourceAcc_Last %>%
  ggplot(aes(x=SourceAcc), stat="count") +
  geom_histogram(binwidth=bw) +
  labs(title = "Condition: Last")

Histograms <- grid.arrange(hist1, hist2, hist3, hist4, ncol = 2, nrow = 2) 

##### Box plots #####
# Subject_SourceAcc %>%
#   group_by(Condition)
head(Subject_SourceAcc2)
bxp <- ggplot(Subject_SourceAcc2, 
              aes(x = Condition, y = SourceAcc)) +
                geom_boxplot(notch = F)
bxp



##### Outliers #####
outliers <- Subject_SourceAcc %>%
  group_by(Condition) %>%
  identify_outliers(SourceAcc)
outliers

outliers_subjects <- unique(c(outliers$Subject))
outliers_subjects

# outliers indices found by hand

Subject_SourceAcc2 <- subset(Subject_SourceAcc, Subject_SourceAcc$Subject!= 313 & Subject_SourceAcc$Subject!= 321 & Subject_SourceAcc$Subject!= 319)
Data_SameContext2 <- subset(Data_SameContext, Data_SameContext$Subject!= 313 & Data_SameContext$Subject!= 321 & Data_SameContext$Subject!= 319)
Data_Boundary2 <- subset(Data_Boundary, Data_Boundary$Subject!= 313 & Data_Boundary$Subject!= 321 & Data_Boundary$Subject!= 319)
Data_First2 <- subset(Data_First, Data_First$Subject!= 313 & Data_First$Subject!= 321 & Data_First$Subject!= 319)
Data_Last2 <- subset(Data_Last, Data_Last$Subject!= 313 & Data_Last$Subject!= 321 & Data_Last$Subject!= 319)

##### Homoscedasticity - Levene Test#####
leveneTest(Subject_SourceAcc$SourceAcc, Subject_SourceAcc$Condition)
leveneTest(Subject_SourceAcc2$SourceAcc, Subject_SourceAcc2$Condition)
##### Histograms #####


# Bar colors

if (lillie_SourceAcc_SameContext$p.value > 0.05) {
  col1 <- "green"
} else {
  col1 <- "red"
}
if (lillie_SourceAcc_Boundary$p.value > 0.05) {
  col2 <- "green"
} else {
  col2 <- "red"
}
if (lillie_SourceAcc_First$p.value > 0.05) {
  col3 <- "green"
} else {
  col3 <- "red"
}
if (lillie_SourceAcc_Last$p.value > 0.05) {
  col4 <- "green"
} else {
  col4 <- "red"
}


# # Histogram bin width
bw <- 0.05
# Histograms:
hist1 <- Subject_SourceAcc_SameContext %>%
  ggplot(aes(x=SourceAcc), stat="count") +
  geom_histogram(binwidth=bw, color=col1) + 
  labs(title = "Condition: SameContext")

hist2 <- Subject_SourceAcc_Boundary %>%
  ggplot(aes(x=SourceAcc), stat="count") +
  geom_histogram(binwidth=bw, color=col2) +
  labs(title = "Condition: Boundary")

hist3 <- Subject_SourceAcc_First %>%
  ggplot(aes(x=SourceAcc), stat="count") +
  geom_histogram(binwidth=bw, color=col3) +
  labs(title = "Condition: First")

hist4 <- Subject_SourceAcc_Last %>%
  ggplot(aes(x=SourceAcc), stat="count") +
  geom_histogram(binwidth=bw, color=col4) +
  labs(title = "Condition: Last")

Histograms <- grid.arrange(hist1, hist2, hist3, hist4, ncol = 2, nrow = 2) 

##### Box plots #####
# Subject_SourceAcc %>%
#   group_by(Condition)
head(Subject_SourceAcc)
bxp <- ggplot(Subject_SourceAcc, 
              aes(x = Condition, y = SourceAcc)) +
  geom_boxplot(notch = F)
bxp




##### ANOVA ####

is.factor(Subject_SourceAcc$Condition)
is.factor(Subject_SourceAcc2$Condition)
# Subject_SourceAcc$Condition2 <- factor(Subject_SourceAcc$Condition,
#   
# levels = c(1,2), labels = c("one", "two"))
# levels(Subject_SourceAcc$Condition2)
# Subject_SourceAcc$ConditionFactor <- factor(Subject_SourceAcc$Condition)
# is.factor(Subject_SourceAcc$Condition2)
# 
# factor(Subject_SourceAcc$Condition, levels = c(1, 2),
#           labels = c("SameContext", "Boundary"))

# ANOVA
anova1 <- aov(SourceAcc ~ Condition, data = Subject_SourceAcc)
anova1_summary <- summary(anova1)

anova1_no_outliers <- aov(SourceAcc ~ Condition, data = Subject_SourceAcc2)
anova1_summary_no_outliers <- summary(anova1_no_outliers)

anova1
anova1_no_outliers

anova1_summary
anova1_summary_no_outliers

############
anova_formula <- y ~ w1*w2 + Error(id/(w1*w2))
anova2 <- anova_test(data = Subject_SourceAcc,
                     formula = anova_formula,
                     dv = Subject_SourceAcc$SourceAcc,
                     wid = Subject_SourceAcc$Subject,
                     within = Subject_SourceAcc$Condition)

anova2

anova2_no_outliers <- anova_test(data = Subject_SourceAcc2,
                                 formula = anova_formula,
                                 dv = Subject_SourceAcc2$SourceAcc,
                                 wid = Subject_SourceAcc2$Subject,
                                 within = Subject_SourceAcc2$Condition)
anova2_no_outliers

##### Performing statistical tests #####


