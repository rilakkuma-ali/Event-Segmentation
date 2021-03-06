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
library(grid)
library(gridExtra)
library(psych)

##### Data #####
# Set the directory path # Paste your path
setwd("C:/Projects/Event-Segmentation")
# Functions file:
source("Functions_.R")
#MyData <- read.csv("C:/Projects/Event-Segmentation/TemporalMemory_Results_All_kopia_bez3.csv")
MyData <- read.csv("C:/Projects/Event-Segmentation/TemporalMemory_Results_All.csv")

# Subseting data
Data_SameContext <- subset.data.frame(MyData, Condition == "SameContext")
Data_Boundary <- subset.data.frame(MyData, Condition == "Boundary")

Subject_Performance_SameContext <- calculate_accuracy_ratio(MyData, "RecencyAcc", "SameContext")
Subject_Performance_Boundary <- calculate_accuracy_ratio(MyData, "RecencyAcc", "Boundary")

Subject_Performance <- rbind(Subject_Performance_SameContext, Subject_Performance_Boundary)
is.factor(Subject_Performance$Condition)

##### Lilliefors normality test #####
lillie.test(Subject_Performance_SameContext$ratio)
lillie.test(Subject_Performance_Boundary$ratio)

##### qq plot #####
#ggqqplot(Subject_Performance_SameContext, Subject_Performance_SameContext$ratio)
#ggqqplot(Subject_Performance_Boundary, Subject_Performance_Boundary$ratio)


##### Histograms #####==========================================================
#===============================================================================
#General:=======================================================================
# Histogram bin width
bw <- 0.05
# Graph title:
gr_title <- "Temporal order memory performance"
# Label for x-axis:
x_label <- "Performance -\n temporal order"
# y limit
y_limit <- c(0, 7)
#===============================================================================
hist1 <- draw_a_hist(Subject_Performance_SameContext, bw, "SameContext", x_label, y_limit)
hist2 <- draw_a_hist(Subject_Performance_Boundary, bw, "Boundary", x_label, y_limit)
#===============================================================================
Histograms <- grid.arrange(hist1, hist2, ncol = 2,
                           top = textGrob(gr_title))  

rm(bw, gr_title, x_label, y_limit, hist1, hist2, Histograms)
#===============================================================================
##### Box plots #####


draw_my_boxplot(Subject_Performance, 
                "Performance \n temporal order", 
                "Temporal order ratings \nPerformance by condition")
#Boxplot + violinplot + median + outliers labels
draw_my_boxplot2(Subject_Performance, 
                 "Distance \n temporal distance", 
                 "Temporal distance ratings Distance by condition")
####Descriptive statistics####
# describe.by(Subject_Distance_SameContext)
# describe.by(Subject_Distance_Boundary)
describe.by(Subject_Performance, group = "Condition")
##### Outliers #####
check_remove_name_outliers(Subject_Performance)
##### Homoscedasticity - Levene Test#####
leveneTest(Subject_Performance$ratio, Subject_Performance$Condition)

##### Paired t-tests #####
Subject_Performance <- rbind(Subject_Performance_SameContext, Subject_Performance_Boundary)

t.test(Subject_Performance_SameContext$ratio, Subject_Performance_Boundary$ratio, paired = T)
c<- cohen.d(Subject_Performance$ratio, Subject_Performance$Condition)
##### Two-sample t-Test #####

t.test(Subject_Performance$ratio ~ Subject_Performance$Condition, var.equal = TRUE)

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
anova1 <- aov(ratio ~ Condition, data = Subject_Performance)
anova1_summary <- summary(anova1)

anova1
anova1_summary

# anova_formula <- y ~ w1*w2 + Error(id/(w1*w2))
# anova2 <- anova_test(data = Subject_Performance, 
#                      formula = anova_formula, 
#                      dv = Subject_Performance$ratio, 
#                      wid = Subject_Performance$Subject, 
#                      within = Subject_Performance$Condition)
# 
# anova2
##### Performing statistical tests #####




