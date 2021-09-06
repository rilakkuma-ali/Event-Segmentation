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
library(pwr)

##### Data #####
# Set the directory path # Paste your path
setwd("C:/Projects/Event-Segmentation")
# Functions file:
source("Functions_.R")

#MyData <- read.csv("C:/Projects/Event-Segmentation/TemporalMemory_Results_All_kopia_bez3.csv")
MyData <- read.csv("C:/Projects/Event-Segmentation/TemporalMemory_Results_All.csv")


Data_SameContext <- subset.data.frame(MyData, Condition == "SameContext")
Data_Boundary <- subset.data.frame(MyData, Condition == "Boundary")

#Distance_SameContext <- sum(Data_SameContext$DurationRESP)/nrow(Data_SameContext)
#Distance_Boundary <- sum(Data_Boundary$DurationRESP)/nrow(Data_Boundary)

##### Lilliefors normality test #####
# Preparing data for Lilliefors test
Subject_Distance_SameContext <- calculate_accuracy_ratio(MyData, "DurationRESP", "SameContext")
Subject_Distance_Boundary <- calculate_accuracy_ratio(MyData, "DurationRESP", "Boundary")

Subject_Distance <- rbind(Subject_Distance_SameContext, Subject_Distance_Boundary)
is.factor(Subject_Distance$Condition)
#Lilliefors normality test #
lillie.test(Subject_Distance_SameContext$ratio)
lillie.test(Subject_Distance_Boundary$ratio)

##### Histograms #####==========================================================
#===============================================================================
#General:=======================================================================
# Histogram bin width
bw <- 0.05
# Graph title:
gr_title <- "Temporal distance memory performance"
# Label for x-axis:
x_label <- "Performance -\n temporal distance"
# y limit
y_limit <- c(0, 5)
#===============================================================================
hist1 <- draw_a_hist(subset(Subject_Distance, Subject_Distance$Condition == "SameContext"), bw, "SameContext", x_label, y_limit)
hist2 <- draw_a_hist(subset(Subject_Distance, Subject_Distance$Condition == "Boundary"), bw, "Boundary", x_label, y_limit)
#===============================================================================
Histograms <- grid.arrange(hist1, hist2, ncol = 2,
                           top = textGrob(gr_title))  

# rm(bw, gr_title, x_label, y_limit, hist1, hist2, Histograms)
#===============================================================================
####ggplot####

ggqqplot(Subject_Distance, "ratio", facet.by = "Condition")


##### Box plots #####===========================================================
#===============================================================================
#Classic boxplot
draw_my_boxplot(Subject_Distance, 
                "Distance \n temporal distance", 
                "Temporal distance ratings Distance by condition")

#Boxplot + violinplot + median + outliers labels
draw_my_boxplot2(Subject_Distance, 
                "Distance \n temporal distance", 
                "Temporal distance ratings Distance by condition")

#===============================================================================

##### Outliers #####============================================================
outliers_check <- c(check_name_outliers(Subject_Distance))
outliers_check
outliers_list <- outliers_check[3]


for(i in 1:length(outliers_list)) {
  Subject_Distance <- subset(Subject_Distance, Subject != (outliers_list[i]))}

#remove_outliers(Subject_Distance, outliers_list)
##### Homoscedasticity - Levene Test#####
leveneTest(Subject_Distance$ratio, Subject_Distance$Condition)

#After removing outliers========================================================
if (outliers_check[1] == F) {
  break
} else {
  #===============================================================================
  hist1 <- draw_a_hist(Subject_Distance_SameContext, bw, "SameContext", x_label, y_limit)
  hist2 <- draw_a_hist(Subject_Distance_Boundary, bw, "Boundary", x_label, y_limit)
  #===============================================================================
  Histograms <- grid.arrange(hist1, hist2, ncol = 2,
                             top = textGrob(gr_title)) 
  #=============================================================================
  #Classic boxplot
  draw_my_boxplot(Subject_Distance, 
                  "Distance \n temporal distance", 
                  "Temporal distance ratings Distance by condition")
  
  #Boxplot + violinplot + median + outliers labels
  draw_my_boxplot2(Subject_Distance, 
                   "Distance \n temporal distance", 
                   "Temporal distance ratings Distance by condition")
}



rm(bw, gr_title, x_label, y_limit, hist1, hist2, Histograms)

# ####Descriptive statistics####
# describe.by(Subject_Distance, group = "Condition")

##### Paired t-tests #####
#Subject_Distance <- rbind(Subject_Distance_SameContext, Subject_Distance_Boundary)

t.test(subset(Subject_Distance, Subject_Distance$Condition == "SameContext")$ratio,
       subset(Subject_Distance, Subject_Distance$Condition == "Boundary")$ratio, 
       paired = T)
cohen_stats <- cohen.d(Subject_Distance$ratio, Subject_Distance$Condition)
cohen_stats


# Power analysis
pwr.t.test(n = NULL, d = cohen_stats$cohen.d[2], sig.level = 0.05, power = 0.8,
           type = c("paired"),
           alternative = c("two.sided"))

##### Two-sample t-Test #####

#t.test(Subject_Distance$ratio ~ Subject_Distance$Condition, var.equal = TRUE)
#t.test(Subject_Distance$ratio ~ Subject_Distance$Condition, var.equal = TRUE)

##### ANOVA ####

# Subject_Distance$Condition2 <- factor(Subject_Distance$Condition, 
#                                               levels = c(1,2), labels = c("one", "two"))
# levels(Subject_Distance$Condition2)
# #Subject_Distance$ConditionFactor <- factor(Subject_Distance$Condition)
# is.factor(Subject_Distance$Condition2)
# 
#   # factor(Subject_Distance$Condition, levels = c(1, 2), +
#   #          labels = c("SameContext", "Boundary"))

# ANOVA
# https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/#data-preparation

anova_formula <- y ~ w1*w2 + Error(id/(w1*w2))
anova2 <- anova_test(data = Subject_Distance,
                     formula = anova_formula,
                     dv = ratio,
                     wid = Subject,
                     within = Condition)

anova2

get_anova_table(anova2)

# ANOVA power analysis
#pwr.anova.test(k = 2, n = NULL, f = NULL, sig.level = 0.05, power = NULL)


##### Performing statistical tests #####

describe.by(Subject_Distance_SameContext)
describe.by(Subject_Distance_Boundary)
describe.by(Subject_Distance, group = "Condition")
