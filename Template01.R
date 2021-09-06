##### Libraries #####
cat("\f")
rm(list=ls())
dev.off()

library(dplyr) #yes
#library(nortest)
library(effsize) #yes
#library(ggplot2) #no
library(tidyverse) #yes
library(hrbrthemes) #yes
library(car) #yes
library(rstatix) #yes
library(ggpubr) #yes
#library(grid) #no
library(gridExtra) #yes
library(psych) #yes
library(pwr) #yes



print(c(1:(118*2)*0))##### Data #####
####Settings####
# Set the directory path # Paste your path
setwd("C:/Projects/Event-Segmentation")
# Functions file:
source("Functions_.R")

#AllData <- read.csv("C:/Projects/Event-Segmentation/TemporalMemory_Results_All_kopia_bez3.csv")
AllData <- read.csv("C:/Projects/Event-Segmentation/TemporalMemory_Results_All.csv")

# MyVariable - the variable for analysis
MyVariable <- "DurationRESP"
MyVariableLabel <- "Temporal distance"
GrVariable <- "Condition"

ExpConditions <- c(unique(AllData$Condition)) #List of conditions
ExpConditions

MyAllData <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(MyAllData) <- c("Subject", "ratio", GrVariable)

for (i in 1:length(ExpConditions)) {
  tmp <- calculate_accuracy_ratio(AllData, MyVariable, ExpConditions[i])
  MyAllData <- rbind(MyAllData, tmp)
}
is.factor(MyAllData$Condition)
##### Lilliefors normality test #####

for (i in 1:length(ExpConditions)) {
  tmp <- subset.data.frame(MyAllData, Condition == ExpConditions[i])
  print(paste("Lilliefors normality test: ", ExpConditions[i], sep=""))
  print(lillie.test(tmp$ratio)[1:2])
  rm(tmp)
}


##### Histograms #####==========================================================
#General:
# Histogram bin width
bw <- 0.05
# Graph title:
gr_title <- "Temporal distance memory performance"
# Label for x-axis:
x_label <- "Performance -\n temporal distance"
# y limit
y_limit1 <- c(0, 5)
y_limit2 <- y_limit1
#
#### Before removing outliers
outliers_check <- c(check_name_outliers(MyAllData))
# if (outliers_check[1] == T) {
  hist1 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[1]),
                       bw, ExpConditions[1], x_label, y_limit1)
  hist2 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[2]),
                       bw, ExpConditions[2], x_label, y_limit1)
  #
  HistogramsA <- grid.arrange(hist1, hist2, ncol = 2,
                             top = textGrob(gr_title))
  
  #rm(bw, gr_title, x_label, y_limit, hist1, hist2, Histograms)
  
  ####ggplot####===================================================================
  
  ggqqplot(MyAllData, "ratio", facet.by = GrVariable)
  
  ##### Box plots #####===========================================================
  #Classic boxplot
  # boxplot1A <- draw_my_boxplot(MyAllData,
  #                 "Distance \n temporal distance",
  #                 "Temporal distance ratings Distance by condition")
  
  #Boxplot + violinplot + median + outliers labels
  boxplot1B <- draw_my_boxplot2(MyAllData,
                   "Distance \n temporal distance",
                   "Temporal distance ratings Distance by condition")
# } else {
#   break
#}

  # boxplot1A
  boxplot1B


##### Outliers #####============================================================
outliers_check <- c(check_name_outliers(MyAllData))
outliers_check
outliers_list <- outliers_check[3]

# for(i in 1:length(outliers_list)) {
#   MyAllData <- subset(MyAllData, Subject != (outliers_list[i]))}

MyAllData <- remove_outliers(MyAllData, outliers_list)

is.factor(MyAllData$Condition)

#### Homoscedasticity - Levene Test#####
leveneTest(MyAllData$ratio, MyAllData$Condition)

#After removing outliers========================================================

hist1 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[1]),
                     bw, ExpConditions[1], x_label, y_limit2)
hist2 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[2]),
                     bw, ExpConditions[2], x_label, y_limit2)
#
HistogramsB <- grid.arrange(hist1, hist2, ncol = 2,
                           top = textGrob(gr_title))

# qqplot
ggqqplot(MyAllData, "ratio", facet.by = GrVariable)

#Classic boxplot
boxplot2A <- draw_my_boxplot(MyAllData,
                "Distance \n temporal distance",
                "Temporal distance ratings Distance by condition")

#Boxplot + violinplot + median + outliers labels
boxplot2B <- draw_my_boxplot2(MyAllData,
                 "Distance \n temporal distance",
                 "Temporal distance ratings Distance by condition")

boxplot2A
boxplot2B
# #Histogram - General:=======================================================================
# # Histogram bin width
# bw <- 0.05
# # Graph title:
# gr_title <- "Temporal distance memory performance"
# # Label for x-axis:
# x_label <- "Performance -\n temporal distance"
# # y limit
# y_limit <- c(0, 5)
# #After removing outliers========================================================
# outliers_check <- c(check_name_outliers(MyAllData))
# if (outliers_check[1] == T) {
#   #===============================================================================
#   hist1 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[1]), 
#                        bw, ExpConditions[1], x_label, y_limit)
#   hist2 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[2]), 
#                        bw, ExpConditions[2], x_label, y_limit)
#   Histograms <- grid.arrange(hist1, hist2, ncol = 2,
#                              top = textGrob(gr_title))  
#   
#   #qqplot
#   ggqqplot(MyAllData, "ratio", facet.by = GrVariable)
#   #BOXPLOTS
#   
#   #Classic boxplot
#   draw_my_boxplot(MyAllData,
#                   "Distance \n temporal distance",
#                   "Temporal distance ratings Distance by condition")
# 
#   #Boxplot + violinplot + median + outliers labels
#   draw_my_boxplot2(MyAllData,
#                    "Distance \n temporal distance",
#                    "Temporal distance ratings Distance by condition")
# 
#   ##### Homoscedasticity - Levene Test
#   
#   leveneTest(MyAllData$ratio, MyAllData$Condition)
#   
#   # Outliers 
#   outliers_check <- c(check_name_outliers(MyAllData))
#   outliers_check
#   outliers_list <- outliers_check[3]
#   
#   
#   for(i in 1:length(outliers_list)) {
#     MyAllData <- subset(MyAllData, Subject != (outliers_list[i]))}  
# } else {
#   #===============================================================================
#   hist1 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[1]), 
#                        bw, ExpConditions[1], x_label, y_limit)
#   hist2 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[2]), 
#                        bw, ExpConditions[2], x_label, y_limit)
#   Histograms <- grid.arrange(hist1, hist2, ncol = 2,
#                              top = textGrob(gr_title))  
#   
#   #qqplot
#   ggqqplot(MyAllData, "ratio", facet.by = GrVariable)
#   
#   #BOXPLOTS
#   
#   #Classic boxplot
#   draw_my_boxplot(MyAllData,
#                   "Distance \n temporal distance",
#                   "Temporal distance ratings Distance by condition")
#   
#   #Boxplot + violinplot + median + outliers labels
#   draw_my_boxplot2(MyAllData,
#                    "Distance \n temporal distance",
#                    "Temporal distance ratings Distance by condition")
#   
#   ##### Homoscedasticity - Levene Test
#   
#   leveneTest(MyAllData$ratio, MyAllData$Condition)
# }

rm(bw, gr_title, x_label, y_limit1, y_limit2, hist1, hist2, Histograms)

# ####Descriptive statistics####
# describe.by(MyAllData, group = GrVariable)

##### Paired t-tests #####
#MyAllData <- rbind(MyAllData_SameContext, MyAllData_Boundary)

t.test(subset(MyAllData, MyAllData$Condition == "SameContext")$ratio,
       subset(MyAllData, MyAllData$Condition == "Boundary")$ratio, 
       paired = T)
cohen_stats <- cohen.d(MyAllData$ratio, MyAllData$Condition)
cohen_stats


# Power analysis
pwr.t.test(n = NULL, d = cohen_stats$cohen.d[2], sig.level = 0.05, power = 0.8,
           type = c("paired"),
           alternative = c("two.sided"))

##### Two-sample t-Test #####

#t.test(MyAllData$ratio ~ MyAllData$Condition, var.equal = TRUE)
#t.test(MyAllData$ratio ~ MyAllData$Condition, var.equal = TRUE)

##### ANOVA ####

# MyAllData$Condition2 <- factor(MyAllData$Condition, 
#                                               levels = c(1,2), labels = c("one", "two"))
# levels(MyAllData$Condition2)
# #MyAllData$ConditionFactor <- factor(MyAllData$Condition)
# is.factor(MyAllData$Condition2)
# 
#   # factor(MyAllData$Condition, levels = c(1, 2), +
#   #          labels = c("SameContext", "Boundary"))

# ANOVA
# https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/#data-preparation

#anova_formula <- y ~ w1*w2 + Error(id/(w1*w2))
anova2 <- anova_test(data = MyAllData,
#                     formula = anova_formula,
                     dv = ratio,
                     wid = Subject,
                     within = Condition)

anova2

get_anova_table(anova2)

# ANOVA power analysis
#pwr.anova.test(k = 2, n = NULL, f = NULL, sig.level = 0.05, power = NULL)


##### Performing statistical tests #####

describe.by(MyAllData, group = GrVariable)
