#=======Clear environment=======================================================
cat("\f")
rm(list=ls())
dev.off()
#=======Libraries===============================================================
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



print(c(1:(118*2)*0))

#=======Directory, functions file===============================================
# Set the directory path # Paste your path
setwd("C:/Projects/Event-Segmentation")
# Functions file:
source("Functions_.R")

#=======Data====================================================================
#AllData <- read.csv("C:/Projects/Event-Segmentation/TemporalMemory_Results_All_kopia_bez3.csv")
AllData <- read.csv("C:/Projects/Event-Segmentation/TemporalMemory_Results_All.csv")

#=======Variables===============================================================
MyVariable <- "DurationRESP" #variable to test
GrVariable <- "Condition" #grouping variable

ExpConditions <- c(unique(AllData$Condition)) #List of conditions
ExpConditions
Subject <- "Subject"

#=======Data preparation========================================================
MyAllData <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(MyAllData) <- c("Subject", "ratio", GrVariable)

for (i in 1:length(ExpConditions)) {
  tmp <- calculate_accuracy_ratio(AllData, MyVariable, ExpConditions[i])
  MyAllData <- rbind(MyAllData, tmp)
}
is.factor(MyAllData$Condition)

#=======Plots titels and labels=================================================
#General
MyVariableLabel <- "Temporal distance"   #Label for the variable to test
gr_title <- "Temporal distance memory performance"  #Histogram titel
titel_outliers_rm <- "\nOutliers removed"   #Information about removed outliers

#Histogram
hist_title <- gr_title                              # Histogram titel
bw <- 0.05                                          # Histogram bin width
x_label <- "Performance -\n temporal distance"      # Label for x-axis
y_limit1 <- c(0, 5)                                 # y limit
#QQ plot
qqtitel <- "Quantile-Quantile plot"                 # QQ plot titel

#Boxplot
box_ylabel <- MyVariableLabel  #label for the boxplot y-axis
box_titel <- "My boxplot titel"
box_titel < paste(gr_title, " ", box_titel, sep="")



#=======Lilliefors normality test===============================================

for (i in 1:length(ExpConditions)) {
  tmp <- subset.data.frame(MyAllData, Condition == ExpConditions[i])
  print(paste("Lilliefors normality test: ", ExpConditions[i], sep=""))
  print(lillie.test(tmp$ratio)[1:2])
  rm(tmp)
}

#=======Descriptive statistics==================================================
describe.by(MyAllData, group = GrVariable)

#=======Histogram===============================================================

#Before removing outliers
#outliers_check <- c(check_name_outliers(MyAllData))

hist1 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[1]),
                     bw, ExpConditions[1], x_label, y_limit1)
hist2 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[2]),
                     bw, ExpConditions[2], x_label, y_limit1)
#
HistogramsA <- grid.arrange(hist1, hist2, ncol = 2,
                           top = textGrob(hist_title))

#rm(bw, hist_title, x_label, y_limit, hist1, hist2, Histograms)

#=======ggplot####==============================================================
ggqqplot(MyAllData, "ratio", facet.by = GrVariable,
         title=qqtitel)

#=======Box plots===============================================================
boxplot1B <- draw_my_boxplot2(MyAllData,
                 "Distance \n temporal distance",
                 "Temporal distance ratings Distance by condition")
boxplot1B

#=======Outliers================================================================
outliers_check <- c(check_name_outliers(MyAllData))
outliers_check
outliers_list <- outliers_check[3]

MyAllData <- remove_outliers(MyAllData, outliers_list)

is.factor(MyAllData$Condition)

#=======Homoscedasticity - Levene Test==========================================
leveneTest(MyAllData$ratio, MyAllData$Condition)

#=======Histogram - After removing outliers=====================================
gr_title <- paste(gr_title, "\n", titel_outliers_rm, sep="")
hist1 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[1]),
                     bw, ExpConditions[1], x_label, y_limit1)
hist2 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[2]),
                     bw, ExpConditions[2], x_label, y_limit1)
HistogramsB <- grid.arrange(hist1, hist2, ncol = 2,
                           top = textGrob(hist_title))

#=======qqplot - After removing outliers========================================
qqtitel <- paste(qqtitel, titel_outliers_rm, sep="")
ggqqplot(MyAllData, "ratio", facet.by = GrVariable,
         title=qqtitel)

#=======Box plots - After removing outliers=====================================
boxplot1B <- draw_my_boxplot2(MyAllData,
                              "Distance \n temporal distance",
                              "Temporal distance ratings Distance by condition")
boxplot1B


box_titel <- paste(box_titel, titel_outliers_rm, sep="")
boxplot2B <- draw_my_boxplot2(MyAllData,
                 "Distance \n temporal distance",
                 "Temporal distance ratings Distance by condition")

boxplot2B

rm(bw, hist_title, x_label, y_limit1, y_limit2, hist1, hist2, boxplot1B, boxplot2B, HistogramsA, HistogramsB)

#=======Descriptive statistics - After removing outliers========================
describe.by(MyAllData, group = GrVariable)

#=======Paired t-test - After removing outliers=================================
t.test(subset(MyAllData, MyAllData$Condition == "SameContext")$ratio,
       subset(MyAllData, MyAllData$Condition == "Boundary")$ratio, 
       paired = T)
cohen_stats <- cohen.d(MyAllData$ratio, MyAllData$Condition)
cohen_stats


#=======t-test Power analysis - After removing outliers=========================
pwr.t.test(n = NULL, d = cohen_stats$cohen.d[2], sig.level = 0.05, power = 0.8,
           type = c("paired"),
           alternative = c("two.sided"))


#=======ANOVA - After removing outliers=========================================
anova_results <- anova_test(data = MyAllData,
                     dv = ratio,
                     wid = Subject,
                     within = Condition)
anova_results
# anova_table <- get_anova_table(anova2)
# anova_table

#ANOVA power analysis
pwr.anova.test(k = 2, n = NULL, f = anova_results[1, 4], sig.level = 0.05, power = 0.8)


