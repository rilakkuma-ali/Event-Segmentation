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
AllData <- read.csv("C:/Projects/Event-Segmentation/data_Clewett/Clewett_source_Exp1.csv", dec=",")

#=======Variables===============================================================
MyVariable <- "ratio" #variable to test
GrVariable <- "Condition" #grouping variable

ExpConditions <- c(unique(AllData$Condition)) #List of conditions
ExpConditions
Subject <- "Subject"

#=======Data preparation========================================================
# MyAllData <- data.frame(matrix(ncol = 3, nrow = 0))
# colnames(MyAllData) <- c("Subject", "ratio", GrVariable)
MyAllData <- data.frame(AllData[1], AllData[3], AllData[4])

# for (i in 1:length(ExpConditions)) {
#   tmp <- calculate_accuracy_ratio(AllData, MyVariable, ExpConditions[i])
#   MyAllData <- rbind(MyAllData, tmp)
# }
MyAllData$Condition <- as.factor(MyAllData$Condition)
is.factor(MyAllData$Condition)

#=======Plots titels and labels=================================================
#General
MyVariableLabel <- "Source memory"   #Label for the variable to test
gr_title <- "Source memory performance"  #Histogram titel
titel_outliers_rm <- "\nOutliers removed"   #Information about removed outliers

#Histogram
hist_title <- gr_title                              # Histogram titel
bw <- 0.05                                          # Histogram bin width
x_label <- "Performance -\n source memory"      # Label for x-axis
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

# hist1 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[1]),
#                      bw, ExpConditions[1], x_label, y_limit1)
# hist2 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[2]),
#                      bw, ExpConditions[2], x_label, y_limit1)
# #
# HistogramsA <- grid.arrange(hist1, hist2, ncol = 2,
#                           top = textGrob(hist_title))

hist1 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[1]),
                     bw, ExpConditions[1], x_label, y_limit1)
hist2 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[2]),
                     bw, ExpConditions[2], x_label, y_limit1)
hist3 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[3]),
                     bw, ExpConditions[3], x_label, y_limit1)
hist4 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[4]),
                     bw, ExpConditions[4], x_label, y_limit1)
#
HistogramsA <- grid.arrange(hist1, hist2, hist3, hist4, ncol = 2, nrow = 2,
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

# #=======Histogram - After removing outliers=====================================
# gr_title <- paste(gr_title, "\n", titel_outliers_rm, sep="")
# hist1 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[1]),
#                      bw, ExpConditions[1], x_label, y_limit1)
# hist2 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[2]),
#                      bw, ExpConditions[2], x_label, y_limit1)
# HistogramsB <- grid.arrange(hist1, hist2, ncol = 2,
#                            top = textGrob(hist_title))
# 
# #=======qqplot - After removing outliers========================================
# qqtitel <- paste(qqtitel, titel_outliers_rm, sep="")
# ggqqplot(MyAllData, "ratio", facet.by = GrVariable,
#          title=qqtitel)
# 
# #=======Box plots - After removing outliers=====================================
# boxplot1B <- draw_my_boxplot2(MyAllData,
#                               "Distance \n temporal distance",
#                               "Temporal distance ratings Distance by condition")
# boxplot1B
# 
# 
# box_titel <- paste(box_titel, titel_outliers_rm, sep="")
# boxplot2B <- draw_my_boxplot2(MyAllData,
#                  "Distance \n temporal distance",
#                  "Temporal distance ratings Distance by condition")
# 
# boxplot2B
# 
# rm(bw, hist_title, x_label, y_limit1, y_limit2, hist1, hist2, boxplot1B, boxplot2B, HistogramsA, HistogramsB)
# 
# #=======Descriptive statistics - After removing outliers========================
# describe.by(MyAllData, group = GrVariable)

# #=======Paired t-test - After removing outliers=================================
# t.test(subset(MyAllData, MyAllData$Condition == "SameContext")$ratio,
#        subset(MyAllData, MyAllData$Condition == "Boundary")$ratio, 
#        paired = T)
# cohen_stats <- cohen.d(MyAllData$ratio, MyAllData$Condition)
# cohen_stats


# #=======t-test Power analysis - After removing outliers=========================
# pwr.t.test(n = NULL, d = cohen_stats$cohen.d[2], sig.level = 0.05, power = 0.8,
#            type = c("paired"),
#            alternative = c("two.sided"))
# 

#=======ANOVA - After removing outliers=========================================
# anova_results <- anova_test(data = MyAllData,
#                      dv = ratio,
#                      wid = Subject,
#                      within = Condition)
# anova_results
# # anova_table <- get_anova_table(anova2)
# # anova_table
# 
# #ANOVA power analysis
# pwr.anova.test(k = 2, n = NULL, f = anova_results[1, 4], sig.level = 0.05, power = 0.8)
# 
# 

#https://datascienceplus.com/one-way-anova-in-r/
#https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/
# anov2 <- aov(ratio ~ Condition, data = MyAllData)
# # Summary of the analysis
# summary(anov2)
# TukeyHSD(anov2, conf.level = 0.95)
# plot(TukeyHSD(anov2, conf.level = 0.95),las=1, col = "red")
# 
# 
# 
# library(gplots)
# plotmeans(data= MyAllData, ratio ~ Condition, 
#           main="Mean Plot with 95% Confidence Interval", 
#           ylab = MyVariableLabel, 
#           xlab = GrVariable)
# 
# bartlett.test(data= MyAllData, ratio ~ Condition)
# levene.test(data= MyAllData, ratio ~ Condition)



anova3 <- anova_test(data=MyAllData, 
           dv=ratio, 
           wid=Subject, 
           within=Condition)
get_anova_table(anova3)
ef <- anova3[["ANOVA"]][["F"]]
#ANOVA power analysis
pwr.anova.test(k = 4, n = NULL, f = anova3[["ANOVA"]][["F"]], sig.level = 0.05, power = 0.8)
