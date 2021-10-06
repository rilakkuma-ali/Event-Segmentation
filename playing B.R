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

library(BayesFactor)
library(hablar)

print(c(1:(118*2)*0))

##### Data #####
# Set the directory path # Paste your path
setwd("C:/Projects/Event-Segmentation")
AllData <- read.csv("C:/Projects/Event-Segmentation/data_Clewett/Clewett_source_Exp1.csv", dec=",")

print(c(1:(118*2)*0))
# AllData %>% convert(fct(Subject, Condition, Experiment))
# # as.factor(MyAllData$Subject)
# is.factor(AllData$Condition)
# 
# is.factor(AllData$Subject)
#=======Directory, functions file===============================================
# Set the directory path # Paste your path
setwd("C:/Projects/Event-Segmentation")
# Functions file:
source("Functions_.R")


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

#======
MyAllData %>% convert(fct(Subject, Condition))
is.factor(MyAllData$Condition)

# Problem with converting into a factor? 
# https://stackoverflow.com/questions/53989587/problems-with-converting-class-to-factor-in-dataframe
MyAllData$Subject <- factor( unlist(MyAllData$Subject))
is.factor(MyAllData$Subject)

##=====Runda1==============
## https://richarddmorey.github.io/BayesFactor/#loading
# summary(aov(data=MyAllData,  ratio ~ Condition + Error(Subject/(Condition))))
# bf = anovaBF(data=MyAllData,  ratio ~ Condition + Subject,  
#              whichRandom="Subject")
# bf
# plot(bf)
# 
# 
# bfWithoutID = lmBF(data=MyAllData,  ratio ~ Condition)
# bfWithoutID
# 
# bfOnlyID = lmBF(data=MyAllData,  ratio ~ Subject,  whichRandom="Subject")
# bfOnlyID 
# bf2 = bfWithoutID / bfOnlyID
# bf2
# 
# bfall = c(bf,bf2)
# bfall
# # result <- bf[4] / bf2[4]
# 
# bfall[1]/bfall[2]

#======Runda2============


