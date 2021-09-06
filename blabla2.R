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
library(finalfit)

print(c(1:(118*2)*0))

#=======Directory, functions file===============================================
# Set the directory path # Paste your path
setwd("C:/Projects/Event-Segmentation")
# Functions file:
source("Functions_.R")

#=======Data====================================================================
AllData <- read.csv("C:/Projects/Event-Segmentation/SourceMemory_Results_All.csv")

#=======Variables===============================================================
MyVariable <- "SourceAcc" #variable to test
GrVariable <- "Condition" #grouping variable

ExpConditions <- c(unique(AllData$Condition)) #List of conditions
ExpConditions
Subject <- "Subject"

#=======Plots titels and labels=================================================
#General
MyVariableLabel <- " source"   #Label for the variable to test
gr_title <- " source memory performance"  #Histogram titel
titel_outliers_rm <- "\nOutliers removed"   #Information about removed outliers

#Histogram
hist_title <- gr_title                              # Histogram titel
bw <- 0.05                                          # Histogram bin width
x_label <- "Performance -\n  source"      # Label for x-axis
y_limit1 <- c(0, 5)                                 # y limit
#QQ plot
qqtitel <- "Quantile-Quantile plot"                 # QQ plot titel

#Boxplot
box_ylabel <- MyVariableLabel  #label for the boxplot y-axis
box_titel <- "My boxplot titel"
box_titel < paste(gr_title, " ", box_titel, sep="")

#=======Data preparation========================================================

MyAllData <- create_MyAllData(AllData)
is.factor(MyAllData$Condition)

#---Outliers====
  outliers <- MyAllData %>%
    group_by(Condition) %>%
    identify_outliers(ratio)
  
  outliers_subjects <- unique(c(outliers$Subject))
  are_any_outliers <- !is.null(outliers_subjects)
  outliers_message <- paste("OUTLIERS: Removed ",
                            length(outliers_subjects),
                            " subject(s) from the data. Subject(s) numbers: ",
                            toString(outliers_subjects),
                            sep="")
  outliers_subjects
  are_any_outliers
  outliers_message 
fff <- c(outliers_subjects, are_any_outliers, outliers_message)  
fff
