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
#=======Data==============
OrgData <- read.csv("C:/Projects/Event-Segmentation/41467_2020_17851_MOESM4_ESM.csv", sep=";")

# empty_frame <- data.frame(matrix(ncol = 3, nrow = (nrow(Study1))))
empty_frame <- data.frame(matrix(ncol = 3, nrow = 0))

colnames(empty_frame) <- c("Subject", "ratio", "Condition")
experiment <- unique(c(OrgData$Experiment))  # Study1, Study2, Study3

temp_order_var <-c("Boundary_OrderAcc", 
                 "SameContext_OrderAcc")
temp_dist_var <- c("BoundaryDistanceRating", 
                   "SameContext_DistanceRating")
source_var <- c("BoundaryItem_SourceAcc", 
                "FirstItem_SourceAcc", 
                "LastIem_SourceAcc", 
                "SameContext_Item_SourceAcc")
Subject <- "Subject"
Experiment <- "Experiment"
Condition <- "Condition"

create_data <- function(AllData, variab) {
  data_i_need <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(data_i_need) <- c(Subject, Experiment, ratio, Condition)
  for (i in length(variab)) {
    variables <- c(Subject, Experiment, variab[i], Condition)
    df <- subset(OrgData, select=variables)
    df$Condition <- variab[i]
    colnames(df)[colnames(df) == variab[i]] <- 'ratio'
    

    data_i_need <- rbind(data_i_need, df)
      
  }
  # variables <- c(Subject, Experiment, variab)
  # data_i_need <- subset(OrgData, select=variables)
  return(data_i_need)
}

Temp_order_data <- create_data(OrgData, temp_order_var)
Temp_dist_data <- create_data(OrgData, temp_dist_var)
Source_data <- create_data(OrgData, source_var)




# Study2 <- subset(OrgData, Experiment=="Study2")
# Study3 <- subset(OrgData, Experiment=="Study3")


#Study1_BoundaryItem_SourceAcc <-empty_frame

  
# BoundaryItem_SourceAcc$Subject <- Study1$Subject
# BoundaryItem_SourceAcc$SourceAcc <- Study1$BoundaryItem_SourceAcc
# BoundaryItem_SourceAcc$Condition <- "Boundary"
# BoundaryItem_SourceAcc <- subset(Study1_BoundaryItem_SourceAcc, Study1_BoundaryItem_SourceAcc$SourceAcc != ".")

# Study1_LastIem_SourceAcc <- 
# Study1_SameContext_Item_SourceAcc <-

# create_MyAllData <- function(AllData) {
#   MyAllData <- data.frame(matrix(ncol = 3, nrow = 0))
#   colnames(MyAllData) <- c(Subject, "ratio", GrVariable)
#   
#   for (i in 1:length(ExpConditions)) {
#     tmp <- calculate_accuracy_ratio(AllData, MyVariable, ExpConditions[i])
#     MyAllData <- rbind(MyAllData, tmp)
#   }
#   return(MyAllData)
# }


# 
# for (i in 1:length(Experiment)) {
#   study1 <- subset(OrgData, Experiment==experiment[i])
#   
# }
# Study1 <- subset(OrgData, Experiment=="Study1")
# 
# 
# for (i in 1:nrow(Study1)) {
#   
# }