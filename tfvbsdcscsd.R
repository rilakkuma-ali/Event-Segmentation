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
dat <- OrgData

experiment <- unique(c(OrgData$Experiment))  # Study1, Study2, Study3


temp_order_var <-c("Boundary_OrderAcc", 
                   "SameContext_OrderAcc")
temp_dist_var <- c("BoundaryDistanceRating", 
                   "SameContext_DistanceRating")
source_var <- c("BoundaryItem_SourceAcc", 
                "SameContext_Item_SourceAcc",
                "FirstItem_SourceAcc", 
                "LastIem_SourceAcc")

variab <- source_var
var_names <- c("Subject", "Experiment", "ratio", "Condition")
conditions <- c("Boundary", "SameContext", "First", "Last")

dczc <- split_time_series(dat, variab, conditions, var_names)

dato <- subset(dczc, dczc$ratio != ".")


Clewett_temp_order_All <- split_time_series(dat, temp_order_var, conditions, var_names)
Clewett_temp_dist_All <- split_time_series(dat, temp_dist_var, conditions, var_names)
Clewett_source_All <- split_time_series(dat, source_var, conditions, var_names)
Clewett_source_All <- subset(Clewett_source_All, Clewett_source_All$ratio != ".")

write.csv(Clewett_temp_order_All,
          "C:/Projects/Event-Segmentation/data_Clewett/Clewett_temp_order_All.csv", 
          row.names = FALSE)
write.csv(Clewett_temp_dist_All,
          "C:/Projects/Event-Segmentation/data_Clewett/Clewett_temp_dist_All.csv", 
          row.names = FALSE)
write.csv(Clewett_source_All,
          "C:/Projects/Event-Segmentation/data_Clewett/Clewett_source_All.csv", 
          row.names = FALSE)


Clewett_temp_order_Exp1 <- subset(Clewett_temp_order_All, Clewett_temp_order_All$Experiment == "Study1")
Clewett_temp_order_Exp2 <- subset(Clewett_temp_order_All, Clewett_temp_order_All$Experiment == "Study2")
Clewett_temp_order_Exp3 <- subset(Clewett_temp_order_All, Clewett_temp_order_All$Experiment == "Study3")

write.csv(Clewett_temp_order_Exp1,
          "C:/Projects/Event-Segmentation/data_Clewett/Clewett_temp_order_Exp1.csv", 
          row.names = FALSE)
write.csv(Clewett_temp_order_Exp2,
          "C:/Projects/Event-Segmentation/data_Clewett/Clewett_temp_order_Exp2.csv", 
          row.names = FALSE)
write.csv(Clewett_temp_order_Exp3,
          "C:/Projects/Event-Segmentation/data_Clewett/Clewett_temp_order_Exp3.csv", 
          row.names = FALSE)

Clewett_temp_dist_Exp1 <- subset(Clewett_temp_dist_All, Clewett_temp_dist_All$Experiment == "Study1")
Clewett_temp_dist_Exp2 <- subset(Clewett_temp_dist_All, Clewett_temp_dist_All$Experiment == "Study2")
Clewett_temp_dist_Exp3 <- subset(Clewett_temp_dist_All, Clewett_temp_dist_All$Experiment == "Study3")

write.csv(Clewett_temp_dist_Exp1,
          "C:/Projects/Event-Segmentation/data_Clewett/Clewett_temp_dist_Exp1.csv", 
          row.names = FALSE)
write.csv(Clewett_temp_dist_Exp2,
          "C:/Projects/Event-Segmentation/data_Clewett/Clewett_temp_dist_Exp2.csv", 
          row.names = FALSE)
write.csv(Clewett_temp_dist_Exp3,
          "C:/Projects/Event-Segmentation/data_Clewett/Clewett_temp_dist_Exp3.csv", 
          row.names = FALSE)

Clewett_source_Exp1 <- subset(Clewett_source_All, Clewett_source_All$Experiment == "Study1")
Clewett_source_Exp2 <- subset(Clewett_source_All, Clewett_source_All$Experiment == "Study2")
Clewett_source_Exp3 <- subset(Clewett_source_All, Clewett_source_All$Experiment == "Study3")

write.csv(Clewett_source_Exp1,
          "C:/Projects/Event-Segmentation/data_Clewett/Clewett_source_Exp1.csv", 
          row.names = FALSE)
write.csv(Clewett_source_Exp2,
          "C:/Projects/Event-Segmentation/data_Clewett/Clewett_source_Exp2.csv", 
          row.names = FALSE)
write.csv(Clewett_source_Exp3,
          "C:/Projects/Event-Segmentation/data_Clewett/Clewett_source_Exp3.csv", 
          row.names = FALSE)