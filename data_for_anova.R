#=======Clear environment=======================================================
cat("\f")
rm(list=ls())
dev.off()
# Set the directory path # Paste your path
setwd("C:/Projects/Event-Segmentation")
# Functions file:
source("Functions_.R")

library(dplyr)
library(tibble)

#=======Load data===============================================================
AllData    <- read.csv("C:/Projects/Event-Segmentation/data_RUB/TemporalMemory_Results_All.csv")
#=======Variables===============================================================
MyVariable    <- "DurationRESP" #variable to test
GrVariable    <- "Condition" #grouping variable
Subject       <- "Subject"      #subject variable

ExpConditions <- c(unique(AllData$Condition)) #List of conditions
ExpConditions

#=======Data====================================================================
# AllData    <- read.csv("C:/Projects/Event-Segmentation/data_RUB/SourceMemory_Results_All.csv")
# MyAllData  <- data.frame(matrix(ncol = 3, nrow = 0))
# colnames(MyAllData) <- c("Subject", "ratio", GrVariable)
# 
# for (i in 1:length(ExpConditions)) {
#   tmp <- calculate_accuracy_ratio(AllData, MyVariable, ExpConditions[i])
#   MyAllData <- rbind(MyAllData, tmp)
# }
# is.factor(MyAllData$Condition)

Data4_RManova  <- data.frame(matrix(ncol = 0, 
                                nrow = (nrow=length(unique(AllData$Subject)))))

# colnames(Data4_RManova) <- c("Subject")
Data4_RManova$Subject <- c(unique(AllData$Subject))

for (i in 1:length(ExpConditions)) {
  condition_name <- ExpConditions[i]
  tmp <- calculate_accuracy_ratio(AllData, MyVariable, condition_name)
  Ratio <- c(tmp$ratio)
  Data4_RManova <- add_column(Data4_RManova, Ratio)
  # Data4_RManova <- Data4_RManova %>% rename(paste(condition_name,collapse="") = Ratio)
  # colnames(Data4_RManova$Ratio) <- c(condition_name)
  names(Data4_RManova)[names(Data4_RManova) == 'Ratio'] <- ExpConditions[i]
  # add_column(Data4_RManova, condition = tmp$ratio)
  # rm(tmp)
}

write.csv(Data4_RManova,"C:/Projects/Event-Segmentation/data_RUB/TemporalMemory_Duration_RManova.csv", row.names = FALSE)
