####lib####
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
library(ggstatsplot)
library(grid)
library(gridExtra)
library(pwr)
library(psych)

####Data####
rm(list=ls())
dev.off()
MyData <- read.csv("C:/Projects/Event-Segmentation/SourceMemory_Results_All.csv")

Data_SameContext <- subset.data.frame(MyData, Condition == "SameContext")

Subject_SourceAcc_SameContext <-
  Data_SameContext %>%
  group_by(Subject) %>%
  summarise_at(vars(SourceAcc),
               list(ratio = mean))


# Subject_SourceAcc_SameContext$Condition <- "SameContext"
# Subject_SourceAcc_SameContext$Condition <- as.factor(Subject_SourceAcc_SameContext$Condition)

####Functions####
Data_SameContext <- subset.data.frame(MyData, Condition == "SameContext")
####f1####
# calculate_accuracy_ratio <- function(data_frame) {
#   Subject_calculate_accuracy_ratio <-
#     data_frame %>%
#     group_by(Subject) %>%
#     summarise_at(vars(SourceAcc), 
#                  list(ratio = mean))
#   return(Subject_calculate_accuracy_ratio)
# }
####f2####
#Function calculate_accuracy_ratio(data_frame, variable, condition)
#   Calculates ratio of correct answers for specific variable and experimental 
#   condition for each subject seperately
#   Arguments:
#       data_frame: specify dataframe, (MyData, ..., ...)
#       variable: specify variable for ratio (..., "ReccencyACC", ...)
#       condition: specify experimental condition (..., ..., "SameContext")
calculate_accuracy_ratio <- function(data_frame, variable, condition) {
  Data_condition <- subset.data.frame(data_frame, Condition == condition)
  Subject_calculate_accuracy_ratio <-
    Data_condition %>%
    group_by(Subject) %>%
    summarise_at(vars(variable), 
                 list(ratio = mean))
  Subject_calculate_accuracy_ratio$Condition <- condition
  Subject_calculate_accuracy_ratio$Condition <- as.factor(Subject_calculate_accuracy_ratio$Condition)
  return(Subject_calculate_accuracy_ratio)
}

####f2####
normality_color <- function(dupaa) {
  lillie_test <- lillie.test(dupaa)
  
  if (lillie_test$p.value > 0.05) {
    col <- "darkseagreen1"
  } else {
    col <- "darksalmon"
  }
}
####lili and color####
lillie_SourceAcc_SameContext <- lillie.test(Subject_SourceAcc_SameContext$SourceAcc)

if (lillie_SourceAcc_SameContext$p.value > 0.05) {
  col1 <- "darkseagreen1"
  } else {
  col1 <- "darksalmon"
  }
####histogram####
bw <- 0.05
# Histograms:
hist1 <- Subject_SourceAcc_SameContext %>%
  ggplot(aes(x=SourceAcc), stat="count") +
  ylim(0,8) + 
  xlab("Performance -\n auditory source") + ylab("Count") +
  geom_histogram(binwidth=bw, fill=col1, color="black") + 
  geom_vline(aes(xintercept=mean(SourceAcc)),
             color="black", linetype="dashed", size=1) +
  labs(title = "Condition: \n SameContext")

Histograms <- grid.arrange(hist1,  ncol = 1,
                           top = textGrob("Auditory source memory performance"))
