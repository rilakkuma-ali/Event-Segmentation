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
AllData <- read.csv("C:/Projects/Event-Segmentation/data_RUB/SourceMemory_Results_All.csv")

#=======Variables===============================================================
MyVariable <- "SourceAcc" #variable to test
GrVariable <- "Condition" #grouping variable
Subject <- "Subject"      #subject variable

ExpConditions <- c(unique(AllData$Condition)) #List of conditions
ExpConditions
#=======Plot the datapoints=====================================================

# v1 <- c()
# a <- table(AllData$SourceAcc)
# a
  
cond1 <- subset(AllData, AllData$Condition == ExpConditions[1])
cond2 <- subset(AllData, AllData$Condition == ExpConditions[2])
cond3 <- subset(AllData, AllData$Condition == ExpConditions[3])
cond4 <- subset(AllData, AllData$Condition == ExpConditions[4])

v1 <-  
  as.data.frame(table(cond1$SourceAcc))
v1$Condition <- ExpConditions[1]

v2 <-  
  as.data.frame(table(cond2$SourceAcc))
v2$Condition <- ExpConditions[2]

v3 <-  
  as.data.frame(table(cond3$SourceAcc))
v3$Condition <- ExpConditions[3]

v4 <-  
  as.data.frame(table(cond4$SourceAcc))
v4$Condition <- ExpConditions[4]

v <- rbind(v1, v2, v3, v4)

  
Barplot1 <- ggplot(data=v, aes(x=Condition, 
                                     y=Freq,
                                     fill=Var1)) +
  geom_bar(position="stack", stat="identity") +
  # geom_text(aes(y=GrVariable, label=MyVariable), vjust=1.6, 
  #           color="white", size=3.5)+
  scale_fill_brewer(palette="Paired")
  # theme_minimal()
Barplot1

# Barplot2 <- ggplot(data=v, aes(x=Condition, 
#                                y=Freq,
#                                fill=Var1)) +
#   geom_bar(position="fill", 
#            stat="identity") +
#   scale_fill_brewer(palette="Paired") +
#   theme_minimal()
# 
# Barplot <- 
#   grid.arrange(Barplot1, Barplot2, ncol = 2,
#   

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
hist3 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[3]),
                     bw, ExpConditions[3], x_label, y_limit1)
hist4 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[4]),
                     bw, ExpConditions[4], x_label, y_limit1)
#
HistogramsA <- grid.arrange(hist1, hist2, hist3, hist4, ncol = 2, nrow = 2,
                            top = textGrob(hist_title))

#rm(bw, hist_title, x_label, y_limit, hist1, hist2, Histograms)
