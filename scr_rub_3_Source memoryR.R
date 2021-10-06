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
library(coin)

print(c(1:(118*2)*0))

#=======Directory, functions file===============================================
# Set the directory path # Paste your path
setwd("C:/Projects/Event-Segmentation")
# Functions file:
source("Functions_.R")

#=======Data====================================================================
# AllData <- read.csv("C:/Projects/Event-Segmentation/data_RUB/SourceMemory_Results_All.csv")
AllData <- read.csv("C:/Projects/Event-Segmentation/data_RUB/SourceMemory_Results_All_kopia_10.csv",  dec=",")


#=======Variables===============================================================
MyVariable <- "SourceAcc" #variable to test
GrVariable <- "Condition" #grouping variable
Subject <- "Subject"      #subject variable

ExpConditions <- c(unique(AllData$Condition)) #List of conditions
ExpConditions

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
MyVariableLabel <- "source"   #Label for the variable to test
gr_title <- " Source memory accuracy ratio"  #Histogram titel, barplot titel
titel_outliers_rm <- "\nOutliers removed"   #Information about removed outliers

#Histogram
hist_title <- gr_title                              # Histogram titel
bw <- 0.05                                          # Histogram bin width
x_label <- "Source memory - Accuracy ratio"      # Label for x-axis
y_limit1 <- c(0, 5)                                 # y limit
#QQ plot
qqtitel <- "Quantile-Quantile plot"                 # QQ plot titel

#Boxplot
box_ylabel <- "Source memory - accuracy ratio"   #label for the boxplot y-axis
box_titel <- "Source memory ratings accuracy ratio by condition"
# box_titel <- paste(gr_title, " ", box_titel, sep="")

#=======Freq bar plot=====================================================

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
  # ylab("Frequency")
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Paired") +
  ggtitle(paste("Number of datapoints for each condition. \nVariable: ", MyVariable, sep="")) +
  labs(fill = MyVariable)
  theme_minimal()

Barplot1



#=======Lilliefors normality test===============================================

for (i in 1:length(ExpConditions)) {
  tmp <- subset.data.frame(MyAllData, Condition == ExpConditions[i])
  print(paste("Lilliefors normality test: ", ExpConditions[i], sep=""))
  print(lillie.test(tmp$ratio)[1:2])
  rm(tmp)
}

#=======Descriptive statistics==================================================
descr <- describe.by(sample(MyAllData[,2]), group = GrVariable)
descr
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


#=======ggplot####==============================================================
ggqqplot(MyAllData, "ratio", facet.by = GrVariable,
         title=qqtitel)

#=======Box plots===============================================================
boxplot1B <- draw_my_boxplot2(MyAllData,
                 box_ylabel,
                 box_titel)
boxplot1B

#=======Outliers================================================================
outliers <- c(check_name_outliers(MyAllData))
outliers

MyAllData2 <- remove_outliers(MyAllData, outliers)

is.factor(MyAllData2$Condition)

#=======Homoscedasticity - Levene Test==========================================
leveneTest(MyAllData2$ratio, MyAllData2$Condition)

#=======Histogram - After removing outliers=====================================
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

#=======qqplot - After removing outliers========================================
qqtitel <- paste(qqtitel, titel_outliers_rm, sep="")
ggqqplot(MyAllData, "ratio", facet.by = GrVariable,
         title=qqtitel)

#=======Box plots - After removing outliers=====================================
box_titel2 <- paste(box_titel, titel_outliers_rm, sep="")
boxplot2B <- draw_my_boxplot2(MyAllData,
                 box_ylabel,
                 box_titel2)

boxplot2B

# rm(bw, hist_title, x_label, y_limit1, y_limit2, hist1, hist2, boxplot1B, boxplot2B, HistogramsA, HistogramsB)

#=======Descriptive statistics - After removing outliers========================
describe.by(MyAllData2, group = GrVariable)


#=======ANOVA - After removing outliers=========================================
anova_results <- anova_test(data = MyAllData,
                     dv = ratio,
                     wid = Subject,
                     within = Condition)
anova_results
# anova_table <- get_anova_table(anova2)
# anova_table

#ANOVA power analysis
pwr.anova.test(k = 4, n = NULL, f = anova_results[["ANOVA"]][["F"]], sig.level = 0.05, power = 0.8)




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
           
#=======Data - only 2 variables=================================================

MyAllData3 <- subset.data.frame(MyAllData, 
                                Condition == c(ExpConditions[1]) | Condition == c(ExpConditions[2]))
  
#=======Box plots - 4 2 variables only==========================================
boxplot1B <- draw_my_boxplot2(MyAllData3,
                              box_ylabel,
                              box_titel)
boxplot1B

#=======Outliers================================================================
outliers <- c(check_name_outliers(MyAllData3))
outliers

MyAllData4 <- remove_outliers(MyAllData3, outliers)

is.factor(MyAllData4$Condition)


#=======Homoscedasticity - Levene Test==========================================
leveneTest(MyAllData4$ratio, MyAllData4$Condition)

#=======Histogram - After removing outliers - 2 conditions======================
hist1 <- draw_a_hist(subset(MyAllData4, MyAllData4$Condition == ExpConditions[1]),
                     bw, ExpConditions[1], x_label, y_limit1)
hist2 <- draw_a_hist(subset(MyAllData4, MyAllData4$Condition == ExpConditions[2]),
                     bw, ExpConditions[2], x_label, y_limit1)

#
HistogramsA2 <- grid.arrange(hist1, hist2, ncol = 2,
                            top = textGrob(hist_title))

#=======Box plots - After removing outliers, 2 conditions=======================
box_titel2 <- paste(box_titel, titel_outliers_rm, sep="")
boxplot2B <- draw_my_boxplot2(MyAllData4,
                              box_ylabel,
                              box_titel2)

boxplot2B

#=======Paired Samples Wilcoxon Test============================================

# SameContext4 <- subset(MyAllData4,  Condition == "SameContext", ratio,
#                  drop = TRUE)
# Boundary4 <- subset(MyAllData4,  Condition == "Boundary", ratio,
#                        drop = TRUE)
# 
# wilcox.test(SameContext4, Boundary4, paired = TRUE, alternative = "two.sided")

wilcox.test(ratio ~ Condition, data = MyAllData4, paired = TRUE)

# Effect size
MyAllData4  %>%
  wilcox_effsize(ratio ~ Condition, paired = TRUE)

#=======Log10(x)================================================================
MyAllData4 <- MyAllData
MyAllData4$ratio <- sqrt(MyAllData4$ratio)
#subset data for 2 variables
MyAllData5 <- subset.data.frame(MyAllData4, 
                                Condition == c(ExpConditions[1]) | Condition == c(ExpConditions[2]))

#=======Histogram - After log10(x) for positive skew============================
hist1 <- draw_a_hist(subset(MyAllData5, MyAllData5$Condition == ExpConditions[1]),
                     bw, ExpConditions[1], x_label, y_limit1)
hist2 <- draw_a_hist(subset(MyAllData5, MyAllData5$Condition == ExpConditions[2]),
                     bw, ExpConditions[2], x_label, y_limit1)
#
HistogramsA <- grid.arrange(hist1, hist2, 
                            ncol = 2,
                            top = textGrob(hist_title))
#======Boxplot - After log10(x) for positive skew===============================
box_titel3 <- paste(box_titel, "\nlog10(x)", sep="")
boxplot2B <- draw_my_boxplot2(MyAllData5,
                              box_ylabel,
                              box_titel3)

boxplot2B
#=======Outliers after log10(x), 2 conditions===================================
outliers <- c(check_name_outliers(MyAllData5))
outliers

MyAllData6 <- remove_outliers(MyAllData5, outliers)

is.factor(MyAllData6$Condition)
#=======Histogram - After log10(x) and removing outliers========================
hist1 <- draw_a_hist(subset(MyAllData6, MyAllData6$Condition == ExpConditions[1]),
                     bw, ExpConditions[1], x_label, y_limit1)
hist2 <- draw_a_hist(subset(MyAllData6, MyAllData6$Condition == ExpConditions[2]),
                     bw, ExpConditions[2], x_label, y_limit1)
#
HistogramsA <- grid.arrange(hist1, hist2, 
                            ncol = 2,
                            top = textGrob(hist_title))
