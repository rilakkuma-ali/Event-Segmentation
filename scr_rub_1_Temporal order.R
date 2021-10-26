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
library(apaTables)
library(WriteXLS)

print(c(1:(118*2)*0))

#=======Directory, functions file===============================================
# Set the directory path # Paste your path
setwd("C:/Projects/Event-Segmentation")
# Functions file:
source("Functions_.R")

#=======Data====================================================================
AllData <- read.csv("C:/Projects/Event-Segmentation/data_RUB/TemporalMemory_Results_All.csv")
#=======Data for rmANOVA========================================================
# RManova <- read.csv("C:/Projects/Event-Segmentation/data_RUB/TemporalMemory_Recency_RManova.csv")
#=======Variables===============================================================
MyVariable <- "RecencyAcc" #variable to test
GrVariable <- "Condition" #grouping variable

# # Experimental conditions - unique values from the data
# ExpConditions <- c(unique(AllData$Condition)) #List of conditions
# ExpConditions

# Hard coding experimental conditions (to change the order of the graphs)
ExpConditions <- c("Boundary", "SameContext")
ExpConditions

# Blocks <- c(unique(AllData$Block))
# Blocks
#=======Data preparation========================================================
MyAllData <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(MyAllData) <- c("Subject", "ratio", GrVariable)


for (i in 1:length(ExpConditions)) {
  tmp <- calculate_accuracy_ratio(AllData, MyVariable, ExpConditions[i])
  MyAllData <- rbind(MyAllData, tmp)
  rm(tmp)
}
is.factor(MyAllData$Condition)

#=======Filtering participants < ratio 0.5============================
Subject_calculate_accuracy_ratio <-
  AllData %>%
    group_by(Subject) %>%
    summarise_at(MyVariable, 
                 list(ratio = mean))
Subject_calculate_accuracy_ratio

subjects_to_del <- Subject_calculate_accuracy_ratio %>% filter(ratio < 0.5)
subjects_to_del <- unique(c(subjects_to_del$Subject))
subjects_to_del

MyAllData <- remove_outliers(MyAllData, subjects_to_del)

is.factor(MyAllData$Condition)

#=======Plots titels and labels=================================================
#General
MyVariableLabel <- "Temporal order - accuracy ratio"   #Label for the variable to test
gr_title <- "Temporal order memory accuracy ratio"  #Histogram titel
# gr_title <- "Temporal order memory accuracy ratio"  #Histogram titel
titel_outliers_rm <- "\nOutliers removed"   #Information about removed outliers

#Histogram
hist_title <- gr_title                              # Histogram titel
bw <- 0.05                                          # Histogram bin width
x_label <- "Accuracy ratio -\ntemporal order"       # Label for x-axis
y_limit1 <- c(0, 5)                                 # y limit
#QQ plot
qqtitel <- "Quantile-Quantile plot"                 # QQ plot titel

#Boxplot
box_ylabel <- MyVariableLabel  #label for the boxplot y-axis
box_titel <- "Temporal order ratings accuracy ratio by condition"
# box_titel < paste(gr_title, " ", box_titel, sep="")

#=======spaghetti plot==========================================================
interaction.plot(MyData_Blocks$Block,
                 MyData_Blocks$Subject, 
                 MyData_Blocks$ratio, 
                 col=c(1:15),
                 xlab="Block", 
                 ylab=x_label, 
                 legend=F)

MyData_Blocks <- MyData_Blocks %>% filter(Block <= 10)

fit <- by(MyData_Blocks,
          MyData_Blocks$Subject,
          function(x) fitted.values(lm(MyData_Blocks$ratio ~
                                         MyData_Blocks$Block, 
                                       data=x)))  



# fit1 <- unlist(fit)
# names(fit1) <- NULL
# #plotting the linear fit by id
# interaction.plot(MyData_Blocks$Block, 
#                  MyData_Blocks$Subject, 
#                  fit1,
#                  xlab="Block", 
#                  ylab=x_label, 
#                  legend=F)

#=======Freq bar plot=====================================================

cond1 <- subset(AllData, AllData$Condition == ExpConditions[1])
cond2 <- subset(AllData, AllData$Condition == ExpConditions[2])

v1 <-
  as.data.frame(table(cond1$RecencyAcc))
v1$Condition <- ExpConditions[1]

v2 <-
  as.data.frame(table(cond2$RecencyAcc))
v2$Condition <- ExpConditions[2]


v <- rbind(v2, v1)

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
descriptive <- describe.by(MyAllData, group = GrVariable)
descriptive

#=======Histogram===============================================================

#Before removing outliers
#outliers_check <- c(check_name_outliers(MyAllData))

hist1 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[1]),
                     bw, ExpConditions[1], x_label, y_limit1)
hist2 <- draw_a_hist(subset(MyAllData, MyAllData$Condition == ExpConditions[2]),
                     bw, ExpConditions[2], x_label, y_limit1)

HistogramsA <- grid.arrange(hist1, hist2, ncol = 2,
                           top = textGrob(hist_title))

# HistogramsA <- grid.arrange(hist2, hist1, ncol = 2,
#                             top = textGrob(hist_title))

#rm(bw, hist_title, x_label, y_limit, hist1, hist2, Histograms)

#=======ggplot####==============================================================
ggqqplot(MyAllData, "ratio", facet.by = GrVariable,
         title=qqtitel)

#=======Box plots===============================================================
boxplot1B <- draw_my_boxplot2(MyAllData,
                 "order \n temporal order",
                 "Temporal order ratings order by condition")
boxplot1B

#=======Outliers================================================================
outliers_check <- c(check_name_outliers(MyAllData))
outliers_check
  # outliers_list <- outliers_check[3]
# 
# MyAllData2 <- remove_outliers(MyAllData, outliers_list)
# 
# is.factor(MyAllData$Condition)

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
# box_titel <- paste(box_titel, titel_outliers_rm, sep="")
# boxplot2B <- draw_my_boxplot2(MyAllData,
#                  "order \n temporal order",
#                  "Temporal order ratings order by condition")
# 
# boxplot2B
# 
# # rm(bw, hist_title, x_label, y_limit1, y_limit2, hist1, hist2, boxplot1B, boxplot2B, HistogramsA, HistogramsB)
# 
# #=======Descriptive statistics - After removing outliers========================
# describe.by(MyAllData, group = GrVariable)
# 
#=======Paired t-test - After removing outliers=================================
# t.test(subset(MyAllData, MyAllData$Condition == "SameContext")$ratio,
#        subset(MyAllData, MyAllData$Condition == "Boundary")$ratio, 
#        paired = T)
# cohen_stats <- cohen.d(MyAllData$ratio, MyAllData$Condition)
# cohen_stats

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
anova_results <- anova_test(data = RManova,
                            dv = ratio,
                            wid = Subject,
                            within = Condition)
anova_results
# anova_table <- get_anova_table(anova2)
# anova_table

#ANOVA power analysis
pwr.anova.test(k = 2, n = NULL, f = anova_results[1, 4], sig.level = 0.05, power = 0.8)


# #=======ANOVA - After removing outliers=========================================
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


