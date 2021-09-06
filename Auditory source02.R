##### Libraries #####
rm(list=ls())
dev.off()

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

##### Data #####
# Set the directory path # Paste your path
setwd("C:/Projects/Event-Segmentation")
# Functions file:
source("Functions_.R")

#MyData <- read.csv("C:/Projects/Event-Segmentation/SourceMemory_Results_All_kopia_bez3_b.csv", sep = ";")
MyData <- read.csv("C:/Projects/Event-Segmentation/SourceMemory_Results_All.csv")


Data_SameContext <- subset.data.frame(MyData, Condition == "SameContext")
Data_Boundary <- subset.data.frame(MyData, Condition == "Boundary")
Data_First <- subset.data.frame(MyData, Condition == "First")
Data_Last <- subset.data.frame(MyData, Condition == "Last")

# Preparing data for Lilliefors test

Subject_SourceAcc_SameContext <- calculate_accuracy_ratio(MyData, "SourceAcc", "SameContext")
Subject_SourceAcc_Boundary <- calculate_accuracy_ratio(MyData, "SourceAcc", "Boundary")
Subject_SourceAcc_First <- calculate_accuracy_ratio(MyData, "SourceAcc", "First")
Subject_SourceAcc_Last <- calculate_accuracy_ratio(MyData, "SourceAcc", "Last")

Subject_SourceAcc <- rbind(Subject_SourceAcc_SameContext, 
                           Subject_SourceAcc_Boundary,
                           Subject_SourceAcc_First,
                           Subject_SourceAcc_Last)
is.factor(Subject_SourceAcc$Condition)

# ##### Lilliefors normality test #####
# 
# 
# #Lilliefors normality test #
# lillie.test(Subject_SourceAcc_SameContext$ratio)
# lillie.test(Subject_SourceAcc_Boundary$ratio)
# lillie.test(Subject_SourceAcc_First$ratio)
# lillie.test(Subject_SourceAcc_Last$ratio)

# ##### qq plot #####
# ggqqplot(Subject_SourceAcc_SameContext, Subject_SourceAcc_SameContext$ratio)
# ggqqplot(Subject_SourceAcc_Boundary, Subject_SourceAcc_Boundary$ratio)
# ggqqplot(Subject_SourceAcc_First, Subject_SourceAcc_First$ratio)
# ggqqplot(Subject_SourceAcc_Last, Subject_SourceAcc_Last$ratio)

##### Histograms #####==========================================================
#===============================================================================
#General:=======================================================================
# Histogram bin width
bw <- 0.05
# Graph title:
gr_title <- "Auditory source memory performance"
# Label for x-axis:
x_label <- "Performance -\n auditory source"
# y limit
y_limit <- c(0, 9)
#===============================================================================
hist1 <- draw_a_hist(Subject_SourceAcc_SameContext, bw, "SameContext", x_label, y_limit)
hist2 <- draw_a_hist(Subject_SourceAcc_Boundary, bw, "Boundary", x_label, y_limit)
hist3 <- draw_a_hist(Subject_SourceAcc_First, bw, "First", x_label, y_limit)
hist4 <- draw_a_hist(Subject_SourceAcc_Last, bw, "Last", x_label, y_limit)

#===============================================================================
Histograms <- grid.arrange(hist1, hist2, hist3, hist4, ncol = 2, nrow = 2,
                           top = textGrob(gr_title))  

rm(bw, gr_title, x_label, y_limit, hist1, hist2, hist3, hist4, Histograms)
#===============================================================================

##### Box plots #####
# Subject_SourceAcc %>%
#   group_by(Condition)

draw_my_boxplot(Subject_SourceAcc, 
                "SourceAcc \n temporal SourceAcc", 
                "Auditory source memory accuracy by condition")

##### Outliers #####
outliers <- Subject_SourceAcc %>%
  group_by(Condition) %>%
  identify_outliers(ratio)
outliers

outliers_subjects <- unique(c(outliers$Subject))
outliers_subjects

# outliers indices found by hand

#Subject_SourceAcc2 <- subset(Subject_SourceAcc, Subject_SourceAcc$Subject!= 313 & Subject_SourceAcc$Subject!= 321 & Subject_SourceAcc$Subject!= 319)

Subject_SourceAcc2 <- Subject_SourceAcc
for(i in 1:length(outliers_subjects)) {
  Subject_SourceAcc2 <- Subject_SourceAcc2[Subject_SourceAcc2$Subject != outliers_subjects[i],]
}

# Data_SameContext2 <- subset(Data_SameContext, Data_SameContext$Subject!= 313 & Data_SameContext$Subject!= 321 & Data_SameContext$Subject!= 319)
# Data_Boundary2 <- subset(Data_Boundary, Data_Boundary$Subject!= 313 & Data_Boundary$Subject!= 321 & Data_Boundary$Subject!= 319)
# Data_First2 <- subset(Data_First, Data_First$Subject!= 313 & Data_First$Subject!= 321 & Data_First$Subject!= 319)
# Data_Last2 <- subset(Data_Last, Data_Last$Subject!= 313 & Data_Last$Subject!= 321 & Data_Last$Subject!= 319)

##### Outliers for pairs of conditions#####

Subject_SourceAcc_SameContext_Boundary <- subset(Subject_SourceAcc, Condition == "SameContext" | Condition == "Boundary")
Subject_SourceAcc_First_Last <- subset(Subject_SourceAcc, Condition == "First" | Condition == "Last")

outliers_SameContext_Boundary <- Subject_SourceAcc_SameContext_Boundary %>%
  group_by(Condition) %>%
  identify_outliers(ratio)
outliers_SameContext_Boundary

outliers_subjects_SameContext_Boundary <- unique(c(outliers_SameContext_Boundary$Subject))
outliers_subjects_SameContext_Boundary

outliers_First_Last <- Subject_SourceAcc_First_Last %>%
  group_by(Condition) %>%
  identify_outliers(ratio)
outliers_First_Last

outliers_subjects_First_Last <- unique(c(outliers_First_Last$Subject))
outliers_subjects_First_Last

#Remove outliers

Subject_SourceAcc_SameContext_Boundary2 <- Subject_SourceAcc_SameContext_Boundary
for(i in 1:length(outliers_subjects_SameContext_Boundary)) {
  Subject_SourceAcc_SameContext_Boundary2 <- 
    Subject_SourceAcc_SameContext_Boundary2[Subject_SourceAcc_SameContext_Boundary2$Subject 
                                            != outliers_subjects_SameContext_Boundary[i],]
}

Subject_SourceAcc_First_Last2 <- Subject_SourceAcc_First_Last
for(i in 1:length(outliers_subjects_First_Last)) {
  Subject_SourceAcc_First_Last2 <- 
    Subject_SourceAcc_First_Last2[Subject_SourceAcc_First_Last2$Subject 
                                            != outliers_subjects_First_Last[i],]
}

# outliers indices found by hand

#Subject_SourceAcc2 <- subset(Subject_SourceAcc, Subject_SourceAcc$Subject!= 313 & Subject_SourceAcc$Subject!= 321 & Subject_SourceAcc$Subject!= 319)
# Data_SameContext2 <- subset(Data_SameContext, Data_SameContext$Subject!= 313 & Data_SameContext$Subject!= 321 & Data_SameContext$Subject!= 319)
# Data_Boundary2 <- subset(Data_Boundary, Data_Boundary$Subject!= 313 & Data_Boundary$Subject!= 321 & Data_Boundary$Subject!= 319)
# Data_First2 <- subset(Data_First, Data_First$Subject!= 313 & Data_First$Subject!= 321 & Data_First$Subject!= 319)
# Data_Last2 <- subset(Data_Last, Data_Last$Subject!= 313 & Data_Last$Subject!= 321 & Data_Last$Subject!= 319)


##### Homoscedasticity - Levene Test#####
leveneTest(Subject_SourceAcc_SameContext_Boundary$ratio, 
           Subject_SourceAcc_SameContext_Boundary$Condition)

leveneTest(Subject_SourceAcc_SameContext_Boundary2$ratio, 
           Subject_SourceAcc_SameContext_Boundary2$Condition)

leveneTest(Subject_SourceAcc_First_Last $ratio, 
           Subject_SourceAcc_First_Last $Condition)

leveneTest(Subject_SourceAcc_First_Last2 $ratio, 
           Subject_SourceAcc_First_Last2 $Condition)

####Normality test####
Subject_SourceAcc_SameContext2 <- 
  Subject_SourceAcc_SameContext_Boundary2[Subject_SourceAcc_SameContext_Boundary2$Condition == "SameContext",] 
lillie_SourceAcc_SameContext2 <- lillie.test(Subject_SourceAcc_SameContext2$ratio)

Subject_SourceAcc_Boundary2 <- 
  Subject_SourceAcc_SameContext_Boundary2[Subject_SourceAcc_SameContext_Boundary2$Condition == "Boundary",]
lillie_SourceAcc_Boundary2 <- lillie.test(Subject_SourceAcc_Boundary2$ratio)

Subject_SourceAcc_First2 <-
  Subject_SourceAcc_First_Last2[Subject_SourceAcc_First_Last2$Condition == "First",]
lillie_SourceAcc_First2 <- lillie.test(Subject_SourceAcc_First2$ratio)

Subject_SourceAcc_Last2 <- 
  Subject_SourceAcc_First_Last2[Subject_SourceAcc_First_Last2$Condition == "Last",]
lillie_SourceAcc_Last2 <- lillie.test(Subject_SourceAcc_Last2$ratio)

lillie_SourceAcc_SameContext2
lillie_SourceAcc_Boundary2
lillie_SourceAcc_First2
lillie_SourceAcc_Last2

# ##### Histograms #####


# Bar colors

if (lillie_SourceAcc_SameContext2$p.value > 0.05) {
  col1 <- "darkseagreen1"
} else {
  col1 <- "darksalmon"
}
if (lillie_SourceAcc_Boundary2$p.value > 0.05) {
  col2 <- "darkseagreen1"
} else {
  col2 <- "darksalmon"
}
if (lillie_SourceAcc_First2$p.value > 0.05) {
  col3 <- "darkseagreen1"
} else {
  col3 <- "darksalmon"
}
if (lillie_SourceAcc_Last2$p.value > 0.05) {
  col4 <- "darkseagreen1"
} else {
  col4 <- "darksalmon"
}


# # Histogram bin width
bw <- 0.05
# Histograms:
hist1 <- Subject_SourceAcc_SameContext2 %>%
  ggplot(aes(x=ratio), stat="count") +
  ylim(0,8) + 
  xlab("Performance -\n auditory source") + ylab("Count") +
  geom_histogram(binwidth=bw, fill=col1, color="black") + 
  geom_vline(aes(xintercept=mean(ratio)),
             color="black", linetype="dashed", size=1) +
  labs(title = "Condition: \n SameContext")

hist2 <- Subject_SourceAcc_Boundary2 %>%
  ggplot(aes(x=ratio), stat="count") +
  ylim(0,8) + 
  xlab("Performance -\n auditory source") + ylab("Count") +
  geom_histogram(binwidth=bw, fill=col2, color="black") +
  geom_vline(aes(xintercept=mean(ratio)),
             color="black", linetype="dashed", size=1) +
  labs(title = "Condition: \n Boundary")

hist3 <- Subject_SourceAcc_First2 %>%
  ggplot(aes(x=ratio), stat="count") +
  ylim(0,8) + 
  xlab("Performance -\n auditory source") + ylab("Count") +
  geom_histogram(binwidth=bw, fill=col3, color="black") +
  geom_vline(aes(xintercept=mean(ratio)),
             color="black", linetype="dashed", size=1) +
  labs(title = "Condition: \n First")

hist4 <- Subject_SourceAcc_Last2 %>%
  ggplot(aes(x=ratio), stat="count") +
  ylim(0,8) + 
  xlab("Performance -\n auditory source") + ylab("Count") +
  geom_histogram(binwidth=bw, fill=col4, color="black") +
  geom_vline(aes(xintercept=mean(ratio)),
             color="black", linetype="dashed", size=1) +
  labs(title = "Condition: \n Last")

Histograms <- grid.arrange(hist1, hist2, hist3, hist4, ncol = 2, nrow = 2, 
                           top = textGrob("Auditory source memory performance \n Outliers removed for pares: \n SameContext - Boundary,  First - Last"))

##### Box plots #####
# Subject_SourceAcc %>%
#   group_by(Condition)
# head(Subject_SourceAcc)
# bxp <- ggplot(Subject_SourceAcc, 
#               aes(x = Condition, y=ratio)) +
#   geom_boxplot(notch = F)
# bxp

# head(Subject_SourceAcc2)
# bxp <- ggplot(Subject_SourceAcc2, 
#               aes(x = Condition, y=ratio, fill = Condition)) +
#   geom_boxplot(notch = F) +
#   scale_fill_brewer(palette="Set2") +
#   geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.4) +
#   xlab("Condition") + ylab("SourceAcc \n temporal SourceAcc") +
#   labs(fill=Subject_SourceAcc2$Condition) +
#   ggtitle("Auditory source memory performance")
# bxp

head(Subject_SourceAcc_SameContext_Boundary2)
box1 <- ggplot(Subject_SourceAcc_SameContext_Boundary2, 
              aes(x = Condition, y=ratio, fill = Condition)) +
  geom_boxplot(notch = F) +
  scale_fill_brewer(palette="Set2") +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.4) +
  xlab("Condition") + ylab("SourceAcc \n temporal SourceAcc") +
  labs(fill=Subject_SourceAcc2$Condition) +
  ggtitle("SameContext - Boundary")

head(Subject_SourceAcc_First_Last2)
box2 <- ggplot(Subject_SourceAcc_First_Last2, 
              aes(x = Condition, y=ratio, fill = Condition)) +
  geom_boxplot(notch = F) +
  scale_fill_brewer(palette="Set1") +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.4) +
  xlab("Condition") + ylab("SourceAcc \n temporal SourceAcc") +
  labs(fill=Subject_SourceAcc2$Condition) +
  ggtitle("First - Last")
Boxplots <- grid.arrange(box1, box2, ncol = 2, 
                         top = textGrob("Auditory source memory performance"))
Boxplots

####Descriptive statistics####
describe.by(Subject_SourceAcc_SameContext_Boundary2, group = "Condition")
describe.by(Subject_SourceAcc_First_Last2, group = "Condition")
#describe.by(Subject_Distance, group = "Condition")


##### Paired t-tests #####
#Subject_Distance <- rbind(Subject_Distance_SameContext, Subject_Distance_Boundary)

Subject_SourceAcc_SameContext3 <- subset(Subject_SourceAcc_SameContext_Boundary2, 
                                         Condition == "SameContext")
Subject_SourceAcc_Boundary3 <- subset(Subject_SourceAcc_SameContext_Boundary2, 
                                         Condition == "Boundary")
Subject_SourceAcc_First3 <- subset(Subject_SourceAcc_First_Last2, 
                                      Condition == "First")
Subject_SourceAcc_Last3 <- subset(Subject_SourceAcc_First_Last2, 
                         Condition == "Last")

t.test(Subject_SourceAcc_SameContext3$ratio, Subject_SourceAcc_Boundary3$ratio, paired = T)
c1 <- cohen.d(Subject_SourceAcc_SameContext_Boundary2$ratio, Subject_SourceAcc_SameContext_Boundary2$Condition)
c1
d1 <- c1$cohen.d[2]
d1
# pwr.t.test(d=d1,n=40,sig.level=0.05,type="paired",alternative="two.sided")

t.test(Subject_SourceAcc_First3$ratio, Subject_SourceAcc_Last3$ratio, paired = T)
c2 <- cohen.d(Subject_SourceAcc_First_Last2$ratio, Subject_SourceAcc_First_Last2$Condition)
c2
d2 <- c2$cohen.d[2]
d2
##### ANOVA ####

is.factor(Subject_SourceAcc$Condition)
is.factor(Subject_SourceAcc2$Condition)
# Subject_SourceAcc$Condition2 <- factor(Subject_SourceAcc$Condition,
#   
# levels = c(1,2), labels = c("one", "two"))
# levels(Subject_SourceAcc$Condition2)
# Subject_SourceAcc$ConditionFactor <- factor(Subject_SourceAcc$Condition)
# is.factor(Subject_SourceAcc$Condition2)
# 
# factor(Subject_SourceAcc$Condition, levels = c(1, 2),
#           labels = c("SameContext", "Boundary"))

# ANOVA
anova1 <- aov(SourceAcc ~ Condition, data = Subject_SourceAcc)
anova1_summary <- summary(anova1)

anova1_no_outliers <- aov(SourceAcc ~ Condition, data = Subject_SourceAcc2)
anova1_summary_no_outliers <- summary(anova1_no_outliers)

anova1
anova1_no_outliers

anova1_summary
anova1_summary_no_outliers

############
anova_formula <- y ~ w1*w2 + Error(id/(w1*w2))
anova2 <- anova_test(data = Subject_SourceAcc,
                     formula = anova_formula,
                     dv = Subject_SourceAcc$ratio,
                     wid = Subject_SourceAcc$Subject,
                     within = Subject_SourceAcc$Condition)

anova2

anova2_no_outliers <- anova_test(data = Subject_SourceAcc2,
                                 formula = anova_formula,
                                 dv = Subject_SourceAcc2$ratio,
                                 wid = Subject_SourceAcc2$Subject,
                                 within = Subject_SourceAcc2$Condition)
anova2_no_outliers

##### Performing statistical tests #####


