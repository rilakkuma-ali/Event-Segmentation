####lib####
library(dplyr) #yes!
library(nortest)
library(effsize)
library(ggplot2) #yes!
library(tidyverse)
library(hrbrthemes)
library(car)
library(rstatix)
library(ggpubr)
library(ggstatsplot)
library(grid) #yes!
library(gridExtra)
library(pwr)
library(psych)
library(ggstatsplot)


#====calculate_accuracy_ratio()=================================================
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


#====create_MyAllData()=========================================================
# Function create_MyAllData() takes the input dataframe, 
# calculates ratio, 
# and creates the output dataframe with 3 variables
create_MyAllData <- function(AllData) {
  MyAllData <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(MyAllData) <- c(Subject, "ratio", GrVariable)
  
  for (i in 1:length(ExpConditions)) {
    tmp <- calculate_accuracy_ratio(AllData, MyVariable, ExpConditions[i])
    MyAllData <- rbind(MyAllData, tmp)
  }
  return(MyAllData)
}

#====give_lillie_p()============================================================
# Function give_lillie_p() returns the p-value for the Lilliefors normality test
# Arguments:
#    - variable to test for the normal distribution.(variable_to_test)

give_lillie_p <- function(variable_to_test) {
  lillie_test <- lillie.test(variable_to_test)
  lillie_p <- lillie_test$p.value
  return(lillie_p)
}

#====norm_dist_check_color()====================================================
# Function norm_dist_check_color():
#    - performs the Lilliefors test on the indicated variable.
#    - gives a color depending if distribution is normal or not:
#       - NORMAL distribution - green - "darkseagreen1" 
#       - NOT NORMAL distribution - pink - "darksalmon" 
# Arguments:
#    - variable to test for the normal distribution.(variable_to_test)

norm_dist_check_color <- function(variable_to_test) {
  lillie_test <- lillie.test(variable_to_test)
  
  if (lillie_test$p.value > 0.05) {
    col <- "darkseagreen1"
  } else {
    col <- "darksalmon"
  }
}


#====draw_a_hist()==============================================================

# Function draw_a_hist(hist_data, bw, which_condition, x_label, y_limit)
#   - creates a histogram 
#   - bar colour depands on normal distribution
#     - green: normal
#     - pink: NOT normal
#   - Title: 
#       - experimental condition
#       - n
#       - Lillefors p-value
#       - mean
#   Arguments:
#     - dataframe - with calculated ratios (hist_data, ..., ..., ..., ...)
#     - bin width (..., bw, ..., ..., ...)
#     - specify the name of experimental condition (..., ..., "First", ..., ...)
#     - the label for the x-axis (..., ..., ..., x_label, ...)
#     - limit (hight) of the y-axis (..., ..., ..., ..., y_limit)

draw_a_hist <- function(hist_data, bw, which_condition, x_label, y_limit) {
#    hist <- hist_data %>%
    hist_data %>%
    ggplot(aes(x=ratio), stat="count") +
    ylim(y_limit) + 
    xlab(x_label) + ylab("Count") +
    geom_histogram(binwidth=bw, 
                   fill=norm_dist_check_color(hist_data$ratio),
                   color="black", alpha=0.5) +
    geom_vline(aes(xintercept=mean(ratio)),
               color="black", linetype="dashed", size=1) +
    # geom_text(aes(x=(mean(hist_data$ratio)+0.1), y=6.8,
    #               label=paste("Mean:\n", round(mean(hist_data$ratio),2)))) +
    ggtitle(paste("Condition: ", which_condition,
                  "\nn: ", nrow(hist_data),
                  "\nLilliefors normality test p-value: ", 
                  round(give_lillie_p(hist_data$ratio), 4),
                  "\nMean: ", round(mean(hist_data$ratio),4),
                  sep = "")) +
    theme(plot.title = element_text(size = 8, face = "bold"))
}


#====draw_my_boxplot()==========================================================

draw_my_boxplot <-function(box_data, y_label, boxpl_titel) {
  head(box_data)
  ggplot(box_data,
                aes(x = Condition, y = ratio, fill = Condition)) +
    geom_boxplot(notch = F,
                 outlier.shape=16, outlier.color = "red", outlier.size = 2) +
    stat_summary(
      aes(label = round(stat(y), 1)),
      geom = "text",
      fun.y = function(y) { o <- boxplot.stats(y)$out; if(length(o) == 0) NA else o },
      hjust = -1) +
    scale_fill_brewer(palette="Set2") +
    geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.4) +
    xlab("Condition") + ylab(y_label) +
    labs() +
    ggtitle(boxpl_titel)
}


# Boxplot - violinplot
# https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggbetweenstats.html
draw_my_boxplot2 <-function(box_data, y_label, boxpl_titel) {
  head(box_data)
  ggbetweenstats(data = box_data, 
                 plot.type = "box",
                 x = Condition,
                 y = ratio,
                 ylab = box_ylabel,
                 outlier.tagging = TRUE,
                 outlier.label = Subject,
                 package = "yarrr",
                 palette = "basel",
                 title = box_titel)
}

#====check_name_outliers()======================================================

# check_name_outliers <- function(data_to_check) {
#   outliers <- data_to_check %>%
#     group_by(Condition) %>%
#     identify_outliers(ratio)
#   
#   outliers_subjects <- unique(c(outliers$Subject))
#   are_any_outliers <- !is.null(outliers_subjects)
#   if (are_any_outliers == F) {
#     outliers_message <- "No outliers found"
#   } else {
#     outliers_message <- paste("OUTLIERS: Removed ", length(outliers_subjects), " subject(s) from the data. Subject(s) numbers: ", outliers_subjects, sep="")
#   }
#   return(c(are_any_outliers, length(outliers_subjects), outliers_subjects, outliers_message))
# }
check_name_outliers <- function(data_to_check) {
  outliers <- data_to_check %>%
    group_by(Condition) %>%
    identify_outliers(ratio)
  
  outliers_subjects <- unique(c(outliers$Subject))
  return(outliers_subjects)
}

#=====are_any_outliers <- !is.null(outliers_subjects)
are_any_outliers <- function(outliers_subjects){
  !is.null(outliers_subjects)
  }
  
#=====remove_outliers()=========================================================
remove_outliers <- function(data_to_check, list_of_outliers) {
  for(i in 1:length(list_of_outliers)) {
    data_to_check <- subset(data_to_check, Subject != (list_of_outliers[i]))}
  return((data_to_check))
}

#====split_time_series()========================================================
split_time_series <- function(dat, variab, var_names, conditions) {
  
  dati <- data.frame(matrix(ncol = length(var_names), nrow =nrow(dat)))
  dato <- data.frame(matrix(ncol = length(var_names), nrow =0))
  for (i in 1:length(variab)) {
    #    var_names <- c("Subject", "Experiment", "ratio", "Condition")
    funny_var <- variab[i]
    # print(funny_var)
    dati[1] <- dat$Subject
    dati[2] <- dat$Experiment
    index_i <- which(names(dat) == funny_var)
    varaible_to_paste <- as.vector(dat[index_i])
    # colnames(varaible_to_paste) <- c("ratio")
    # colnames(dati) <- var_names
    dati[3] <- varaible_to_paste
    dati[4] <- var_names[i]
    colnames(dati) <- conditions
    dato <- rbind(dato, dati)
    print(names(dati))
  }
  # dato <- subset(dato, dato$ratio != ".")
  return(dato)
}