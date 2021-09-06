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


####f1####
# calculate_accuracy_ratio <- function(data_frame) {
#   Subject_calculate_accuracy_ratio <-
#     data_frame %>%
#     group_by(Subject) %>%
#     summarise_at(vars(SourceAcc), 
#                  list(ratio = mean))
#   return(Subject_calculate_accuracy_ratio)
# }


####f1####
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
# Function give_lillie_p() returns the p-value for the Lilliefors normality test
# Arguments:
#    - variable to test for the normal distribution.(variable_to_test)

give_lillie_p <- function(variable_to_test) {
  lillie_test <- lillie.test(variable_to_test)
  lillie_p <- lillie_test$p.value
  return(lillie_p)
}

####f3####
#Function norm_dist_check_color():
#    - performs the Lilliefors test on the indicated variable.
#    - gives a color depending if distribution is normal or not:
#       - NORMAL distribution - green - "darkseagreen1" 
#       - NOT NORMAL distribution - pink - "darksalmon" 
#Arguments:
#    - variable to test for the normal distribution.(variable_to_test)

norm_dist_check_color <- function(variable_to_test) {
  lillie_test <- lillie.test(variable_to_test)
  
  if (lillie_test$p.value > 0.05) {
    col <- "darkseagreen1"
  } else {
    col <- "darksalmon"
  }
}


####f4####
draw_a_hist <- function(hist_data, bw, which_condition, x_label) {
  hist_data <- hist_data
  which_condition <- "SameContext"
  
  hist1 <- hist_data %>%
    ggplot(aes(x=ratio), stat="count") +
    ylim(0,7) + 
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