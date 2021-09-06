rm(list=ls())

## Set the directory path. 
# Paste your path
setwd("C:/Projects/EventSegmentation")

## Library dplyr for the function rbind()
library(dplyr)

## read the content of the folder where the subject's files are
# Paste the folder path in the argument path=""

files <- list.files(path="C:/Projects/EventSegmentation/TemporalMemory_Results", pattern="*.txt", full.names=TRUE, recursive=FALSE)

## Creating a dataframe to store all subjects' data
# Taking the variables names from the first file and adding a new wariable "Subject" to store numbers of subjects
All_Data <- read.table(files[1], header = TRUE)
All_Data$Subject <- NA
All_Data <- All_Data[0,]

## Specify position of the subject number in the string of the file name
# The beggining of the subject number: please specify
SubName_beggining <- 61
# The beggining of the subject number: please specify
SubName_end <- 63

## Loop takes the file of each subject and merges it together in "i" steps 
# in the dataframe "All_Data
# Also, adds subject's number from the file name to the variable "Subject"

# Creating an empty dataframe to store data from each subject in the loop
SubjectData <- data.frame()
for(i in 1:length(files)) {
  SubjectFile <- files[i]
  SubjectNumber <- substring(SubjectFile,SubName_beggining,SubName_end)
  SubjectData <- read.table(SubjectFile, header = TRUE)
  SubjectData$Subject <-rep(c(SubjectNumber),each=nrow(SubjectData))
  All_Data <- rbind(All_Data, SubjectData)
  rm(SubjectData)
}

## Saving merged data in the csv file.
# Please, specify the folder to save the file and the file name in argument below
# Chose different folder than Subjects' data folder!!
write.csv(All_Data,"C:/Projects/EventSegmentation/TemporalMemory_Results_All.csv", row.names = FALSE)
rm(list=ls())