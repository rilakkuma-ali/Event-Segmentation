#=======Clear environment=======================================================
cat("\f")
rm(list=ls())
dev.off()
#=======Directory, functions file===============================================
# Set the directory path # Paste your path
setwd("C:/Projects/Event-Segmentation")
# Functions file:
source("Functions_.R")

#=======Data====================================================================
# AllData <- read.csv("C:/Projects/Event-Segmentation/data_Clewett/Clewett_source_Exp3.csv", dec=",")
AllData <- read.csv("C:/Projects/Event-Segmentation/data_RUB/TemporalMemory_Results_All.csv")
#=======Variables===============================================================
MyVariable <- "RecencyAcc" #variable to test
GrVariable <- "Condition" #grouping variable

ExpConditions <- c(unique(AllData$Condition)) #List of conditions
ExpConditions
SameContext <- subset.data.frame(AllData, Condition=="SameContext")
Boundary <- subset.data.frame(AllData, Condition=="Boundary")
#=====================

# set.seed(20190417)                   ### set seed for replicability
# len.pi <- 1001L                      ### number of candidate values for pi
# pi <- seq(0, 1, length.out = len.pi) ### candidate values for pi
# a <- b <- 5                          ### hyperparameters
# n <- 200                             ### num. of coin flips
# pi_true <- .8                        ### true parameter
# data <- rbinom(n, 1, pi_true)        ### n coin flips
# posterior <- matrix(NA, 3L, n)       ### matrix container for posterior


# # set.seed(20190417)                   ### set seed for replicability
# len.pi <- 1001L                      ### number of candidate values for pi
# pi <- seq(0, 1, length.out = len.pi) ### candidate values for pi
# a <- b <- 5                          ### hyperparameters
# n <- nrow(AllData)                             ### num. of coin flips
# pi_true <- .5                        ### true parameter
# data <- c(AllData$RecencyAcc)       ### n coin flips
# posterior <- matrix(NA, 3L, n)       ### matrix container for posterior

# # set.seed(20190417)                   ### set seed for replicability
# len.pi <- 1001L                      ### number of candidate values for pi
# pi <- seq(0, 1, length.out = len.pi) ### candidate values for pi
# a <- b <- 5                          ### hyperparameters
# n <- nrow(SameContext)                             ### num. of coin flips
# pi_true <- .5                        ### true parameter
# data <- c(SameContext$RecencyAcc)       ### n coin flips
# posterior <- matrix(NA, 3L, n)       ### matrix container for posterior

# set.seed(20190417)                   ### set seed for replicability
len.pi <- 1001L                      ### number of candidate values for pi
pi <- seq(0, 1, length.out = len.pi) ### candidate values for pi
a <- b <- 5                          ### hyperparameters
n <- nrow(Boundary)                             ### num. of coin flips
pi_true <- .5                        ### true parameter
data <- c(Boundary$RecencyAcc)       ### n coin flips
posterior <- matrix(NA, 3L, n)       ### matrix container for posterior

for (i in seq_len(n)) {    
  current.sequence <- data[1:i]      ### sequence up until ith draw
  k <- sum(current.sequence)         ### number of heads in current sequence
  
  ##### Updating
  a.prime <- a + k               
  b.prime <- b + i - k
  
  ### Analytical means and credible intervals
  posterior[1, i] <- a.prime / (a.prime + b.prime)
  posterior[2, i] <- qbeta(0.025, a.prime, b.prime)
  posterior[3, i] <- qbeta(0.975, a.prime, b.prime)
}

## Plot
plot(                                ### set up empty plot with labels
  1:n, 1:n,
  type = 'n',
  xlab = "Number of Coin Flips",
  ylab = expression(
    paste(
      "Posterior Means of ",
      pi, " (with 95% Credible Intervals)",
      sep = " "
    )
  ),
  ylim = c(0, 1),
  xlim = c(1, n)
)
abline(                              ### reference line for the true pi
  h = c(.5, .8),
  col = "gray80"
)
rect(-.5, qbeta(0.025, 5, 5),        ### prior mean + interval at i = 0
     0.5, qbeta(0.975, 5, 5),
     col = adjustcolor('red', .4),
     border = adjustcolor('red', .2))
segments(-.5, .5,
         0.5, .5,
         col = adjustcolor('red', .9),
         lwd = 1.5)
polygon(                             ### posterior means + intervals
  c(seq_len(n), rev(seq_len(n))),
  c(posterior[2, ], rev(posterior[3, ])),
  col = adjustcolor('blue', .4),
  border = adjustcolor('blue', .2)
)
lines(
  seq_len(n),
  posterior[1, ],
  col = adjustcolor('blue', .9),
  lwd = 1.5
)

