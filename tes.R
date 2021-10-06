#=====================
library(greta)
library(rethinking)
rm(list=ls())
dev.off()
#=====================
# Likelihood
rangeP <- seq(0, 1, length.out = 100)
plot(rangeP, dbinom(x = 8, prob = rangeP, size = 10),
     type = "o", col = "blue", xlab = "P(Black)", ylab = "Density")

#Prior
lines(rangeP, dnorm(x = rangeP, mean = .5, sd = .1) / 15,
      col = "red")
lik <- dbinom(x = 8, prob = rangeP, size = 10)
prior <- dnorm(x = rangeP, mean = .5, sd = .1)
lines(rangeP, lik * prior, col = "green")


unstdPost <- lik * prior
unstdPost
sum(unstdPost)
stdPost <- unstdPost / sum(unstdPost)
stdPost
lines(rangeP, stdPost, col = "blue")
legend("topleft", legend = c("Lik", "Prior", "Unstd Post", "Post"),
       text.col = 1:4, bty = "n")
