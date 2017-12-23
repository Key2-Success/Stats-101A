# Name:       Kitu Komya (UID: 404-491-375)
# Class:      Statistics 101A (Discussion 3A)
# Assignment: Homework 9 (due: 12/02/16)

# Note: I'm concurrently taking Stats 20, so I'm still familiarizing myself with R.

# 2a. I am reading in the data
nc <- read.csv(file = "Kitu/College/Junior Year/Fall Quarter/Stats 101A/Homework/Week 9/NCbirths.csv")
fit1 <- lm(pounds~sex+fage+mage+meduc+weeks+visits+cignum+gained+smoke, data = nc) # fits a linear model
summary(fit1) # provides model

# 2b.
library(alr3)
summary(powerTransform(pounds~sex+fage+mage+meduc+weeks+visits+cignum+gained+smoke, data = nc)) # box-cox transformation


# 2c.
invResPlot(fit1) # inverse reverse plot
library(leaps)
bkwd <- regsubsets(pounds~sex+fage+mage+meduc+weeks+visits+cignum+gained+smoke, data = nc, method = "backward")
bic <- summary(bkwd)$bic
p <- length(bic)
plot(1:p, bic)
lines(1:p, bic)
summary(bkwd)

best <- lm(pounds~mage+weeks, data = nc)
summary(best)


# 2d
vif(best)

# 2e.
plot(best)
