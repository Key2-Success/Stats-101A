# Name:       Kitu Komya (UID: 404-491-375)
# Class:      Statistics 101A (Discussion 3A)
# Assignment: Homework 8 (due: 11/18/16)

# Note: I'm concurrently taking Stats 20, so I'm still familiarizing myself with R.

# A: I am uploading the data
realty <- read.delim(file = "Kitu/College/Junior Year/Fall Quarter/Stats 101A/Homework/Week 8/realty.txt")

# we see the types of homes
table(realty$type)

# we are only keeping the condos and single family residence homes
new.frame <- subset(realty, type== "Condo/Twh" | type == "SFR")

# eliminate houses with no square footage or bathrooms
new2.frame <- subset(new.frame, sqft>0 & bath>0)

# log transform
realty.new <- transform(new2.frame, lprice=log(price))

# Ai: fitting a log model with all variables as asked
fit <- lm(log(price)~city+bed+bath+sqft, data = realty.new)
summary(fit) # f partial test

# Aii: to double check the validity of my claims, I made a table using pipes 
#to calculate the average cost of homes in each city (not required, but used to double-check)
library(dplyr)
library(knitr)
realty.new %>%
  group_by(city) %>%
  summarize(mean_price = mean(price)) %>%
  kable()

# Av: fitting a log model with all but bedroom variable
fit2 <- lm(log(price)~city+bath+sqft, data = realty.new)
summary(fit2) # f partial test

# Avi: lattice for bed and bath vs log(price)
library(lattice)
cloud(log(price)~bath+bed, data = realty.new) # cloud
xyplot(log(price)~bath|bed, data = realty.new) # xy plot

# Avii: lattice for sqft and city vs log(price)
xyplot(log(price)~sqft|city, data = realty.new) # xy plot

# Ba: read in dataset
golf <- read.csv(file = "Kitu/College/Junior Year/Fall Quarter/Stats 101A/Homework/Week 8/pgatour2006.csv")
library(alr3)
fit3 <- lm(log(PrizeMoney)~AveDrivingDistance+DrivingAccuracy+GIR+PuttingAverage+BirdieConversion+SandSaves+Scrambling+BounceBack+PuttsPerRound, data = golf) # log model fitted
mmps(fit3) # mmp plots

#Bb: full regression model
summary(fit3) # will give model
plot(fit3) # will plot diagnostics
