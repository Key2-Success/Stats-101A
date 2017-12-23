# Name:       Kitu Komya (UID: 404-491-375)
# Class:      Statistics 101A (Discussion 3A)
# Assignment: Homework 6 (due: 11/04/16)

# Note: I'm concurrently taking Stats 20, so I'm still familiarizing myself with R.

#1: PART A: A
#I'm loading in the dataset

AdRevenue <- read.csv(file = "Kitu/College/Junior Year/Fall Quarter/Stats 101A/Homework/Week 6/AdRevenue.csv")

#linear model (no transformation)
fit1 <- lm(AdRevenue~Circulation, AdRevenue) # fits a linear model
plot(AdRevenue~Circulation, AdRevenue) # plots scatterplot
abline(fit1) # fits best model trendline
plot(fit1) # shows all diagnostics

#transform response variables
fit2 <- lm(log(AdRevenue)~Circulation, AdRevenue) # fits a linear model
plot(log(AdRevenue)~Circulation, AdRevenue) # plots scatterplot
plot(fit2) # shows all diagnostics

#transform predictor variable
fit3 <- lm(AdRevenue~log(Circulation), AdRevenue) # fits a linear model
plot(AdRevenue~log(Circulation), AdRevenue) # plots scatterplot
plot(fit3) # shows all diagnostics

#transform both variables
fit4 <- lm(log(AdRevenue)~log(Circulation), AdRevenue) # fits a linear model
plot(log(AdRevenue)~log(Circulation), AdRevenue) # plots scatterplot
plot(fit4) # shows all diagnostics
summary(fit4) # will ouput the equation of the model

#1: PART A: B
predict(fit4, data.frame(Circulation = 0.5), interval = "prediction", level = 0.95) # will predict ad revenue
# for an individual observation of 0.5 million copies in circulation at a 95% prediction interval

predict(fit4, data.frame(Circulation = 20), interval = "prediction", level = 0.95) # will predict ad revenue
# for an individual observation of 0.5 million copies in circulation at a 95% prediction interval

#1: PART B: A
fit5 <- lm(AdRevenue~Circulation + I(Circulation^2), AdRevenue) # fits quadratic model
fit6 <- lm(AdRevenue~Circulation + I(Circulation^2) + I(Circulation^3), AdRevenue) # fits cubic model
summary(fit5) # provides equation of quadratic model
summary(fit6) # provides equation of cubic model
plot(fit5) # plots diagnostics of quadratic model
plot(fit6) # Plots diagnostics of cubic model

#1: PART B: B
predict(fit6, data.frame(Circulation = 0.5), interval = "prediction", level = 0.95) # provides a 95% prediction interval of Ad Revenue for the cubic model at 0.5 million copies of Circulation
predict(fit6, data.frame(Circulation = 20), interval = "prediction", level = 0.95)  # provides a 95% prediction interval of Ad Revenue for the cubic model at 20 million copies of Circulation
plot(fit6) # plots diagnostics of cubic model

#2: PART 1: A
diamonds <- read.table(file = "Kitu/College/Junior Year/Fall Quarter/Stats 101A/Homework/Week 6/diamonds.txt", header = T) # I am reading in the data
fit7 <- lm(Price~Size, diamonds) # fits a linear model
plot(Price~Size, diamonds) # plots a scatterplot
plot(fit7) # plots diagnostics and r-squared value
summary(fit7) # provides equation in summary and r-squared value

fit8 <- lm(log(Price)~log(Size), diamonds) # fits a log transform on both variables
plot(log(Price)~log(Size), diamonds) # plots a scatterplot
plot(fit8) # plots diagnostics
summary(fit8) # provides equation in summary

#3: I have read in the data
cars <- read.csv(file = "Kitu/College/Junior Year/Fall Quarter/Stats 101A/Homework/Week 6/cars04.csv")

#3A
plot(cars) # plots a scatterplot matrix
fit9 <- lm(SuggestedRetailPrice~DealerCost, cars) # fits a linear model
plot(SuggestedRetailPrice~DealerCost, cars) # plots a scatterplot
summary(fit9) # provides equation in summary
confint(fit9) # provides confidence intervals of estimates

#3B
plot(fit9) # plots diagnostics

#3C
fit10 <- lm(SuggestedRetailPrice~Hybrid+DealerCost+EngineSize+Cylinders+Horsepower+CityMPG+HighwayMPG+Weight+WheelBase+Length+Width, cars)
summary(fit10) # shows significance of each variable
