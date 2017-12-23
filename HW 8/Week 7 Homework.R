# Name:       Kitu Komya (UID: 404-491-375)
# Class:      Statistics 101A (Discussion 3A)
# Assignment: Homework 6 (due: 11/04/16)

# Note: I'm concurrently taking Stats 20, so I'm still familiarizing myself with R.


# 1A: I am reading in the data
wwh <- read.table(file = "Kitu/College/Junior Year/Fall Quarter/Stats 101A/Homework/Week 7/waistweightheight.txt", header = T)
fit1 <- lm(Weight~Waist+Height, data = wwh) # fits a multiple variable regression model
summary(fit1) # will show summary statistics

# 1B
set.seed(23) # so we can all use same data
new.df <- transform(wwh, worthless = rnorm(dim(wwh)[1],0,5)) # added a worthless variable
fit2 <- lm(Weight~Waist+Height+worthless, data = new.df) # fits a mulitple variable regression model
summary(fit2) # will show summary statistics

# 1C
set.seed(23) # so we can all use same data
new.df <- transform(wwh, worthless = rnorm(dim(wwh)[1],0,5)) # added a worthless variable
fit3 <- lm(Weight~worthless+Waist+Height, data = new.df) # fits a mulitple variable regression model
summary(fit3) # will show summary statistics

# 2A & 2B: I am reading in the data
cars <- read.csv(file = "Kitu/College/Junior Year/Fall Quarter/Stats 101A/Homework/Week 7/cars04.csv")
fit3 <- lm(SuggestedRetailPrice~DealerCost+EngineSize+Cylinders+Horsepower+CityMPG+HighwayMPG+Weight+WheelBase+Length+Width, cars)
summary(fit3) # will show summary statistics

# 2C & 2D
anova(lm(SuggestedRetailPrice~DealerCost+EngineSize+Horsepower+CityMPG+HighwayMPG+Weight+WheelBase+Length+Width+Cylinders, data = cars)) # creates an anova table

# 2E
fit4 <- lm(SuggestedRetailPrice~DealerCost+EngineSize+Cylinders+Horsepower+CityMPG+HighwayMPG+Weight+WheelBase+Length+Width, cars) # fits the full model
summary(fit4) # shows summary statistics

fit5 <- lm(SuggestedRetailPrice~DealerCost+EngineSize+Cylinders+Horsepower+Weight+WheelBase+Length+Width, cars) # fits the model without fuel consumption variables
summary(fit5) # shows summary statistics