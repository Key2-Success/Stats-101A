# Name:       Kitu Komya (UID: 404-491-375)
# Class:      Statistics 101A (Discussion 3A)
# Assignment: Homework 4 (due: 10/20/16)
---
  
# Note: I'm concurrently taking Stats 20, so I'm still familiarizing myself with R.

#QE I am reading in the data
housescrapeWW1 = read.table(file = "Kitu/College/Junior Year/Fall Quarter/Stats 101A/Homework/Week 4/housescrapeWW1.txt")

# I have subsetted the dataframe to only include those whose sqft is larger than 0
df <- housescrapeWW1[housescrapeWW1$sqft > 0, ]

#QE (a)
fit <- lm(price~sqft, df) # creates a best fit model linear trendline
summary(fit) # will provide the output of the linear equation

#QE (b)
fit2 <- lm(log(price)~log(sqft), df) # creates a best fit model logarithmic trendline
summary(fit2)

#QE (c)
fit3 <- lm(log(price)~sqft, df)
summary(fit3)

#QE (d)
# model a
plot(price~sqft, df) # plots model 1
abline(fit) # adds regression line
res1 = resid(fit) # saves residual
plot(df$sqft, res1) # plots residual
plot(fit) # to look at QQ plot

# model b
plot(log(price)~log(sqft), df) # plots model 2
abline(fit2) # adds regression line
res2 = resid(fit2) # saves residual
plot(log(df$sqft), res2) # plots residual
plot(fit2) # to look at QQ plot

# model c
plot(log(price)~sqft, df) # plots model 3
abline(fit3) # adds regression line
res3 = resid(fit3) # saves residual
plot(log(df$sqft), res3) # plots residual
plot(fit3) # to look at QQ plot
