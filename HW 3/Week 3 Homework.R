# Name:       Kitu Komya (UID: 404-491-375)
# Class:      Statistics 101A (Discussion 3A)
# Assignment: Homework 3 (due: 10/14/16)
---
  
# Note: I'm concurrently taking Stats 20, so I'm still familiarizing myself with R.
  
# I'm reading in the armspan data
armspan <- read.csv(file = "Kitu/College/Junior Year/Fall Quarter/Stats 101A/Data/armspan.recent.csv")

# 1
# I'm creating a data frame to clean that data. It will only select data with at least 30 inches 
# of an armspan. This will eliminate any bad data, such as unusually low inches that result 
# from typos or from improper units, such as feet instead of inches. We are left with 129 observations
# from the original 134 observations.
armspan.df <- subset(armspan, armspan > 30)

# 1A
plot(armspan.df$height, armspan.df$armspan) # plots the scatterplot
my.model = lm(armspan~height, armspan.df) # creates a best fit linear model trendline
abline(my.model) # adds the best fit linear model trendline
summary(my.model) # will provide summary of our data, including equation of linear model

# 1B
residuals = resid(my.model) # saves residuals of the data
plot(armspan.df$height, residuals) # plots height versus residuals

# 1C
confint(my.model) # gives 95% confidence interval for model's y-intercept and slope

# 1D
predict(my.model, data.frame(height = 76), interval = "prediction", level = 0.95) # will predict Phelps'
#individual armspan at a 95% prediction interval

# 1E
predict(my.model, data.frame(height = 76), interval = "confidence", level = 0.95) # will predict the mean 
#armspan of all individuals with a height of 76 inches at a 95% confidence interval

#2
# I'm reading in the playbill data
playbill <- read.csv(file = "Kitu/College/Junior Year/Fall Quarter/Stats 101A/Data/playbill.csv")

# 2A
my.model2 = lm(CurrentWeek~LastWeek, playbill) # creates a best fit linear model trendline
confint(my.model2) # gives 95% confidence interval for model's y-intercept and slope

# 2B
summary(my.model2) # use to find the estimate y-intercept and standard error of y-intercept
tb0 = abs((10000-6805)/9929) # calculates absolute value of t-value. 10000 is what we're testing against,
#6805 is the estimate y-intercept, and 9929 is the standard error, the last two found from the summary from above
p <- pt(tb0, 17, lower.tail = T) # calculates p-value using 17 as degrees of freedom for a
#two-tailed hypothesis testing
p # the p-value outputs to be 0.6242306

# 2C
predict(my.model2, data.frame(LastWeek = 400000), interval = "prediction", level = 0.95) # will predict
#the individual gross box office results for the current week at a 95% prediction interval

# 2D
t.test(x = playbill$LastWeek, y = playbill$CurrentWeek, mu = 0, alternative = "two.sided") # will output p-value
#of the probability that there is no difference between the gross box office returns of Last and Current Weeks
summary(my.model2) # use to see that the estimate for slope is 0.9821

# 3
# I'm reading in the indicators data from the website
indicators <- read.table("http://www.stat.tamu.edu/~sheather/book/docs/datasets/indicators.txt", header = T)

# 3A
plot(indicators$LoanPaymentsOverdue, indicators$PriceChange) # creates a scatterplot of the data (unrequired)
my.model3 = lm(PriceChange~LoanPaymentsOverdue, indicators) # creates a best fit linear model trendline (unrequired)
abline(my.model3) # adds best fit linear model trend onto scatterplot (unrequired)
confint(my.model3) # gives 95% confidence interval for y-intercept and slope

# 3B
predict(my.model3, data.frame(LoanPaymentsOverdue = 4), interval = "confidence", level = 0.95) # will predict
#the mean percentage change in all average prices from 7/2006 to 7/2007 at a 95% confidence interval

# 4
# I'm reading in the invoices data from the website
invoices <- read.table("http://www.stat.tamu.edu/~sheather/book/docs/datasets/invoices.txt", header = T)

# 4A
my.model4 = lm(Time~Invoices, invoices) # creates a best fit linear model trendline
confint(my.model4) # gives 95% confidence interval for y-intercept and slope

# 4B
summary(my.model4) # use to find the estimate slope and standard error of slope
tb1 = abs((0.01-0.0112916)/0.0008184) # calculates absolute value of t-value. 0.01 is what we're testing against,
#0.0112916 is the estimate slope, and 0.0008184 is the standard error, the last two found from the summary from above
q <- pt(tb1, 29, lower.tail = T) # calculates p-value using 29 as degrees of freedom for a
#two-tailed hypothesis testing
q # the p-value outputs to be 0.9373167

# 4C
predict(my.model4, data.frame(Invoices = 130), interval = "prediction", level = 0.95) # will predict
#the individual time to process 130 invoices at a 95% prediction interval

# 5A
x1 <- 1:20 # assigning values to x1 as requested
y1 <- 4 + 3*x1 + rnorm(20, 0, 5) # assigning values to y1 as requested
m1 <- lm(y1~x1) # model created as requested

x2 <- c(11:20, 11:20) # assigning values to x2 as requested
y2 <- 4 + 3*x2 + rnorm(20, 0, 5) # assigning values to y2 as requested
m2 <- lm(y2~x2) # model created as requested

plot(x1, y1) # can confirm that situation 1 has a more precise linear model best fit trendline
abline(m1) # adds a linear model best fit trendline

plot(x2, y2) # can confirm that sitation 2 has a scattered and thus less precise linear model best fit trendline
abline(m2) # adds a linear model best fit trendline

# 5B
confint(m1) # gives 95% confidence interval for y-intercept and slope
confint(m2) # gives 95% confidence interval for y-intercept and slope
