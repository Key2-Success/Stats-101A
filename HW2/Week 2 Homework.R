# Name:       Kitu Komya (UID: 404-491-375)
# Class:      Statistics 101A (Discussion 3A)
# Assignment: Homework 1 (due: 10/07/16)
---

# Note: I'm concurrently taking Stats 20, so I'm still familiarizing myself with R.

# I'm reading in the armspan data
armspan <- read.csv("Kitu/College/Junior Year/Fall Quarter/Stats 101A/Data/armspan1.csv")

# I'm creating a data frame to clean that data. It will only select data with at least 30 inches 
# of an armspan. This will eliminate any bad data, such as unusually low inches that result 
# from typos or from improper units, such as feet instead of inches. We are left with 129 observations
# from the original 134 observations.
armspan.df <- subset(armspan, armspan > 30)

# 1A
mydata = plot(armspan.df$height, armspan.df$armspan) # plots the scatterplot

#1B
my.model = lm(armspan.df$armspan ~ armspan.df$height) # creates a best fit linear model trendline
line = abline(my.model) # adds the best fit linear model trendline
summary(my.model) # will provide summary of our data, including equation of linear model
  
#1C
pred.arm <- 1.00049*(65) - 0.63014 # finding my predicted armspan from the equation
res <- 64 - 64.40171 # finding my residual from the equation

#1D
pred.mp <- 79 - (1.00049*(76) - 0.63014) # finding the residual of Michael Phelps' using two equations

#1E
residuals = resid(my.model) # saves residuals of the data
plot(armspan.df$height, residuals) # plots height versus residuals

#2A (reading in data as requested)
x <- 1:20
y <- 1 + 3*x + 3*x^2 + rnorm(20,0,5)
df <- data.frame(x,y)

plot(x,y) # plots scatterplot

m1 <- lm(y~x, df)
abline(m1) # adds linear model trendline

#2C
residuals1 = resid(m1) # saves residuals of the data
plot(df$x, residuals1) # plots x versus residuals

#3A
atus <- read.csv("Kitu/College/Junior Year/Fall Quarter/Stats 101A/Data/atus.csv") # imports data
atus1 <- subset(atus,homework>0) # subsets to those who actually did spend time on homework
plot(atus1$homework, atus1$sleep) # plots homework vs sleep

m3 <- lm(atus1$sleep ~ atus1$homework) # creates a best fit linear model trendline
abline(m3) # adds a best fit linear model trendline
summary(m3) # provides summary of data

#3B
residuals2 = resid(m3) # saves residuals of the data
plot(atus1$homework, residuals2) # plots homework vs residuals

