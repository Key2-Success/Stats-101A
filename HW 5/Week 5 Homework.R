# Name:       Kitu Komya (UID: 404-491-375)
# Class:      Statistics 101A (Discussion 3A)
# Assignment: Homework 5 (due: 10/28/16)
  
# Note: I'm concurrently taking Stats 20, so I'm still familiarizing myself with R.

x <- rep(1:100,2)
trend <- 1 + 4*x + 7*x^2
set.seed(234)
e <- rnorm(length(x),0,10000)
y <- trend+e

#1B: fitting a linear model
fit <- lm(y~x) # fits linear model
plot(y~x) # plots scatterplot
abline(fit) # adds least squares linear regression model trendline

# to see residual plot, qq plot, scale-location plot, and residual vs leverage plot
plot(fit)

#1C: fitting a quadratic model
fit2 <- lm(y~x + I(x^2)) # fits quadratic model
plot(y~x + I(x^2)) # plots scatterplot

# 1D: looking at quadratic model's diagnostics
# to see residual plot, qq plot, scale-location plot, and residual vs leverage plot
plot(fit2)

#1E: hypothesis test on B0, B1, and B2
summary(fit2)

#1F: new fit with sigma = 100
x1 <- rep(1:100,2)
trend <- 1 + 4*x1 + 7*x1^2
set.seed(234)
e <- rnorm(length(x1),0,100)
y1 <- trend+e

fit3 <- lm(y1~x1 + I(x1^2)) # fits a quadratic model
plot(y1~x1 + I(x1^2)) # plots scatterplot
summary(fit3) # hypothesis test on B0, B1, and B2
