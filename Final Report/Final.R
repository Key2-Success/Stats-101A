# Name:       Kitu Komya (UID: 404-491-375)
# Class:      Statistics 101A (Discussion 3A)
# Assignment: Final Report (due: 12/06/16)

# Note: I'm concurrently taking Stats 20, so I'm still familiarizing myself with R.


# 1: I have read in the data
wsj <- read.csv(file = "Kitu/College/Junior Year/Fall Quarter/Stats 101A/Homework/Final Report/wsj3.csv")

# 2b: fitting model
fit <- lm(DEFAULT_RATE~NETPRICE+INSTITUTION_GROUP, data = wsj)
summary(fit)

# 2d: confidence interval
predict(fit, data.frame(NETPRICE = 15000, INSTITUTION_GROUP = "bachelor"), interval = "confidence", level = 0.95)

library(dplyr)
library(knitr)
wsj %>%
  group_by(INSTITUTION_GROUP) %>% 
  filter(INSTITUTION_GROUP == "bachelor") %>% 
  summarize(mean(!is.na(DEFAULT_RATE))) %>% 
  kable()

# 3a: vif
fit2 <- lm(DEFAULT_RATE~MEDIAN_BORROWING+MEDIAN_PAYMENT+NETPRICE+GRAD_RATE, data = wsj)
library(alr3)
vif(fit2)

# 4a: model
fit3 <- lm(DEFAULT_RATE~MEDIAN_BORROWING+NETPRICE+GRAD_RATE, data = wsj)
vif(fit3)

# 4b: mmp
mmps(fit3)
fit4 <- lm(sqrt(DEFAULT_RATE)~MEDIAN_BORROWING+NETPRICE+GRAD_RATE, data = wsj)
mmps(fit4)

# 5a: fitting model
fit5 <- lm(DEFAULT_RATE+10~NETPRICE+GRAD_RATE+MEDIAN_BORROWING, data = wsj)
plot(fit5)
summary(fit5)
vif(fit5)
mmps(fit5)

# 5b: better model
library(MASS)
boxcox(fit5)
summary(powerTransform(fit5))

# 5c: fit model
fit6 <- lm(log(DEFAULT_RATE+10)~NETPRICE+GRAD_RATE+MEDIAN_BORROWING, data = wsj)
mmps(fit6)
summary(fit6)
vif(fit6)
plot(fit6)
