## HEADER ####
## Who: JABEN BAKO (213221)
## what: C7081 Assessment
## Last edited: 20222-11-22
####

## CONTENTS ####
## Setup
## Exploratory Data Analysis
## Regression
## Decision Tree
## Conclusion

## Setup ####
library(openxlsx)
library(ggplot2)
library(car)
library(carData)
company <- read.xlsx("C:/Users/hp/Desktop/My R/company.xlsx")
setwd("C:/Users/hp/Desktop/My R")

## Exploratory Data Analysis ####
names(company)
head(company)
summary(company)

# According to the data summary, TV advertisement seem to cost more yet, more efficient.
## Regression analysis ####
# lm.fit for Sales with TV as Predictor
lm.fit <- lm(Sales ~ TV, data = company)
lm.fit
# Dist = 6.97 + 0.06*TV
summary(lm.fit) 
# R squared of 0.8122 is clear indication that model is good/fitting
# The adjusted R squared 0.8112 can be said that 81% of the variance in the data is explained in the model
# The coefficient intercept is (6.97) and TV (0.06). 

## Corelation between Sales and TV advertisement
cor(company$Sales, company$TV) 
# 0.9012079 signifies a strong relationship between TV advertisement and company's sales

## predict 
predict(lm.fit, data.frame(TV = ( c(5, 10, 15) )),
        interval = "confidence") # The 95% confidence interval associated with TV value of 10 is (6.93, 8.13)

predict(lm.fit, data.frame(TV = ( c(5, 10, 15) )),
        interval = "prediction") # The 95% prediction interval associated with TV value of 10 is (2.96, 12.1)

# Graphing
plot(y = company$Sales, x = company$TV,
     ylab = "Sales", xlab = "TV",
     main = "Jaben's Company Regression Plot",
     pch = 2, col = "blue", cex = 1)  # it looks really strong as TV advert increases, sales increase
abline(lm.fit, lwd = 3, col = "red",
       text(x = 3,
            y = 11,
            labels = "y = 6.98 + (0.56)"), )
# There is linearity in Sales response to TV advertisement.

## Make a new histogram
hist(residuals(lm.fit),
     xlim = c(-7, 7), ylim = c(0,.25),
     main = "",
     prob = T)
lines(
  density(residuals(lm.fit)),
  col = "green3", lty = 1, lwd = 3)
x <- seq(-1, +1, by=0.02)
# Draw on theoretical Gaussian for our residual parameters
curve(dnorm(x, mean = mean(residuals(lm.fit)),
            sd = sd(residuals(lm.fit))),
      add = T,
      col = "blue", lty = 3, lwd = 3)
# Expected mean 
abline(v = 0, # vertical line at the EXPECTED resid. mean = 0
       Freq = F,
       col = "red", lty = 2, lwd = 3)

# Near the mean, our residual density dropped and raised and the distribution is mostly symmetrical around the mean.
# Between -2.4 and -4.5 and  also between 2 and 3m our residual density is lower than the expected under theoretical Gaussian.


## Diagnostic Plot
par(mfrow = c(1, 3))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(residuals(lm.fit), residuals(lm.fit))

par(mfrow = c(1, 1))
plot(hatvalues(lm.fit))

which.max(hatvalues(lm.fit))


## Multiple Regression ####
lm.fit1 <- lm(Sales ~ TV + Radio + Newspaper, data = company)
hist(residuals(lm.fit1), col = "steelblue")
summary(lm.fit1)
# Based on the Coefficient estimate, It shows a 4.6 increase in cost to make sales
# For every 1 increase in cost, the sales up by 0.05 for TV, 0.11 for Radio.
# TV and Radio are statistically significant but the Newspaper isn't according the P-value
# The t-value which measures our standard deviations estimates from 0 shows both TV & Radio to be relevant to sales.
# the R-squared of 0.09026 (90%) of the observed response explained the variance in the variable.
par(mfrow = c(2, 2))
plot(lm.fit1)
abline(lm.fit1)

# prediction of Sales using response from the summary of the model
#From the summary output we know the fitted multiple regression equation is,
Sales = 4.625 + 0.054*Tv + 0.107*Radio + 0.000*Newspaper


#define the coefficients from the model output
intercept <- coef(summary(lm.fit1))["(Intercept)", "Estimate"]
TV <- coef(summary(lm.fit1))["TV", "Estimate"]
Radio <- coef(summary(lm.fit1))["Radio", "Estimate"]
Newspaper <- coef(summary(lm.fit1))["Newspaper", "Estimate"]

#use the model coefficients to predict the value for mpg
intercept + TV*0.054 + Radio*0.107 + Newspaper*0.000
# The model predic ts that the companies would have a Sales of 4.639513.


## Decision Tree  ####
library(tree)
attach(company)
High <- factor(ifelse(Sales <= 8, "No", "Yes"))
company <- data.frame(company, High)
tree.company <- tree(High ~ . -Sales, company)
summary(tree.company)
# Five (5) terminal nodes with a misclassification or the training error rate of 0.005
# Residual mean = 0.03
tree.company

par(mfrow = c(1, 1))
plot(tree.company)
text(tree.company, pretty = 0, cex = .6)
# The most important indicator of Sales is TV
# Summarily, the TV with less than 38.85 stands to be the parent nodes with three (3) branch. 

# Prediction
set.seed(2) #
train <- sample(1:nrow(company), 100) #taking half of the dataset
company.test <- company[-train, ]
High.test <- High[-train]
tree.company <- tree(High ~ . -Sales, company, subset = train)
tree.pred <- predict(tree.company, company.test, type = "class")

table(tree.pred, High.test)

# % correct
(5 + 94) / 100

# The predication leads to 99% of the test.


