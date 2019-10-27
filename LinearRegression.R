#rm(list = ls())
#setwd("C:/Users/lxue5/Dropbox/2019Sp/CIS8695/1 Regression")
# options(repos = c(CRAN = "http://cran.rstudio.com"))

car.df <- read.csv("ToyotaCorolla.csv")
# select variables for regression
selected.var <- c("Price","Age_08_04","KM","Fuel_Type","HP","Met_Color",
                  "Automatic","cc","Doors", "Quarterly_Tax","Weight")

# partition data
set.seed(1)  # set seed for reproducing the partition
train.index <- sample(c(1:dim(car.df)[1]), dim(car.df)[1]*0.6)  
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]

# use lm() to run a linear regression of Price on all 11 predictors in the training set. 
car.lm <- lm(Price ~ ., data = train.df)
#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)   # Get model summary


# Plot barchart of coefficients
library(lattice)
barchart(car.lm$coefficients)
# An alternative approach using ggplot
library(ggplot2)
coeff.plot<-stack(coefficients(car.lm))
ggplot(coeff.plot) + geom_bar(aes(x = ind, y = values), stat = "identity")


# Standardized coefficients
# install.packages("QuantPsyc")
library(QuantPsyc)
car.lm.s<-lm.beta(car.lm)
barchart(car.lm.s)

# Make predictions on a new set. 
# install.packages("forecast") # Package installation is required for the first time
library(forecast)
car.lm.pred <- predict(car.lm, valid.df)
some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20]
data.frame("Predicted" = car.lm.pred[1:20], "Actual" = valid.df$Price[1:20],
           "Residual" = some.residuals)
options(scipen=999, digits = 3)
# use accuracy() to compute common accuracy measures.
accuracy(car.lm.pred, valid.df$Price)


# Visually check residuals
car.lm.pred <- predict(car.lm, valid.df)
all.residuals <- valid.df$Price - car.lm.pred
# The majority of residual values fall into [-1406, 1406]
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")

# Use VIF to check multicollinearity
# install.packages("car")
car::vif(car.lm)


# Variable Selection
# use step() to run stepwise regression.
# set directions =  to either "backward", "forward", or "both".
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step)  # Which variables did it drop?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)
accuracy(car.lm.pred, valid.df$Price)

# Try a Backward Variable Selection
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step)  # Which variables did it drop?

# Try a Forward Variable Selection
null=lm(Price~1, data=train.df)    #For Forward Selection, you need to start with a null model without any predictors
step(null, scope=list(lower=null, upper=car.lm), direction="forward")
step(null, scope=list(lower=null, upper=car.lm), direction="both")



# Alternative: use regsubsets() in package leaps to run an exhaustive search. 
# unlike with lm, categorical predictors must be turned into dummies manually.
# install.packages("leaps") # Package installation is required for the first time
library(leaps)
search <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)

# show models step-by-step entry
sum$which

# show metrics
sum$rsq
sum$adjr2

# Try a Forward Variable Selection Approach using regsubsets()
search <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "forward")
summary(search)
