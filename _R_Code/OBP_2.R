##################################################################
#######                                                ###########
####### OBP ~ HardHit% + Whiff% + First Strike%        ###########
#######                                                ###########
##################################################################

# Setting up the working directory
setwd("C:/Users/sports2i/Desktop/WD")

# Loading Library
library(tidyverse)
library(car)
library(GGally)
library(MASS)
library(lmtest)
library(multcomp)

# Reading in data
mlb <- read.csv(file = "MLB_OBP3.csv", header = TRUE)

# Data check
mlb <- mlb[,c(-1,-2,-3,-5,-9)]
head(mlb)

# Matrix scatter plots
ggpairs(mlb)

# Fitting model
mlb_mlr <- lm(on_base_percent ~ ., data = mlb)
summary(mlb_mlr)

# Test on reduced model
#reduced.lm <- lm(on_base_percent ~ barrel_batted_rate 
#                 + whiff_percent, data = mlb)
#anova(mlb_mlr, reduced.lm)

# Added variable plots
avPlots(mlb_mlr)

# Std.Residual histogram (normality)
std <- stdres(mlb_mlr)
hist(std)

ks.test(std,"pnorm")

# Residual plots
plot(mlb_mlr$residuals ~ mlb_mlr$fitted.values)
abline(h=0)

bptest(mlb_mlr)

# Try prediction - Joey Votto (2015)
predict.lm (mlb_mlr,newdata=data.frame(hard_hit_percent = 32.9, f_strike_percent = 54.5, whiff_percent = 24), interval="prediction", level = 0.95)
# Real Stats: 0.459

# Shin-Soo Choo (2018)
predict.lm (mlb_mlr,newdata=data.frame(hard_hit_percent = 40.5, f_strike_percent = 58.2, whiff_percent = 27), interval="prediction", level = 0.95)
# Real Stats: 0.376



# Cross Validation
n.cv <- 1000
bias <- rep(NA,n.cv)
rpmse <- rep(NA,n.cv)
cvg <- rep(NA,n.cv)
wid <- rep(NA,n.cv)
n.test <- round(nrow(mlb)/10)

for(i in 1:n.cv){
  #split into test and training set
  test.obs <- sample(1:nrow(mlb),n.test)
  test.set <- mlb[test.obs,]
  train.set <- mlb[-test.obs,]
  
  # fit a lm using training data only
  train.lm <- lm(on_base_percent ~ ., data=train.set)
  
  # Prediction and prediction intervals
  test.pred <- predict.lm(train.lm,newdata = test.set,interval="prediction",level = 0.95)  
  
  # calculate results
  bias[i] <- mean(test.pred[,1] - test.set$on_base_percent)
  rpmse[i] <- sqrt(mean((test.pred[,1] - test.set$on_base_percent)^2))
  cvg[i] <- mean(test.pred[,2] < test.set$on_base_percent & test.pred[,3] > test.set$on_base_percent)
  wid[i] <- mean(test.pred[,3]-test.pred[,2])  
}
mean(bias)
mean(rpmse)
mean(cvg)
mean(wid)
