##################################################################
#######                                                ###########
####### OBP ~ Barrel% + Whiff% + First Strike%         ###########
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
mlb_orig <- read.csv(file = "OBP_2019.csv", header = TRUE)
mlb_orig$year <- as.character(mlb_orig$year)
mlb_orig <- mlb_orig[,-9]

# turn OBP into %
mlb_orig$on_base_percent <- mlb_orig$on_base_percent * 100

# Order by OBP
mlb_orig <- arrange(mlb_orig, desc(on_base_percent))

# Data check
mlb <- mlb_orig[,-c(1,2,3,6)]

head(mlb)

# Matrix scatter plots
ggpairs(mlb)

# Fitting model
mlb_mlr <- lm(on_base_percent ~ ., data = mlb)
summary(mlb_mlr)

# Confidence Interval
confint(mlb_mlr)

# Test on reduced model
#reduced.lm <- lm(on_base_percent ~ barrel_batted_rate 
#                 + whiff_percent, data = mlb)
#anova(mlb_mlr, reduced.lm)

# Added variable plots
avPlots(mlb_mlr)

# Std.Residual histogram (normality)
std <- stdres(mlb_mlr)
hist(std, main = "Histogram of Std. Residuals", xlab = "Standardized Residuals")

ks.test(std,"pnorm")

# Residual plots
qplot(x = mlb_mlr$fitted.values, y = mlb_mlr$residuals, 
      data = mlb_mlr, geom = "point", xlab = "Fitted Values", 
      ylab = "Residuals") + geom_hline(aes(yintercept=0))

bptest(mlb_mlr)

# Total prediction
preds <- predict.lm(mlb_mlr, newdata = mlb, interval = "prediction", level = .95)

diff <- mlb$on_base_percent - preds[,1]
diff

# Top missed
hlier <- mlb_orig[which(diff>8),]
hlier

# Low missed
llier <- mlb_orig[which(diff<(-7.5)),]
llier

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
