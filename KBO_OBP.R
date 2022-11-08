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
bar_det <- read.csv(file = "speed_angle.csv", header = TRUE)
bar_det <- bar_det[,-1]
player_names <- unique(bar_det$name)

min_bat <- read.csv(file = "PA_Qual.csv", header = TRUE)
min_bat$OBP <- min_bat$OBP * 100
min_bat <- min_bat[,-1]


# Bat result - hit
for (i in 1:nrow(bar_det)) {
  
  if(bar_det[i,]$result == "단타" |
     bar_det[i,]$result == "내야안타" |
     bar_det[i,]$result == "번트안타"){
    bar_det[i,5] = 1
    bar_det[i,6] = 1
  }
  
  if(bar_det[i,]$result == "2루타"){
    bar_det[i,5] = 2
    bar_det[i,6] = 1
  }
  
  if(bar_det[i,]$result == "3루타"){
    bar_det[i,5] = 3
    bar_det[i,6] = 1
  }
  
  if(bar_det[i,]$result == "홈런"){
    bar_det[i,5] = 4
    bar_det[i,6] = 1
  }
  
}

# Bat result - out
bar_det <- rename(bar_det, base_count = V5)
bar_det <- rename(bar_det, hit = V6)
bar_det$base_count <- replace_na(bar_det$base_count, 0)
bar_det$hit <- replace_na(bar_det$hit, 0)

# Barrel Determination process
test1 <- which(bar_det$speed >= 147 &
          bar_det$speed <= 153&
          bar_det$angle <= 29 & 
          bar_det$angle >= 27)

mean(bar_det$base_count[test1])
mean(bar_det$hit[test1])

for (i in 0:30) {
  cond <- which(bar_det$speed >= (147 + i) &
                bar_det$speed <= (153 + i) &  
                  bar_det$angle <= 29 + (2/3)*i &
                  bar_det$angle >= 27 - 0.9*i)
  
  bar_det[cond,7] = TRUE
}

bar_det <- rename(bar_det, barrel = V7)
bar_det$barrel <- replace_na(bar_det$barrel, FALSE)
mean(bar_det$barrel)

mydata <- data.frame()

for (j in unique(bar_det$name)){
  temp <- bar_det %>% filter(name == j) %>% summarise(name = j, barrel_per = mean(barrel)*100)
  mydata <- rbind(mydata, temp)
}

min_bat <- full_join(min_bat, mydata, by = "name")

for (j in unique(bar_det$name)){
  assign(paste0(j,"2019"), bar_det %>% filter(name == j), envir = .GlobalEnv)
}

# Data cleaning for modeling
min_bat <- min_bat[,c(1,5,2,4,6)]

# Matrix scatter plots
ggpairs(min_bat[,-1])

# Fitting model
kbo_mlr <- lm(OBP ~ whiff_per + first_strike_per + 
                barrel_per, data = min_bat)
summary(kbo_mlr)

# Confidence Interval
confint(kbo_mlr)

# Added variable plots
avPlots(kbo_mlr)

# Std.Residual histogram (normality)
std <- stdres(kbo_mlr)
hist(std, main = "표준오차", xlab = "Standardized Residuals")

ks.test(std,"pnorm")

# Residual plots
qplot(x = kbo_mlr$fitted.values, y = kbo_mlr$residuals, 
      data = kbo_mlr, geom = "point", xlab = "Fitted Values", 
      ylab = "Residuals") + geom_hline(aes(yintercept=0))

bptest(kbo_mlr)

# Total prediction
preds <- predict.lm(kbo_mlr, newdata = min_bat, interval = "prediction", level = .95)

diff <- min_bat$OBP - preds[,1]
diff

# Top missed
hlier <- min_bat[which(diff>3),]
hlier

# Low missed
llier <- min_bat[which(diff<(-4)),]
llier

# Cross Validation
n.cv <- 1000
bias <- rep(NA,n.cv)
rpmse <- rep(NA,n.cv)
cvg <- rep(NA,n.cv)
wid <- rep(NA,n.cv)
n.test <- round(nrow(min_bat)/10)

for(i in 1:n.cv){
  #split into test and training set
  test.obs <- sample(1:nrow(min_bat),n.test)
  test.set <- min_bat[test.obs,]
  train.set <- min_bat[-test.obs,]
  
  # fit a lm using training data only
  train.lm <- lm(OBP ~ whiff_per + first_strike_per + 
                   barrel_per, data=train.set)
  
  # Prediction and prediction intervals
  test.pred <- predict.lm(train.lm,newdata = test.set,interval="prediction",level = 0.95)  
  
  # calculate results
  bias[i] <- mean(test.pred[,1] - test.set$OBP)
  rpmse[i] <- sqrt(mean((test.pred[,1] - test.set$OBP)^2))
  cvg[i] <- mean(test.pred[,2] < test.set$OBP & test.pred[,3] > test.set$OBP)
  wid[i] <- mean(test.pred[,3]-test.pred[,2])  
}
mean(bias)
mean(rpmse)
mean(cvg)
mean(wid)
