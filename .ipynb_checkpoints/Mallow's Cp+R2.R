setwd("D:/Fall2019/STAT 628 Data Science Practicum/Module2")
data = read.csv("Bodyfat.csv",header = T)
dim(data)
str(data)
data = data[,-1]
head(data)
summary(data)
attach(data)


# Data Description #
par(mfrow=c(2,3))
hist(data$BODYFAT,breaks = 20, 
     main = "Histogram of Bodyfat",xlab="Bodyfat %")
hist(data$DENSITY,breaks = 20, 
     main = "Histogram of Density",xlab="Density")
hist(data$AGE,breaks = 20, 
     main = "Histogram of Age",xlab="Age")
hist(data$WEIGHT,breaks = 20, 
     main = "Histogram of Weight",xlab="Weight")
hist(data$HEIGHT,breaks = 20, 
     main = "Histogram of Height",xlab="Height")
hist(data$ADIPOSITY,breaks = 20, 
     main = "Histogram of Adiposity",xlab="Adiposity")

## Check extreme points
data[which(data$BODYFAT==0),] # 182. Consider drop it.
data[which(data$BODYFAT==max(BODYFAT)),] # 216
data[which(HEIGHT==min(HEIGHT)),] # 42, this person has extremely small heigth, 29.5in=77cm 
                                  # and relatively large weight. Consider drop it.
data[which(WEIGHT==max(WEIGHT)),] # 39
data[which(ADIPOSITY==max(ADIPOSITY)),] # 39. This person is tooooo fat. T_T

# Fit the full model
model1=lm(BODYFAT ~ ., data=data)
summary(model1)
plot(model1) # 96, 48, 76 may be an outlier

## Check how 96 looks like
data[96,]
summary(WEIGHT) # exceed 3rd quantile
summary(HEIGHT) # Largest height
summary(ADIPOSITY)
summary(BODYFAT)

## Check how 48 looks like
data[48,] # extremely small bodyfat; other measurements like neck, chest, hip, etc. are normal 
          # so it is impossible to have such a small bodyfat. Consider drop it.
data[76,] # seems normal

## Drop three points.
data=data[-c(42,48,182),]


# Variable Selection
model = lm(BODYFAT~.,data=data)
plot(model)

## Mallow's Cp
layout(matrix(1:1,ncol=1))
X = model.matrix(model)[,-1]
Y = data[,1]
library(leaps) # for leaps()
install.packages("faraway")
library(faraway) # for Cpplot()
g = leaps::leaps(X,Y,nbest=1)
Cpplot(g) # Include all variable.

## R squared
r = leaps::leaps(X,Y,nbest=1, method="adjr2")
plot(r$adjr2)
(r$which)[which(r$adjr2 == max(r$adjr2)),] # 1,2,3,7,12,13


