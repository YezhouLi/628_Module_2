########Load data and packages###########
## assume working directory is the one that current juno is in
set.seed(1)
dt.path = "Data/BodyFat.csv"
dt.fat = read.csv(dt.path)
body.fat = subset(dt.fat, select = c(-IDNO, -DENSITY))
if(!require("dplyr")){install.packages("dplyr")}
if(!require("tidyverse")){install.packages("tidyverse")}
if(!require("caret")){install.packages("caret")}

############Data Description##############
summary(dt.fat)
par(mfrow=c(2,3))
hist(dt.fat$BODYFAT,breaks = 20, 
     main = "Histogram of Bodyfat",xlab="Bodyfat %")
hist(dt.fat$DENSITY,breaks = 20, 
     main = "Histogram of Density",xlab="Density")
hist(dt.fat$AGE,breaks = 20, 
     main = "Histogram of Age",xlab="Age")
hist(dt.fat$WEIGHT,breaks = 20, 
     main = "Histogram of Weight",xlab="Weight")
hist(dt.fat$HEIGHT,breaks = 20, 
     main = "Histogram of Height",xlab="Height")
hist(dt.fat$ADIPOSITY,breaks = 20, 
     main = "Histogram of Adiposity",xlab="Adiposity")
plot(dt.fat$BODYFAT ~ 1/dt.fat$DENSITY,xlab = "1/Density",ylab = "Bodyfat")

###########Data cleaning###########
id<-as.vector(dt.fat$IDNO)
bodyf<-as.vector(dt.fat$BODYFAT)
den<-as.vector(dt.fat$DENSITY)
diff<-abs(bodyf -(495/den-450))
ab<-cbind(id,diff)
ab[order(ab[,2],decreasing = T),][1:10,]
col.mean1 = round(sapply(dt.fat[,-1], dim = 1, FUN = mean), digits = 2)
bd = rbind(dt.fat[c(182,96, 48,54, 76,124), -1], col.mean1)
rownames(bd)[4] = '54 (compared with 48)'
rownames(bd)[6] = '124 (compared with 76)'
rownames(bd)[7] = ' Sample Mean'
bd
id<-as.vector(dt.fat$IDNO)
adi<-as.vector(dt.fat$ADIPOSITY)
weightkg<-as.vector(dt.fat$WEIGHT)*0.453592
hightm<-as.vector(dt.fat$HEIGHT )*0.0254
diff2<-abs(adi -weightkg/(hightm)^2)
ab2<-cbind(id,diff2)
ab2[order(ab2[,2],decreasing = T),][1:10,]
# The person with largest bodyfat
dt.fat[dt.fat$BODYFAT==max(dt.fat$BODYFAT), ]
# The shortest person's other measurements are normal so we can consider his height is recorded wrongly.
body.fat[body.fat$HEIGHT==min(body.fat$HEIGHT),]
# We can use the BMI formula to impute his real height.
body.fat$HEIGHT[42] = round(sqrt(205*0.45359237/29.9)*39.3700787,1)
body.fat[42,]
model.clean = lm(BODYFAT ~ ., data = body.fat)
# summary(model.clean)$coefficient
plot(model.clean, which = 4)
abline(h = 4/(dim(body.fat)[1]-dim(body.fat)[2]), lty = 2, col = 'blue')
col.mean = round(sapply(body.fat, dim = 1, FUN = mean), digits = 2)
. = rbind(body.fat[c(39, 86, 221), ], col.mean)
rownames(.)[4] = ' Sample Mean'
.
body.fat.clean = body.fat[-c(163, 182,216),]
model.clean2 = lm(BODYFAT ~ ., data = body.fat.clean)
layout(matrix(1:4, ncol = 2))
plot(model.clean2)

##########variable selection for linear model#####################
model.1 <- lm(BODYFAT~1,data = body.fat.clean)
model.AIC.forward <- step(model.1,list(lower = model.1,upper = model.clean2),k = 2,direction = "forward",trace = 0)
summary(model.AIC.forward)
model.AIC.backward <- step(model.clean2,k = 2,direction = "backward",trace = 0)
summary(model.AIC.backward)
model.BIC.forward <- step(model.1,list(lower = model.1,upper = model.clean2),k = log(dim(body.fat.clean)[1]),direction = "forward",trace = 0)
summary(model.BIC.forward)
model.BIC.backward <- step(model.clean2,k = log(dim(body.fat.clean)[1]),direction = "backward",trace = 0)
summary(model.BIC.backward)
model.full = lm(BODYFAT ~ ., body.fat.clean)
model.aic.both = step(model.full, direction = "both", k = 2, trace = 0)
summary(model.aic.both)
model.bic.both = step(model.full, direction = "both", k = log(dim(body.fat.clean)[1]), trace = 0)
summary(model.bic.both)
#detach("package:car")
install.packages("leaps")
library("leaps") # for leaps()
install.packages("faraway")
library("faraway") # for Cpplot()
X = model.matrix(model.full)[,-1]
Y = body.fat.clean[,1]
g = leaps::leaps(X,Y,nbest=1)
Cpplot(g) # Include all variable.
r = leaps::leaps(X,Y,nbest=1, method="adjr2")
plot(r$adjr2)
(r$which)[which(r$adjr2 == max(r$adjr2)),] # 1,2,3,7,12,13

###########Data Transformation##############
data = body.fat.clean
data.invwei <- data.frame(BODYFAT = data$BODYFAT,HEIGHT = data$HEIGHT/data$WEIGHT,
                          ADIPOSITY = data$ADIPOSITY/data$WEIGHT,NECK = data$NECK/data$WEIGHT,
                          CHEST = data$CHEST/data$WEIGHT,ABDOMEN = data$ABDOMEN/data$WEIGHT,
                          HIP = data$HIP/data$WEIGHT,THIGH = data$THIGH/data$WEIGHT,
                          KNEE = data$KNEE/data$WEIGHT,ANKLE = data$ANKLE/data$WEIGHT,
                          BICEPS = data$BICEPS/data$WEIGHT,FOREARM = data$FOREARM/data$WEIGHT,
                          WRIST = data$WRIST/data$WEIGHT)
data.invwei$inv_wei <- 1/data$WEIGHT

#########variables selection for fraction model###########
BF.nonlinear = body.fat.clean
kFoldCV = 10

nonWeightCol = c(2, seq(4, 15))
WeightCol = c(3)

BF.nonlinear[, nonWeightCol] = BF.nonlinear[, nonWeightCol] / BF.nonlinear[, WeightCol]
BF.nonlinear$invWeight = 1 / BF.nonlinear$WEIGHT
BF.nonlinear$WEIGHT = NULL

fracModelsInfo = data.frame(matrix(ncol = 3, nrow = length(nonWeightCol)))
colnames(fracModelsInfo) = c("Variable.Name", "RMSE", "rSquare")

set.seed(123) 
train.control = trainControl(method = "cv", number = kFoldCV)

for(i in seq(2, 14)){
  varName = colnames(BF.nonlinear)[i]
  model = train(BODYFAT ~ . , data = BF.nonlinear[, c("BODYFAT", "invWeight", varName)],
                method = "lm", trControl = train.control)
  
  fracModelsInfo$Variable.Name[i-1] = varName
  fracModelsInfo$RMSE[i-1] = model$results[, 'RMSE']
  fracModelsInfo$rSquare[i-1] = model$results[, 'Rsquared']
  # fracModelsInfo$model[i-1] = model$finalModel
}
cat("Best Model by RMSE")
fracModelsInfo[ which.min(fracModelsInfo$RMSE),  ]
cat("Best Model by R Square")
fracModelsInfo[ which.max(fracModelsInfo$rSquare),  ]

fracModelBestVar = fracModelsInfo[ which.min(fracModelsInfo$RMSE), 'Variable.Name']

cat("Sorted by RMSE")
fracModelsInfo[ order(fracModelsInfo$RMSE), ]
cat("Sorted by R Square")
fracModelsInfo[ order(fracModelsInfo$rSquare, decreasing = TRUE), ]
frac.model.1 = lm(BODYFAT ~ . , data = BF.nonlinear[, c("BODYFAT", "invWeight", fracModelBestVar)])
summary(frac.model.1) # >>> the intercept is not significant so we consider drop it in the next model. 

frac.model.2 = lm(BODYFAT ~ . -1, data = BF.nonlinear[, c("BODYFAT", "invWeight", fracModelBestVar)])
summary(frac.model.2)
#install.packages("caret")
#install.packages("car")

###########Multilinearity###########
library(caret)
library(car)
attach(body.fat.clean)
training.index = sample(1:dim(body.fat.clean)[1],size = 0.7*dim(body.fat.clean)[1])
training.set = body.fat.clean[training.index,]
test.set = body.fat.clean[-training.index,]
# Full model with four variables
m1 = lm(BODYFAT ~ WEIGHT + ABDOMEN + FOREARM + WRIST, data = training.set)
predictions = predict(m1,test.set)
df1 = data.frame(RMSE = RMSE(predictions, test.set$BODYFAT),
                 R2 = R2(predictions, test.set$BODYFAT))
vif(m1)
# Drop WEIGHT >>> model performance does not change too much so we can dropping weight might be helpful with multicolinearity
m2 = lm(BODYFAT ~ ABDOMEN + FOREARM + WRIST, data = training.set)
predictions = predict(m2,test.set)
df2 = data.frame(RMSE = RMSE(predictions, test.set$BODYFAT),
                 R2 = R2(predictions, test.set$BODYFAT))

# Drop WEIGHT + ABDOMEN >>> model performance gets worse so we can not drop ABDOMEN
m3 = lm(BODYFAT ~ FOREARM + WRIST, data = training.set)
predictions = predict(m3,test.set)
df3 = data.frame(RMSE = RMSE(predictions, test.set$BODYFAT),
                 R2 = R2(predictions, test.set$BODYFAT))

# Drop WEIGHT + FOREARM
m4 = lm(BODYFAT ~ ABDOMEN + WRIST, data = training.set)
predictions = predict(m4,test.set)
df4 = data.frame(RMSE = RMSE(predictions, test.set$BODYFAT),
                 R2 = R2(predictions, test.set$BODYFAT))

# Drop WEIGHT + WRIST
m5 = lm(BODYFAT ~ ABDOMEN + FOREARM, data = training.set)
predictions = predict(m5,test.set)
df5 = data.frame(RMSE = RMSE(predictions, test.set$BODYFAT),
                 R2 = R2(predictions, test.set$BODYFAT))

# Drop WRIST + FOREARM
m6 = lm(BODYFAT ~ WEIGHT + ABDOMEN, data = training.set)
predictions = predict(m6,test.set)
df6 = data.frame(RMSE = RMSE(predictions, test.set$BODYFAT),
                 R2 = R2(predictions, test.set$BODYFAT))
result= data.frame(c("Full model", "ABDOMEN + FOREARM + WRIST", "FOREARM + WRIST",
                     "ABDOMEN + WRIST", "ABDOMEN + FOREARM","WEIGHT + ABDOMEN"))
df = rbind(df1, df2, df3, df4, df5, df6)
result = cbind(result, df)
colnames(result) = c("Models", "RMSE", "R2")
result
linear.model = lm(BODYFAT~ABDOMEN+WRIST,data = body.fat.clean)
summary(linear.model)
