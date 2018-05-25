#Data set: bostonhousing.csv
#You have to predict the median house value (medv) using the following variables: crim,
#zn, indus, nox, rm, age, dis, rad, tax, ptratio and lstat. Identify the model with the highest
#prediction accuracy using these methods:
#  - best subset selection regression
#- forward and backward stepwise regression
#- ridge regression
#- lasso regression
#- PLS regression

House <- read.csv('bostonhousing.csv')
House$chas <-NULL
str(House)

#Check for missing values 
sapply(House, function(x) sum(is.na(x)))

library(psych)
?pairs.panels
pairs.panels(House[1:11])
library(leaps)

#######Best Subsey regression 

#Run the Best Subset regression from the leaps package
bss_Fit <- regsubsets(medv~.,House, nvmax=12)
summ <-summary(bss_Fit)
summ

##To asses the goodness of fit, we use the adjusted R squared
summ$adjr2

## To find the maximum adjusted R squared --11 Is the optimal model
which.max(summ$adjr2)

#Print the coefficients of this model 
coef(bss_Fit, 9)

plot(bss_Fit)

####Forward Stepwise Regression 

fsr_Fit <- regsubsets(medv~.,House, nvmax=11, method="forward")
summ <-summary(bss_Fit)
summ

##To asses the goodness of fit, we use the adjusted R squared
summ$adjr2

## To find the maximum adjusted R squared --11 Is the optimal model
which.max(summ$adjr2)

#Print the coefficients of this model 
coef(fsr_Fit, 11)

plot(fsr_Fit)

#####Backwards Stepwise Regression 

bsr_Fit <- regsubsets(medv~.,House, nvmax=11, method="backward")
summ <-summary(bss_Fit)
summ

##To asses the goodness of fit, we use the adjusted R squared
summ$adjr2

## To find the maximum adjusted R squared --11 Is the optimal model
which.max(summ$adjr2)

#Print the coefficients of this model 
coef(bsr_Fit, 9)

plot(bsr_Fit)

###### Ridge Regression 
library(glmnet)
## A matix and vector needs to be created

x <- model.matrix(medv~., House)[,-12]

y <- House$medv # Vector of the dependent variables

##Lambda values are power of 10
w <- seq(10,-3, length= 100)
lvalues <-10^w
lvalues

#fit the ridge regression 
rr_fit <- glmnet(x,y, alpha=0, lambda = lvalues)
rr_fit$lambda[40]
coef(rr_fit)[,40]

predict(rr_fit, s=1200, type="coefficients")

plot(rr_fit)

#Validating Ridge reg optimal lambda model with lowest MSE
# Split into training and test set to compute test set mse 

n <-sample(506, 250)

#Fit model on the training set 
#Perform 10 fold cross validation 
library(glmnet)

cv_fit <- cv.glmnet(x[n,], y[n], alpha=0, nfolds=10)
## No need to set lambda in the function. 

optLambda <- cv_fit$lambda.min
optLambda

###Predict Y values for the test set
### Using the optimum lambda
### & compute MSE

pred <- predict(cv_fit, s=optLambda, newx=x[-n,])
head(pred)

mse_test <- mean((pred-y[-n])^2)
mse_test

####Lasso Regression 

x <- model.matrix(medv~., House)

y <- House$medv # Vector of the dependent variables

##Lambda values are power of 10
w <- seq(10,-3, length= 100)
lvalues <-10^w
lvalues

#fit the ridge regression 
lr_fit <- glmnet(x,y, alpha=1, lambda = lvalues)#alpha set to one for lasso
lr_fit$lambda[3]
coef(lr_fit)[,3]

## a model with low lamda 
lr_fit$lambda[99]
coef(lr_fit)[,99]

## Intermediate lambda
lr_fit$lambda[70]
coef(lr_fit)[,70]

## Coefficients different from zero 
coef(lr_fit)[,70][coef(lr_fit)[,70]!=0]

#### Validating lasso regression 

n <-sample(506, 250)
## perform k-fold CV

cv_fit <- cv.glmnet(x[n,], y[n], alpha=1, nfolds=10)
optLambda <- cv_fit$lambda.min
optLambda

###Predict Y values for the test set
### Using the optimum lambda
### & compute MSE

pred <- predict(cv_fit, s=optLambda, newx=x[-n,])
head(pred)

mse_test <- mean((pred-y[-n])^2)
mse_test

## Create vector of lambdas and fit lasso again 
w <- seq(10,-3, length=100)
lvalues <- 10^w
lr_fit <- glmnet(x,y, alpha=1, lambda = lvalues)

##print coefficients of the best model 
predict(lr_fit, s=optLambda, type="coefficients")

library(pls)
#Fit model with k-fold cv
plsr_fit <-plsr(medv~., data =House, scale=T, validation="CV")
#The variable are standardized 
summary(plsr_fit)
MSE <- 4.939^2
MSE
#24.39372

#predictor coefficients for the model with 7 components 

coef(plsr_fit, 7)

## Model with optimal numer of components 
plsr_fit2 <- plsr(medv~., data=House, scale=T, ncomp=7)
summary(plsr_fit2)

coef(plsr_fit2)

## Validation PLS regression approach to obtain test MSE
n <-sample(506, 250)
house_train <- House[n,]
house_test <-House[-n,]

##Fit PLS model on the training set 
plsr_fit <- plsr(medv~., data=house_train, scale=T, ncomp=7)

## Compute the MSE on the test set 

pred <- predict(plsr_fit, house_test, ncomp=7)
head(pred)

mse <- mean((pred-house_test$medv)^2)
mse
