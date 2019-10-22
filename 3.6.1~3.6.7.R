# 3.6.1 Libraries
library(MASS)
library(ISLR)

# 3.6.2 Simple Linear Regression
fix(Boston)
names(Boston)
?Boston

# lm(y~x,data)
# basic syntax, y is the response, x is the predictor

lm.fit=lm(medv~lstat)  #error
lm.fit=lm(medv~lstat,data=Boston) #correct

# attach Boston, the ???rst line works ???ne because R now recognizes the variables. 
attach(Boston)  
lm.fit=lm(medv~lstat)
plot(lm.fit)
lm.fit
summary(lm.fit)

# ???nd out what other pieces of information are stored in lm.fit
names(lm.fit) 

#  extractor functions, the second code is btter
lm.fit$coefficients
coef(lm.fit)

# obtain a con???dence interval for the coe???cient estimates, 
confint(lm.fit)  

# produce con???dence intervals and prediction intervals 
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="confidence")
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="prediction")

plot(lstat,medv)

# To draw a line with intercept a and slope b, we type abline(a,b)
abline(lm.fit)
# The lwd=3 command causes the width of the regression line to be increased by a factor of 3; this works for the plot() and lines() functions also. We can also use the pch option to create di???erent plotting symbols.
abline(lm.fit ,lwd=3)
abline(lm.fit ,lwd=3,col="red")
plot(lstat, medv, pch="20")
plot(lstat, medv, pch="+")
plot(1:20,1:20,pch=1:20) # different shape

# divide the image to be viewed simultaneously
par(mfrow=c(2,2))
plot(lm.fit)

# compute the residuals from a linear regression fit
plot(predict (lm.fit), residuals (lm.fit)) 
# return the studentized residuals
plot(predict (lm.fit), rstudent (lm.fit))

# compute Leverage statistics, hatvalues mean estimate-value  
plot(hatvalues(lm.fit))

# identi???es the index of the largest element of a vector. 
# In this case, it tells us which observation has the largest leverage statistic.
which.max(hatvalues(lm.fit))

# 3.6.3 Multiple Linear Regression

# The syntax lm(y???x1+x2+x3) is used to ???t a model with three predictors, x1, x2, and x3. 
lm.fit=lm(medv~lstat+age,data=Boston) 
summary(lm.fit)
plot(lm.fit)

# perform a regression using all of the predictors.
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)          

# type ?summary.lm to see what is available
# summary(lm.fit)$r.sq gives us the R^2
# summary(lm.fit)$sigma gives us the RSE

library(car)
# compute variance in???ation factors (VIF)
vif(lm.fit)

# run a regression excluding this predictor(age)
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)

# Alternatively, the update() function can be used.
lm.fit1=update(lm.fit , ~.-age)

############################################################
############################################################
# 3.6.4 Interaction Terms

# The syntax lstat:black tells R to include an interaction term between lstat and black
# lstat*age it is a shorthand for lstat+age+lstat:age. 
summary(lm(medv~lstat*age, data=Boston))
lm.fitla=lm(medv~lstat*age, data=Boston)

anova(lm.fit,lm.fitla)
plot(lm.fitla)


plot(lstat,medv, data=Boston)
plot(age,medv, data=Boston)
plot(lstat*age, medv, data= Boston)
# anylsis t-stat, p-value, F-stat

# 3.6.5Non-linear Transformations of the Prediction
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)


plot(lstat, medv)
plot(I(lstat^2),medv)
plot(lstat+I(lstat^2),medv)

lm.fit=lm(medv~lstat)
anova(lm.fit, lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)           

plot(lm.fit5)

summary(lm(medv~log(rm),data=Boston))
lm.fitl= lm(medv~log(rm),data=Boston)
plot(lm.fitl)
anova(lm.fit2,lm.fit5,lm.fitl)

# 3.6.6 Qualitative Predictors

fix(Carseats)
names(Carseats)
summary(lm(Sales~.,data=Carseats))
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
attach(Carseats )
contrasts(ShelveLoc)

# 3.6.7 Writing Functions
LoadLibraries()
LoadLibraries=function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
  }

LoadLibraries
LoadLibraries()

#容易忽略：画线abline()， 对比更优模型anova(lm.fit ,lm.fit2)，、
#置信区间confint() ，预测predict (lm.fit ,data.frame(lstat=c(5,10,15)), interval="confidence")
#hatvalues() 用于Leverage statistics can be computed for any number of predictors
#which.max() function identiﬁes the index of the largest element of a vector
