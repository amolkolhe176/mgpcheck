library(ISLR)
library(gam)
library(visreg)

data("Auto")
str(Auto)

pairs(Auto)

#rmse function to calculate training error on residuals
rmse <- function(error)
{
  sqrt(mean(error^2))
}

#linear regression
basic.lm <- lm(mpg ~ acceleration, data=Auto)
summary(basic.lm)
rmse.lm <- rmse(basic.lm$residuals)

#polynomial regression, fourth-degree polynomial
poly.lm <- lm(mpg ~ poly(acceleration,4), data=Auto)
summary(poly.lm)
rmse.poly <- rmse(poly.lm$residuals)


#step function regression with 4 cuts
step.lm <- lm(mpg ~ cut(acceleration,4), data=Auto) 
summary(step.lm)
rmse.step <- rmse(step.lm$residuals)

#visualize fit
visreg(basic.lm, main="simple regression")
visreg(poly.lm, main="fourth-degree poly")
visreg(step.lm, main="step regression")

#lets fit a cubic spline with knots at ages 25, 40, & 60
cspline.lm <- lm(mpg ~ bs(acceleration, knots=c(25,40,60)), data=Auto)
rmse.cspline <- rmse(cspline.lm$residuals)
visreg(cspline.lm, main="cubic spline")

#natural spline fit with 4 degrees of freedom
nspline.lm <- lm(mpg ~ ns(acceleration, df=4), data=Auto)
rmse.nspline <- rmse(nspline.lm$residuals)

visreg(cspline.lm, main="natural spline")

#first lets put all trained models in a list object
rmse.plot<- list("lm"=rmse.lm,"poly" = rmse.poly, "step" = rmse.step,
              "cspline" = rmse.cspline, "nspline"=rmse.nspline)

rmse.plot

#smooth spline using cross validation to determine smoothness
sspline <- smooth.spline(x=Auto$acceleration, y=Auto$mpg, cv=T)
sspline$df

dev.off()

#visualize spline
plot(mpg ~ acceleration, data=Auto)
lines(sspline, col="blue", lwd=3)


#local regression LOESS with neighborhoods of 20% of the data
loess.fit <- loess(mpg~acceleration, span=.2, data=Auto)
visreg(loess.fit) #visualzie local regression

#generalized additive models, 2 smooth splines + weight
gam.fit <- gam(mpg ~ s(acceleration,4) + s(year,5) + weight, data=Auto)
summary(gam.fit)
rmse(gam.fit$residuals)
par(mfrow = c(2,2)); plot(gam.fit)

#some caret modeling on Auto data

library(caret)
library(pROC)

data(Auto)

#remove row number column
Auto$name <- NULL 


#lasso regression with polynomials to find optimal degrees
ctrl <- trainControl(method="cv", number=5)

set.seed(400)

#gamSpline in caret 
gam.train <- train(mpg ~ ., data=Auto, 
                   method="gamSpline",tuneLength=10,
                   trControl=ctrl)
gam.train
gam.train$finalModel

#compare to lm
set.seed(400)
lm.train <- train(mpg ~ ., data=Auto, 
                  method="lm",
                  trControl=ctrl)
lm.train


#pcr pca regression best from last class
library(pls)
set.seed(400) #SEED
h.pcr <- train(mpg ~ ., data= Auto, method = "pcr", tuneLength=10, trControl=ctrl)
h.pcr
plot(h.pcr)

#lets gather the models
#first lets put all trained models in a list object
models<- list("lm"=lm.train, "gam" = gam.train,
              "PCR"=h.pcr)


Auto.resamples<- resamples(models)
summary(Auto.resamples)

#plot performances
bwplot(Auto.resamples, metric="RMSE")
bwplot(Auto.resamples, metric="Rsquared")

