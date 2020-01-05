# Source: http://www.unofficialgoogledatascience.com/2017/07/fitting-bayesian-structural-time-series.html

rm(list = ls())
options(error = NULL, scipen = 999)

library(bsts)     # load the bsts package
data(iclaims)     # bring the initial.claims data into scope

dat <- initial.claims
head(dat)
plot(dat$iclaimsNSA)

traindat <- dat[time(dat) < "2012-01-01"]
testdat <- dat[time(dat) >= "2012-01-01"]

ss <- AddLocalLinearTrend(list(), traindat$iclaimsNSA)
ss <- AddSeasonal(ss, traindat$iclaimsNSA, nseasons = 52)
model1 <- bsts(traindat$iclaimsNSA,
               state.specification = ss,
               niter = 1000)

names(model1)

plot(model1)
plot(model1,"components")
plot(model1,"residuals")
#plot(model1, 'help')


pred1 <- predict(model1, horizon = nrow(testdat))
plot(pred1)

#pred1$distribution
#pred1$interval
#pred1$mean
#pred1$median


# Fit a bsts model with expected model size 1, the default.
model2 <- bsts(iclaimsNSA ~ .,
               state.specification = ss,
               niter = 1000,
               data = traindat)


# Fit a bsts model with expected model size 5, to include more coefficients.
model3 <- bsts(iclaimsNSA ~ .,
               state.specification = ss,
               niter = 1000,
               data = traindat,
               expected.model.size = 5)  # Passed to SpikeSlabPrior.


plot(model2,"comp")
plot(model3,"comp")

plot(model2,"coef")
plot(model3,"coef")


pred2 <- bsts::predict.bsts(object = model2,newdata = testdat)
pred3 <- bsts::predict.bsts(object = model3,newdata = testdat)


par(mfrow=c(2,2))
plot(dat$iclaimsNSA)
plot(pred1)
plot(pred2)
plot(pred3)



mape1 <- mean(abs((as.numeric(testdat$iclaimsNSA)-pred1$mean)/as.numeric(testdat$iclaimsNSA)))*100
mape2 <- mean(abs((as.numeric(testdat$iclaimsNSA)-pred2$mean)/as.numeric(testdat$iclaimsNSA)))*100
mape3 <- mean(abs((as.numeric(testdat$iclaimsNSA)-pred3$mean)/as.numeric(testdat$iclaimsNSA)))*100

mape1a <- mean(abs((as.numeric(testdat$iclaimsNSA)-pred1$median)/as.numeric(testdat$iclaimsNSA)))*100
mape2a <- mean(abs((as.numeric(testdat$iclaimsNSA)-pred2$median)/as.numeric(testdat$iclaimsNSA)))*100
mape3a <- mean(abs((as.numeric(testdat$iclaimsNSA)-pred3$median)/as.numeric(testdat$iclaimsNSA)))*100
