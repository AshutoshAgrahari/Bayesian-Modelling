# Source: http://www.unofficialgoogledatascience.com/2017/07/fitting-bayesian-structural-time-series.html

rm(list = ls())
options(error = NULL, scipen = 999)

library(bsts)     # load the bsts package
data(iclaims)     # bring the initial.claims data into scope

dat <- initial.claims
head(dat)
plot(dat$iclaimsNSA)


ss <- AddLocalLinearTrend(list(), dat$iclaimsNSA)
ss <- AddSeasonal(ss, initial.claims$iclaimsNSA, nseasons = 52)
model1 <- bsts(initial.claims$iclaimsNSA,
               state.specification = ss,
               niter = 1000)

names(model1)

plot(model1)
plot(model1,"components")
plot(model1,"residuals")
plot(model1, 'help')

pred1 <- predict(model1, horizon = 12)
plot(pred1,plot.original = 156)



# Fit a bsts model with expected model size 1, the default.
model2 <- bsts(iclaimsNSA ~ .,
               state.specification = ss,
               niter = 1000,
               data = initial.claims)


# Fit a bsts model with expected model size 5, to include more coefficients.
model3 <- bsts(iclaimsNSA ~ .,
               state.specification = ss,
               niter = 1000,
               data = initial.claims,
               expected.model.size = 5)  # Passed to SpikeSlabPrior.


plot(model2,"comp")
plot(model3,"comp")

plot(model2,"coef")
plot(model3,"coef")


# pred1 <- predict(model2, horizon = 12)
# plot(pred1,plot.original = 156)
# 
# pred1 <- predict(model1, horizon = 12)
# plot(pred1,plot.original = 156)
