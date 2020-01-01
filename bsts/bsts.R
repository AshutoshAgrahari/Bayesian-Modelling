# bsts: Bayesian Structure Time Series Modelling
# Reference: https://rpubs.com/osazuwa/bsts

library(tidyverse)
library(bsts)

data(iclaims)
.data <- initial.claims
claims <- .data$iclaimsNSA
plot(claims, ylab = "")


#Start with an empty list of model components.
model_components <- list()

#Add a Trend Component
?AddAr
?AddAutoAr
?AddLocalLevel
?AddLocalLinearTrend
?AddStudentLocalLinearTrend
?AddGeneralizedLocalLinearTrend


summary(model_components <- AddLocalLinearTrend(model_components, y = claims))


#Add a Seasonal Component
?AddTrig # Trigonometric seasonal
?AddSeasonal
?AddNamedHolidays # not found
?AddFixedDateHoliday # not found
?AddNthWeekdayInMonthHoliday # not found
?AddLastWeekdayInMonthHoliday # not found


summary(model_components <- AddSeasonal(model_components, y = claims, nseasons  = 52))


# Fitting a model
fit <- bsts(claims, model_components, niter = 2000)

burnin <- 500 # Throw away first 500 
tibble(
  date = as.Date(time(claims)),
  trend = colMeans(fit$state.contributions[-(1:burnin),"trend",]),
  seasonality = colMeans(fit$state.contributions[-(1:burnin),"seasonal.52.1",])) %>%
  gather("component", "value", trend, seasonality) %>%
  ggplot(aes(x = date, y= value)) + 
  geom_line() + theme_bw() + 
  theme(legend.title = element_blank()) + ylab("") + xlab("") +
  facet_grid(component ~ ., scales="free") + guides(colour=FALSE) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))



# Prediction with a 90% credible interval
pred <- predict(fit, horizon = 100, burn = burnin, quantiles = c(.05, .95))
plot(pred)



# Check Errors
errors <- bsts.prediction.errors(fit, burn = 1000)
PlotDynamicDistribution(errors$in.sample)




# Adding Regressors to the Model
# The dataset also includes time series that correlate strongly with initial claims. bsts will combine the trend and seasonal components with a regression on these other time series.

fit2 <- bsts(iclaimsNSA ~ ., state.specification = model_components, data = initial.claims, niter = 1000)

colMeans(fit2$coefficients)

