
# Study Material for Time series and ARIMA, SARIMA modelling

# 1. Time Series Analysis in Python – A Comprehensive Guide with Examples:-
#   https://www.machinelearningplus.com/time-series/time-series-analysis-python/
#   
# 2. ARIMA Model – Complete Guide to Time Series Forecasting in Python:-
#   https://www.machinelearningplus.com/time-series/arima-model-time-series-forecasting-python/


AirPassengers

data.table::fwrite(data.frame(period = as.Date(as.yearmon(time(AirPassengers))), y = as.matrix(AirPassengers)),file = "airpassengers.csv")

getwd()
