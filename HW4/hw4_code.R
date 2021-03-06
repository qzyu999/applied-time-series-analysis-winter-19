library(xlsx) # load data
nasa <- read.xlsx('GlobalTemp_NASA.xlsx', sheetIndex = 1)

# (a) Obtain the loess estimate of the trend with span=0.25. Plot the series along
# with the loess estimate of the trend on the same graph.
loess_fit <- loess(Temp.Anomaly~Year, span = 0.25, data = nasa)
time_length <- 1:dim(nasa)[1]

plot(time_length, nasa$Temp.Anomaly, type='l', 
     main = 'NASA Global Annual Temperature Annomalies (1880-2017)',
     ylab = 'Temperature Annomaly (C)', xlab = 'Date')
points(time_length, loess_fit$fitted, type='l', lty=2)
legend(1, 0.7, c('Temperature Annomaly', 'Loess'), lty = c(1,2), bty = 'n')

# (b) Obtain estimate of the rough part and plot it against time. Comment.
plot(time_length, loess_fit$residuals, type = 'l', main = 'Loess: Rough',
     ylab = 'Rough', xlab = 'Time')

# (c) Obtain the ACF and PACF plots of the rough part. What is your tentative
# conclusion about the type of model that may be suitable for the series
acf(loess_fit$residuals, lag.max = 10, main = 'Autocorrelation Function')
acf(loess_fit$residuals, lag.max = 10, main = 'Autocorrelation Function', plot = FALSE)
pacf(loess_fit$residuals, lag.max = 10, main = 'Partial Autocorrelation Function')
pacf(loess_fit$residuals, lag.max = 10, main = 'Partial Autocorrelation Function', plot = FALSE)

library(forecast)
auto.arima(loess_fit$residuals)

# (d) Fit AR(p) models to X_t, p=0,...,5. Select the appropriate
# one by using the AICC criterion.
source('aicc.R')

aicc(arima(loess_fit$residuals, order = c(0,0,0), include.mean = FALSE))
aicc(arima(loess_fit$residuals, order = c(1,0,0), include.mean = FALSE))
aicc(arima(loess_fit$residuals, order = c(2,0,0), include.mean = FALSE))
aicc(arima(loess_fit$residuals, order = c(3,0,0), include.mean = FALSE))
aicc(arima(loess_fit$residuals, order = c(4,0,0), include.mean = FALSE))
aicc(arima(loess_fit$residuals, order = c(5,0,0), include.mean = FALSE)) # smallest

# (e) For the selected model in part (d), obtain the parameter estimates and their
# standard errors. Plot the residuals. Also obtain the plots of ACF and PACF
# of the residuals. Do these plots suggest that the model selected in part (d) a
# reasonable one? Explain.
ar5 <- arima(loess_fit$residuals, order = c(5,0,0), include.mean = FALSE)
plot(ar5$residuals, main = 'AR(5) Residuals', ylab = 'Rough')
acf(ar5$residuals, main = 'AR(5) ACF') # good, b/c white noise
pacf(ar5$residuals, main = 'AR(5) PACF')

# aicc(arima(loess_fit$residuals, order = c(2,0,1), include.mean = FALSE))

# 2.  For each of the following models, generate a mean zero random series of
# length n = 400 with 2 = 1 (R function íarima.simí), plot the data, and obtain
# the ACF and PACF plots. Comment on the results in each case.
set.seed(137)
# a) MA(1), theta=-0.8
MA1 <- arima.sim(n = 400, model = list(ma = c(-0.8)), sd = 1)
par(mfrow=c(3,1))
plot(MA1, main = 'MA(1) theta=-0.8')
acf(MA1, main = 'MA(1) theta=-0.8 ACF')
acf(MA1, main = 'MA(1) theta=-0.8 ACF', plot = FALSE)
pacf(MA1, main = 'MA(1) theta=-0.8 PACF')
pacf(MA1, main = 'MA(1) theta=-0.8 PACF', plot = FALSE)


# b) MA(2), theta1=-0.8, theta2=0.6
MA2 <- arima.sim(n = 400, model = list(ma = c(-0.8, 0.6)), sd = 1)
plot(MA2, main = 'MA(2) theta1=-0.8, theta2=0.6')
acf(MA2, main = 'MA(2) theta1=-0.8, theta2=0.6 ACF')
acf(MA2, main = 'MA(2) theta1=-0.8, theta2=0.6 ACF', plot = FALSE)
pacf(MA2, main = 'MA(2) theta1=-0.8, theta2=0.6 PACF')
pacf(MA2, main = 'MA(2) theta1=-0.8, theta2=0.6 PACF', plot = FALSE)

# c) AR(2) phi1=0.6, phi2=-0.8
AR2 <- arima.sim(n = 400, model = list(ar = c(0.6, -0.8)), sd = 1)
plot(AR2, main = 'AR(2) phi1=0.6, phi2=-0.8')
acf(AR2, main = 'AR(2) phi1=0.6, phi2=-0.8 ACF')
acf(AR2, main = 'AR(2) phi1=0.6, phi2=-0.8 ACF', plot = FALSE)
pacf(AR2, main = 'AR(2) phi1=0.6, phi2=-0.8 PACF')

# d) AR(2) phi1=-0.4, phi2=0.4
AR2b <- arima.sim(n = 400, model = list(ar = c(-0.4, 0.4)), sd = 1)
plot(AR2b, main = 'AR(2) phi1=-0.4, phi2=0.4')
acf(AR2b, main = 'AR(2) phi1=-0.4, phi2=0.4 ACF')
acf(AR2b, main = 'AR(2) phi1=-0.4, phi2=0.4 ACF')
pacf(AR2b, main = 'AR(2) phi1=-0.4, phi2=0.4 PACF', plot = FALSE)

# e) ARMA(1,2) phi=-0.6, theta1=-0.8, theta2=0.6
ARMA12 <- arima.sim(n = 400, model = list(ar = c(-0.6), ma = c(-0.8, 0.6)), sd = 1)
plot(ARMA12, main = 'ARMA(1,2) phi=-0.6, theta1=-0.8, theta2=0.6')
acf(ARMA12, main = 'ARMA(1,2) phi=-0.6, theta1=-0.8, theta2=0.6 ACF')
acf(ARMA12, main = 'ARMA(1,2) phi=-0.6, theta1=-0.8, theta2=0.6 ACF', plot = FALSE)
pacf(ARMA12, main = 'ARMA(1,2) phi=-0.6, theta1=-0.8, theta2=0.6 PACF')