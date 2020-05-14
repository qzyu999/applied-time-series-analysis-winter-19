library(xlsx) # load data
uscrude <- read.xlsx('USCrude.xlsx', sheetIndex = 1)
colnames(uscrude) <- c('Date', 'Barrels')

# 1a
# fit the loess model
loess_fit <- loess(Barrels~Date, span = 0.25, data = uscrude)
time_length <- 1:118

# plot the data with loess fit
plot(time_length, uscrude$Barrels, type='l', main = 'U.S. Field Production of Crude Oil',
     ylab = 'Thousands of Barrels per Day', xlab = 'Date')
points(time_length, loess_fit$fitted, type='l', lty=2)
legend(1, 8000, c('Barrels per day (thousands)', 'Loess'), lty = c(1,2), bty = 'n')

# plot the rough
plot(time_length, loess_fit$residuals, type = 'l', main = 'Loess: Rough',
     ylab = 'Rough', xlab = 'Time')

# R^2
1-(sum(loess_fit$residuals^2) / sum((uscrude$Barrels - mean(uscrude$Barrels))^2))

# 1b
# fit the log loess model
log_loess_fit <- loess(log(Barrels)~Date, span = 0.25, data = uscrude)

# plot the data with log loess fit
plot(time_length, log(uscrude$Barrels), type='l', main = 'Log U.S. Field Production of Crude Oil',
     ylab = 'Log Thousands of Barrels per Day', xlab = 'Date')
points(time_length, log_loess_fit$fitted, type='l', lty=2)
legend(60, 6, c('Log Barrels per day (thousands)', 'Loess'), lty = c(1,2), bty = 'n')

# plot the rough
plot(time_length, log_loess_fit$residuals, type = 'l', main = 'Loess: Rough (log)',
     ylab = 'Rough', xlab = 'Time')

# R^2
1-(sum(log_loess_fit$residuals^2) / sum((log(uscrude$Barrels) - mean(log(uscrude$Barrels)))^2))

# 1c
# histogram rough
par(mfrow=c(2,1))
hist(log_loess_fit$residuals, freq = FALSE, 
     xlab = 'Rough', main = 'Histogram: Log Loess rough', breaks = 20)

# QQ-plot
qqnorm(log_loess_fit$residuals); qqline(log_loess_fit$residuals)
qq <- qqnorm(log_loess_fit$residuals)
cor(qq$x, qq$y)

# Shaprio-Wilk test
shapiro.test(log_loess_fit$residuals)

# 1d
# ACF of rough
acf(log_loess_fit$residuals, lag.max = 10, main = 'Autocorrelation Function')

# 1e
# Ljung-Box test
Box.test(log_loess_fit$residuals, lag = 10, type = "Ljung-Box")

acf_result = acf(uscrude$Barrels, lag = 10)
acf_val = as.numeric(acf_result$acf)
n = length(uscrude$Barrels)
n*(n+2)*sum(acf_val[-1]^2/(n-1:10))

# 2a
# AR(1) autocorrelation
par(mfrow=c(2,1))
jmax <- 6
lags <- 0:jmax
rhos <- ARMAacf(ar = c(0.6), lag.max = jmax)
rhos <- rhos[-1]
plot(lags, rhos, pch = 19, xlab = "Lag", ylab = "ACF", ylim = c(-0.01, 0.6), 
     main = 'Autocorrelation Function at phi = 0.6')
segments(x0 = lags, y0 = 0, x1 = lags, y1 = rhos)
abline(h = 0)
rhos2 <- ARMAacf(ar = c(-0.6), lag.max = jmax)
rhos2 <- rhos2[-1]
plot(lags, rhos2, pch = 19, xlab = "Lag", ylab = "ACF", 
     main = 'Autocorrelation Function at phi = -0.6')
segments(x0 = lags, y0 = 0, x1 = lags, y1 = rhos2)
abline(h = 0)

# 2b
# MA(1) autocorrelation
rhos3 <- ARMAacf(ma = c(0.6), lag.max = jmax)
rhos3 <- rhos3[-1]
plot(lags, rhos3, pch = 19, xlab = "Lag", ylab = "ACF", ylim = c(-0.01, 0.6), 
     main = 'Autocorrelation Function at phi = 0.6')
segments(x0 = lags, y0 = 0, x1 = lags, y1 = rhos3)
abline(h = 0)
rhos4 <- ARMAacf(ma = c(-0.6), lag.max = jmax)
rhos4 <- rhos4[-1]
plot(lags, rhos4, pch = 19, xlab = "Lag", ylab = "ACF", 
     main = 'Autocorrelation Function at phi = -0.6')
segments(x0 = lags, y0 = 0, x1 = lags, y1 = rhos4)
abline(h = 0)