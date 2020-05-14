library(xlsx); library(astsa) # Load libraries

### 1
# a
phi1 <- 0.5253; phi2 <- -0.8551; mu <- 27.0253
phi0 <- mu*(1 - phi1 - phi2)
X_96 <- phi0 + phi1*28.499 + phi2*34.043
X_97 <- phi0 + phi1*X_96 + phi2*28.499
X_98 <- phi0 + phi1*X_97 + phi2*X_96
X_99 <- phi0 + phi1*X_98 + phi2*X_97
X_100 <- phi0 + phi1*X_99 + phi2*X_98

# b
# stationarity
-1 < phi1; phi1 < 1
-1 < phi1 / (1 - phi2); phi1 / (1 - phi2) < 1

psi0 <- 0
psi1 <- phi1
psi2 <- phi1^2 + phi2
psi3 <- phi1^3 + 2*phi1*phi2
psi4 <- phi1^4 + 3*phi1^2*phi2 + phi2^2

ARMAtoMA(ar = c(phi1, phi2), lag.max = 4)

sigma2 <- 4.636
forecast_error_1 <- sigma2
forecast_error_2 <- (1 + psi1^2)*sigma2
forecast_error_3 <-(1 + psi1^2 + psi2^2)*sigma2
forecast_error_4 <-(1 + psi1^2 + psi2^2 + psi3^2)*sigma2
forecast_error_5 <-(1 + psi1^2 + psi2^2 + psi3^2 + psi4^2)*sigma2

sink('ci.txt')
X_96 - 1.96 * sqrt(forecast_error_1); X_96 + 1.96 * sqrt(forecast_error_1)
X_97 - 1.96 * sqrt(forecast_error_2); X_97 + 1.96 * sqrt(forecast_error_2)
X_98 - 1.96 * sqrt(forecast_error_3); X_98 + 1.96 * sqrt(forecast_error_3)
X_99 - 1.96 * sqrt(forecast_error_4); X_99 + 1.96 * sqrt(forecast_error_4)
X_100 - 1.96 * sqrt(forecast_error_5); X_100 + 1.96 * sqrt(forecast_error_5)
sink()

lower_bound <- c(X_96 - 1.96 * forecast_error_1, X_97 - 1.96 * 
                   forecast_error_2, X_98 - 1.96 * forecast_error_3,
                 X_99 - 1.96 * forecast_error_4, X_100 - 1.96 *
                   forecast_error_5)
upper_bound <- c(X_96 + 1.96 * forecast_error_1, X_97 + 1.96 *
                   forecast_error_2, X_98 + 1.96 * forecast_error_3,
                 X_99 + 1.96 * forecast_error_4, X_100 + 1.96 * 
                   forecast_error_5)

time_index2 <- 1:5
obs_vec <- c(18.149, 21.720, 32.727, 34.150, 25.539)
plot(time_index2, obs_vec, ylim = c(5,55), col = 'blue', 
     main = 'Observed with Fitted Values and Confidence Intervals',
     xlab = 'Time Index', ylab = 'Values', xaxt = 'n')
axis(1, at=time_index2, labels=96:100)
pred_vec2 <- c(X_96, X_97, X_98, X_99, X_100)
points(c(X_96, X_97, X_98, X_99, X_100), col = 'blue')
lines(time_index2, pred_vec2, type = 'c', lty = 1, col = 'blue')
lines(time_index2, obs_vec, type = 'c', lty = 1, col = 'black')
lines(time_index2, lower_bound, type = 'c', lty = 2, col = 'red')
lines(time_index2, upper_bound, type = 'c', lty = 2, col = 'red')
legend('topleft', legend = c('Observed', 'Fitted', '95% Confidence Interval'), 
       col = c('black', 'blue', 'red'), lty = c(1,1,2))

### 2
# a
temp_data <- read.xlsx('GlobalTemp_NASA.xlsx', sheetIndex = 1)
time_index <- 1:dim(temp_data)[1]
par(mfrow=c(2,1))
plot(time_index, temp_data$Temp.Anomaly, type='l', 
     main = 'Global Annual Temperature Annomalies (1880-2017)',
     ylab = 'Temperature Annomaly (C)', xlab = 'Date')
plot(time_index[-1] - 1, diff(temp_data$Temp.Anomaly), type='l', 
     main = 'First Differenced Global Annual Temperature Annomalies (1880-2017)',
     ylab = 'First Differenced Temperature Annomaly (C)', xlab = 'Date')
dev.off()

# acf/pacf
acf(diff(temp_data$Temp.Anomaly), main = 'ACF of Differenced Series')
pacf(diff(temp_data$Temp.Anomaly), main = 'PACF of Differenced Series')

# b
ma_index <- 1
ar_index <- 1
sarima_data1 <- sarima(xdata = temp_data$Temp.Anomaly, p = ar_index, d = 1, q = 1)
sarima_data2 <- sarima(xdata = temp_data$Temp.Anomaly, p = ar_index, d = 1, q = 2)
sarima_data3 <- sarima(xdata = temp_data$Temp.Anomaly, p = ar_index, d = 1, q = 3)
ar_index <- 2
sarima_data4 <- sarima(xdata = temp_data$Temp.Anomaly, p = ar_index, d = 1, q = 1)
sarima_data5 <- sarima(xdata = temp_data$Temp.Anomaly, p = ar_index, d = 1, q = 2)
sarima_data6 <- sarima(xdata = temp_data$Temp.Anomaly, p = ar_index, d = 1, q = 3)
ar_index <- 3
sarima_data7 <- sarima(xdata = temp_data$Temp.Anomaly, p = ar_index, d = 1, q = 1)
sarima_data8 <- sarima(xdata = temp_data$Temp.Anomaly, p = ar_index, d = 1, q = 2)
sarima_data9 <- sarima(xdata = temp_data$Temp.Anomaly, p = ar_index, d = 1, q = 3)

aicc <- c(sarima_data1$AICc, sarima_data2$AICc, sarima_data3$AICc, sarima_data4$AICc,
  sarima_data5$AICc, sarima_data6$AICc, sarima_data7$AICc, sarima_data8$AICc, sarima_data9$AICc)
which.min(aicc)
sink('aicc.txt'); aicc; sink()
arima(x = temp_data$Temp.Anomaly, order = c(2,1,2))
mod_res <- sarima(xdata = temp_data$Temp.Anomaly, p = 2, d = 1, q = 2)$fit$residuals
acf(mod_res, main = 'ACF of Residuals')


# c
plot(sarima_data5$fit)
arima_212 <- sarima(xdata = temp_data$Temp.Anomaly, p = 2, d = 1, q = 2)
plot(time_index, temp_data$Temp.Anomaly, type = 'l', main = 'Observed with Fitted Values',
     xlab = 'Time Index', ylab = 'Temperature Anomalies')
lines(time_index, temp_data$Temp.Anomaly - arima_212$fit$residuals, 
      col = 'red', type = 'l')
legend(x = 20, y = 0.5, legend = c('Observed', 'Fitted'), col = c('black', 'red'), 
       lty = 1:1)

# d
sarima.for(xdata = head(temp_data$Temp.Anomaly, -5), n.ahead = 5, p = 2, d = 1, q = 2)
pred_val <- sarima.for(xdata = head(temp_data$Temp.Anomaly, -5), n.ahead = 5, p = 2, d = 1, q = 2)

plot(time_index, temp_data$Temp.Anomaly, main = 'Predicted with Actual Temperatures Anomalies',
     xlab = 'Time Index', ylab = 'Temperature Anomalies', type = 'l')
points(temp_data$Temp.Anomaly)
pred_vec <- c(rep(NA, length(time_index) - 5), pred_val$pred[1:5])
points(time_index, pred_vec, col = 'red', pch = 22)
se_vec <- c(rep(NA, length(time_index) - 5), pred_val$se[1:5])
points(time_index, pred_vec + se_vec, col = 'blue')
points(time_index, pred_vec - se_vec, col = 'blue')
# points(time_index, pred_vec + 1.96*se_vec, col = 'blue')
# points(time_index, pred_vec - 1.96*se_vec, col = 'blue')
legend("topleft", legend = c('Observed', 'Predicted', 'Standard Errors'), 
       col = c('black', 'red', 'blue'), pch = c(1,7,1), lty = c(1, rep(NA, 2)))
