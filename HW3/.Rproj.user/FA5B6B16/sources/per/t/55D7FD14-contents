# 1. For each of the following models, generate a random series of length n = 250 
# and sigma^2 = 1, plot the data and obtain the ACF (with 20 lags). Comment on the results 
# in each case. 
# (a) MA(1), theta1 = -0.7
set.seed(137)
simMA1 <- arima.sim(n = 250, model = list(ma = c(-0.7)))
simMA1b <- arima.sim(n = 250, model = list(ma = c(0.7)))

plot(simMA1, main = 'MA(1) theta = -0.7', ylab = 'MA(1)')
acfMA1 <- acf(simMA1, lag.max = 20, main = 'MA(1) theta = -0.7')

#theoretical value
theta1 = -0.7
theta1 / (1+theta1^2)

# (b) MA(2), theta1 = 1, theta2 = 0.8
set.seed(123)
simMA2 <- arima.sim(n = 250, model = list(ma = c(1, 0.8)))
plot(simMA2, main = 'MA(2) theta1 = 1, theta2 = 0.8', ylab = 'MA(2)')
acfMA2 <- acf(simMA2, lag.max = 20, main = 'MA(2) theta1 = 1, theta2 = 0.8')

# theoretical value
theta1 = 1; theta2 = 0.8
theta1*(1+theta2) / (1 + theta1^2 + theta2^2)
theta2 / (1 + theta1^2 + theta2^2)

# 2. Let Xbar be the sample mean of an MA(1) sequence. For the parts below, assume that sigma^2 = 9
# and the sample size n = 90. 
# a
# j=0
9 + ((-.7)^2)*9 # 13.41

# j=1
-.7*9 # -6.3

# rho(1)
-.7*9 / (9 + ((-.7)^2)*9) # -0.4697987

# b / c
(13.41 + 2*((1-1/90)*(-6.3))) / 90 # theta=-0.7
(13.41 + 2*((1-1/90)*(6.3))) / 90 # theta=0.7

35.2 - (1.96 * sqrt((13.41 + 2*((1-1/90)*(6.3))))) / sqrt(90)
35.2 + (1.96 * sqrt((13.41 + 2*((1-1/90)*(6.3))))) / sqrt(90)

# 3. Let X_t be an AR(1) sequence with sigma^2 = 9. Let Xbar be the sample mean 
# with the sample size n = 90
# a
(9 / (1+0.7)^2) / 90

# b
(9 / (1-0.7)^2) / 90