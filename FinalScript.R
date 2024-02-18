# Read the code paralelly with raport
# installing and loading necessary packages
install.packages("ggplot2")
install.packages("forecast")
install.packages("dplyr")
library("ggplot2")
library("forecast")
library("dplyr")
# first lookout on the time series
View(AirPassengers)
is.ts(AirPassengers)
deltat(AirPassengers)
time(AirPassengers)
cycle(AirPassengers)
frequency(AirPassengers)

# first visualizations
# ggplot2 needs data in data.frame type
# not Time Series type
frame <- data.frame(AirPassengers)
Time <- time(AirPassengers)
# point plot and line plot to see the behaviour
ggplot(frame) + geom_point(mapping=aes(Time, y = AirPassengers))
ggplot(frame) + geom_line(mapping=aes(Time, AirPassengers))
# monthplot and seasonplot to observe seasonability
# we also see on seasonplot
# that variation increases with time
monthplot(AirPassengers, main = "Month Plot")
seasonplot(AirPassengers, years.labels=TRUE, col = rainbow(13), main = "Season Plot")
# autocorrelation function within default range
acf <- acf(AirPassengers)
acf
# adjusting lag to 2 years after first acf plot
acf(AirPassengers, main = "Autocorrelation Function",lag = 24)
# additive decomposition into trend, season and random
AirPassengers_decomposition_add <- decompose(AirPassengers, "additive")
View(AirPassengers_decomposition_add)
# plotting trend, season and random altogether
ggplot(frame) +
  geom_line(aes(Time, AirPassengers_decomposition_add$seasonal, color = "Seasonal")) +
  geom_line(aes(Time, AirPassengers_decomposition_add$trend, color = "Trend")) +
  geom_line(aes(Time, AirPassengers_decomposition_add$random, color = "Random")) +
  scale_color_manual(values = c("green", "blue", "red"),
                     labels = c("Random", "Seasonal", "Trend")) +
  labs(color = "Components", y = "Decomposition") +
  theme_minimal()
dev.off()
# plotting BoxCox transforms to check if we should
# apply one of them
ggplot(frame) +
  geom_line(aes(Time, AirPassengers, color = "Original")) +
  geom_line(aes(Time, BoxCox(AirPassengers, lambda = 0.5), color = "BoxCox (lambda = 0.5)")) +
  geom_line(aes(Time, BoxCox(AirPassengers, lambda = 0), color = "BoxCox (lambda = 0)")) +
  scale_color_manual(values = c("blue", "green", "red"),
                     labels = c("BoxCox lambda=0", "BoxCox (lambda = 0.5)", "Original")) +
  labs(x = "Time", y = "AirPassengers", title = "BoxCox Transformation") +
  theme_minimal()
# applying BoxCox for lambda = 0
log_air <- BoxCox(AirPassengers, lambda=0)
# additive decomposition of box-coxed series
bc_decompostion_add <- decompose(log_air, "additive")
# and plotting it
ggplot(frame) +
  geom_line(aes(Time, bc_decomposition_add$seasonal, color = "Seasonal")) +
  geom_line(aes(Time, bc_decomposition_add$trend, color = "Trend")) +
  geom_line(aes(Time, bc_decomposition_add$random, color = "Random")) +
  scale_color_manual(values = c("green", "blue", "red"),
                     labels = c("Random", "Seasonal", "Trend")) +
  labs(color = "Components", y = "Decomposition") +
  theme_minimal()
# BC-transform removed seasonability
# subtracting lag 12 (assumed from first plots)
# will remove trend
# and will give us stationary series
diff_log_air <- log_air - lag(log_air, 12)
plot(diff_log_air)
# decomposition and plot of lagged Box-Coxed series
diff_bc_decomposition_add <- decompose(diff_log_air, "additive")
Diff_Time <- Time[0:132]
# decomposition of series with removed lag
ggplot(diff_log_air) +
  geom_line(aes(Diff_Time, diff_bc_decomposition_add$seasonal, color = "Seasonal")) +
  geom_line(aes(Diff_Time, diff_bc_decomposition_add$trend, color = "Trend")) +
  geom_line(aes(Diff_Time, diff_bc_decomposition_add$random, color = "Random")) +
  scale_color_manual(values = c("green", "blue", "red"),
                     labels = c("Random", "Seasonal", "Trend")) +
  labs(color = "Components", y = "Decomposition") +
  theme_minimal()
# division into training and test set
diff_log_air_train <- window(diff_log_air, end=c(1958, 12))
diff_log_air_test <- window(diff_log_air, start=c(1959, 01))
# creating three models: arima, linear trend and ets
# arima with default parameters
model1 <- auto.arima(diff_log_air_train)
summary(model1)
# linear trend + season
model2 <- tslm(diff_log_air_train ~ trend + season)
summary(model2)
# exponential smoothing
model3 <- ets(diff_log_air_train, model="ZAN")
summary(model3)
# forecasting
# forecast 0 is naive
# which means ARIMA(0,0,0)(0,1,0)12
forecast0 <- snaive(diff_log_air_train, h=24)
forecast1 <- forecast(model1, h=24)
forecast2 <- forecast(model2, h=24)
forecast3 <- forecast(model3, h=24)
# plotting all forecasts
par(mfrow=c(2,2))
legend("topleft", legend=c("real", "forecast"),
       lty= c(2,1), col = c("red", "blue"))
# Plot 1
plot(forecast0)
lines(diff_log_air_test, col="red", lty=2)
grid()
# Plot 2
plot(forecast1)
lines(diff_log_air_test, col="red", lty=2)
grid()
# Plot 3
plot(forecast2)
lines(diff_log_air_test, col="red", lty=2)
grid()
# Plot 4
plot(forecast3)
lines(diff_log_air_test, col="red", lty=2)
grid()

# comparing the models
criterions <- c("RMSE", "MAE", "MASE", "MAPE")
acc0 <- accuracy(forecast0, diff_log_air_test)[, criterions]
acc1 <- accuracy(forecast1, diff_log_air_test)[, criterions]
acc2 <- accuracy(forecast2, diff_log_air_test)[, criterions]
acc3 <- accuracy(forecast3, diff_log_air_test)[, criterions]

acc0
acc1
acc2
acc3
 



