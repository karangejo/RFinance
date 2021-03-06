t = 0:300
y_stationary <- rnorm(length(t),mean=1,sd=1) # the stationary time series (ts)
y_trend      <- cumsum(rnorm(length(t),mean=1,sd=4))+t/100 # our ts with a trend
# lets normalize each for simplicity
y_stationary<- y_stationary/max(y_stationary) 
y_trend      <- y_trend/max(y_trend) 

plot.new()
frame()
par(mfcol=c(2,2))
# the stationary signal and ACF
plot(t,y_stationary,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Y(t)",
     main = "Stationary signal")
acf(y_stationary,lag.max = length(y_stationary),
    xlab = "lag #", ylab = 'ACF',main=' ')
# the trend signal and ACF
plot(t,y_trend,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Y(t)",
     main = "Trend signal")
acf(y_trend,lag.max = length(y_trend),
    xlab = "lag #", ylab = 'ACF', main=' ')