library(fArma)
library(quantmod)
library(xts)
library(zoo)
library(pracma)
library(fractal)
library(urca)
library(aTSA)
library(tseries)
library(ggplot2)

# get the symbol
ticker_data = getSymbols("GOOG",src="yahoo", from = "2000-01-01", to = "2013-01-01",auto.assign = getOption('getSymbols.auto.assign',FALSE))
# print it out
kable(tail(ticker_data))

# just keep the close (on the fourth column)
ticker_close <- ticker_data[,4]

# select the period we want to keet
ticker_close <- ticker_close[index(ticker_close) >= "2018-12-30" & index(ticker_close) <= "2019-11-30"]

# check for nan
ticker_close[is.na(ticker_close),]

# backfill missing data
ticker_close <- na.locf(ticker_close)
plot(index(ticker_close), ticker_close[,1])

periodicHurst <- function(data,period){
  loopLength <- length(data) - period
  ticker_close$Hurst <- 1:length(data)
  hurst_list <- vector(mode = "list", length = length(data))
  for( i in 1:loopLength){
    hurstList <-  hurstexp(data[i:(i+period)], display = FALSE)
    hurst_list[i+period] <- hurstList$Hal
  }
  
  for( i in 1:period){
    hurst_list[i] <- 0
  }
  return(as.numeric(unlist(hurst_list)))
}

hurstFrame <- data.frame("Close"= ticker_close$GOOG.Close, "Hurst" =  Hlist)

Hlist <- periodicHurst(ticker_close, 7)
ticker_close$Hurst <- Hlist

par(mfrow=c(2,1))
plot(index(ticker_close),ticker_close$GOOG.Close)
plot(index(ticker_close),ticker_close$Hurst)

layout
penghurst = pengFit(ticker_close)@hurst$H
aggvarhurst = aggvarFit(ticker_close)@hurst$H
diffvarhurst = diffvarFit(ticker_close)@hurst$H
absvalhurst = absvalFit(ticker_close)@hurst$H
higuchihurst = higuchiFit(ticker_close)@hurst$H
rshurst = rsFit(ticker_close)@hurst$H
perhurst = perFit(ticker_close)@hurst$H
#whittlehurst = whittleFit(ticker_close)@hurst$H
boxperhurst = boxperFit(ticker_close)@hurst$H

pracmathurst <- hurstexp(ticker_close)

roverHurst <- RoverS(ticker_close)
whittleHurst <- FDWhittle(ticker_close)

print(paste0("peng : ",penghurst))
print(paste0("aggvar : ",aggvarhurst))
print(paste0("diffvar : ",diffvarhurst))
print(paste0("absval : ",absvalhurst))
print(paste0("higuchi : ",higuchihurst))
print(paste0("rs : ",rshurst))
print(paste0("per : ",perhurst))
#print(paste0(whittlehurst))
print(paste0("boxper : ",boxperhurst))

tseries::adf.test(ticker_close, k=0)
tseries::kpss.test(ticker_close, null="Trend")

wn <- rnorm(100)
tseries::adf.test(wn)
tseries::kpss.test(wn, null="Trend")
