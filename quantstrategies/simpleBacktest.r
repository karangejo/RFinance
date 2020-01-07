library(quantmod)
library(xts)
library(zoo)
library(PerformanceAnalytics)
library(knitr)

# get the symbol
getSymbols("^NSEI",src="yahoo")
# print it out
kable(tail(NSEI))

# just keep the close
nifty <- NSEI[,"NSEI.Close"]

# select the period we want to keet
nifty <- nifty[index(nifty) >= "2016-07-01" & index(nifty) <= "2017-06-30"]

# check for nan
nifty[is.na(nifty),]

# backfill missing data
nifty <- na.locf(nifty)

# get returns
ret_nifty <- Delt(nifty)

# get some indicators for the strategy
macd <- MACD(nifty, nFast = 12, nSlow = 26, nSig = 9, maType = "SMA", percent = FALSE)
rsi <- RSI(nifty, n = 14, maType = "SMA")

# get the trading signals according to strategy
# strategy will buy if macdsignal is greater than macd and rsi is greater than 60
# and will sell if macdsignal is less than macd and rsi is less than 40
signal <- NULL
signal <-
  ifelse((macd$signal > macd$macd) & (rsi$rsi > 60),
         1,
         ifelse((macd$signal < macd$macd) & (rsi$rsi < 40),
                -1,
                0))
# fill na values with zero
signal[is.na(signal)] <- 0

# we must apply our strategy the next trading day so we add a lag k = 1 day
trade_return <- ret_nifty*lag(signal, k = 1, na.pad = FALSE)

# calculate some performance metrics
cumm_return <- Return.cumulative(trade_return)
annual_return <- Return.annualized(trade_return)

# plot equity curve
charts.PerformanceSummary(trade_return)

# summary of the trade returns
summary(as.ts(trade_return))

print(paste0("Maximum Drawdown : ", maxDrawdown(trade_return)))
print(paste0("Sharpe Ratio : ", SharpeRatio(as.ts(trade_return), Rf = 0, p = 0.95, FUN = "StdDev")))
print(paste0("Annualized Sharpe Ratio : ", SharpeRatio.annualized(trade_return, Rf = 0)))
print(paste0("Calmar Ratio : ", CalmarRatio(trade_return, scale = 252)))
