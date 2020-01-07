library(quantmod)
library(xts)
library(zoo)
library(PerformanceAnalytics)
library(knitr)
library(plotly)

# function to backtest the MACDRSI strategy
MACDRSIstrategyBacktest <- function(ticker, beginDate, endDate){
  # get the symbol
  ticker_data = getSymbols(ticker,src="yahoo",auto.assign = getOption('getSymbols.auto.assign',FALSE))

  # just keep the close (on the fourth column)
  ticker_close <- ticker_data[,4]
  
  # select the period we want to keet
  ticker_close <- ticker_close[index(ticker_close) >= beginDate & index(ticker_close) <= endDate]
  
  # check for nan
  ticker_close[is.na(ticker_close),]
  
  # backfill missing data
  ticker_close <- na.locf(ticker_close)
  # plot close data should probably plot candlesticks and the indicators
  plot(index(ticker_close), ticker_close[,1],type = "l", main = ticker)
  names(ticker_data) <- c("open", "high", "low", "close","volume", "adjusted")
  ticker_data = Date=index(AAPL)
  p <- plot_ly(x = ticker_data, type="candlestick",
            open = ~open, close = ~close,
            high = ~high, low = ~low) %>%
    layout(title = "Basic Candlestick Chart",
           xaxis = list(rangeslider = list(visible = F)))
  
  p
  
  # get returns
  ticker_return <- Delt(ticker_close)
  
  # get some indicators for the strategy
  macd <- MACD(ticker_close, nFast = 12, nSlow = 26, nSig = 9, maType = "SMA", percent = FALSE)
  rsi <- RSI(ticker_close, n = 14, maType = "SMA")
  
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
  trade_return <- ticker_return*lag(signal, k = 1, na.pad = FALSE)
  
  # print some information
  print(ticker)
  print(paste0(beginDate," : ", endDate))
  
  # calculate some performance metrics
  cumm_return <- Return.cumulative(trade_return)
  print(cumm_return)
  annual_return <- Return.annualized(trade_return)
  print(annual_return)
  
  # plot equity curve
  charts.PerformanceSummary(trade_return,main=ticker)
  
  # summary of the trade returns
  print("Summary of trade returns:")
  print(summary(as.ts(trade_return)))
  
  
  print(paste0("Maximum Drawdown : ", maxDrawdown(trade_return)))
  print(paste0("Sharpe Ratio : ", SharpeRatio(as.ts(trade_return), Rf = 0, p = 0.95, FUN = "StdDev")))
  print(paste0("Annualized Sharpe Ratio : ", SharpeRatio.annualized(trade_return, Rf = 0)))
  print(paste0("Calmar Ratio : ", CalmarRatio(trade_return, scale = 252)))
  
}

# select the symbols we want to test and the dates
symbol_list = c("HOG", "AAPL", "AMZN")
begin = "2018-12-30"
end = "2019-11-30"

# create a closure with the dates we want for the test
backtest <- function(data){
  MACDRSIstrategyBacktest(data, begin, end)
}

# apply the backtest to each ticker symbol in our list
lapply(symbol_list, backtest)

