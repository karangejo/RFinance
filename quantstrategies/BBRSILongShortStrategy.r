# load required libraries
library(quantstrat)
library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(htmltools)
library(htmlwidgets)
library(knitr)
library(lattice)
library(pander)
library(tidyr)
library(webshot)

sessionInfo()

# settings
Sys.setenv(TZ = "UTC")
currency('USD')

init_date <- "2010-12-31"
start_date <- "2012-01-01"
end_date <- "2018-12-31"
init_equity <- 1e4 # $10,000
adjustment <- TRUE

# basic functions to load symbol list into symbols variable
basic_symbols <- function() {
  symbols <- c(
    "IWM", # iShares Russell 2000 Index ETF
    "QQQ", # PowerShares QQQ TRust, Series 1 ETF
    "SPY" # SPDR S&P 500 ETF Trust
  )
}


enhanced_symbols <- function() {
  symbols <- c(
    basic_symbols(), 
    "TLT", # iShares Barclays 20+ Yr Treas. Bond ETF
    "XLB", # Materials Select Sector SPDR ETF
    "XLE", # Energy Select Sector SPDR ETF
    "XLF", # Financial Select Sector SPDR ETF
    "XLI", # Industrials Select Sector SPDR ETF
    "XLK", # Technology  Select Sector SPDR ETF
    "XLP", # Consumer Staples  Select Sector SPDR ETF
    "XLU", # Utilities  Select Sector SPDR ETF
    "XLV", # Health Care  Select Sector SPDR ETF
    "XLY" # Consumer Discretionary  Select Sector SPDR ETF
  )
}

# helper function to make sure everything is OK (portfolio and such...)
checkBlotterUpdate <- function(port.st = portfolio.st, 
                               account.st = account.st, 
                               verbose = TRUE) {
  
  ok <- TRUE
  p <- getPortfolio(port.st)
  a <- getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(
    sapply(
      syms, 
      FUN = function(x) eval(
        parse(
          text = paste("sum(p$symbols", 
                       x, 
                       "posPL.USD$Net.Trading.PL)", 
                       sep = "$")))))
  
  port.sum.tot <- sum(p$summary$Net.Trading.PL)
  
  if(!isTRUE(all.equal(port.tot, port.sum.tot))) {
    ok <- FALSE
    if(verbose) print("portfolio P&L doesn't match sum of symbols P&L")
  }
  
  initEq <- as.numeric(first(a$summary$End.Eq))
  endEq <- as.numeric(last(a$summary$End.Eq))
  
  if(!isTRUE(all.equal(port.tot, endEq - initEq)) ) {
    ok <- FALSE
    if(verbose) print("portfolio P&L doesn't match account P&L")
  }
  
  if(sum(duplicated(index(p$summary)))) {
    ok <- FALSE
    if(verbose)print("duplicate timestamps in portfolio summary")
    
  }
  
  if(sum(duplicated(index(a$summary)))) {
    ok <- FALSE
    if(verbose) print("duplicate timestamps in account summary")
  }
  return(ok)
}

# load the symbol list into the variable
symbols <- enhanced_symbols()

# get the symbols
getSymbols(Symbols = symbols, 
           src = "yahoo", 
           index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = adjustment)

# set some settings for our model
stock(symbols, 
      currency = "USD", 
      multiplier = 1)

# name our strategy appropriately
portfolio.st <- "Port.BBRSI"
account.st <- "Acct.BBRSI"
strategy.st <- "Strat.BBRSI"

# remove and clean any previous data
rm.strat(portfolio.st)
rm.strat(account.st)

# initialize portfolio
initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)

# initialize account
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)

# initialize orders
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)

# initialize strategy
strategy(strategy.st, store = TRUE)

# add some indicators to the strategy

# add an rsi
add.indicator(strategy = strategy.st,
              name = "RSI",
              arguments = list(price= quote(Cl(mktdata)), 
                               n = 14),
              label = "rsi")

# add bollinger bands
add.indicator(strategy = strategy.st, 
              name = "BBands", 
              arguments = list(HLC = quote(HLC(mktdata)), 
                               n = 20, 
                               maType = "SMA", 
                               sd = 1), 
              label = "BB.20.2")

# add some signals to our strategy
# enter when the close is greater than the lower bollinger band and the RSI is less than 30
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns=c("Close","dn","rsi"),
                            formula = "(Close < dn) & (rsi > 30)"),
           label = "long")

# exit when the close crosses the middle bollinger band
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("Close", "mavg"),
                            relationship = "gte"),
           label = "exitlong")

# short when close goes above upper bollinger band and rsi is greater than 70
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns=c("Close","up","rsi"),
                            formula = "(Close > up) & (rsi < 70)"),
           label = "short")

# exit when the close crosses the middle bollinger band
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("Close", "mavg"),
                            relationship = "lte"),
           label = "exitshort")

# add some entry rules for long and short positions
# add a rule that says if our long signal is true then we enter a long position
# we will buy 100 shares with a stop limit and create a new variable in mktdata call enterLONG
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = 100,
                          ordertype = "stoplimit",
                          orderside = "long", 
                          threshold = 0.0005,
                          prefer = "High", 
                          TxnFees = -10, 
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")

# rule to enter short positions
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -100,
                          ordertype = "stoplimit",
                          threshold = -0.005, 
                          orderside = "short", 
                          replace = FALSE, 
                          TxnFees = -10, 
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT")

# add a rule to exit long positions
add.rule(strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "exitlong", 
                          sigval = TRUE, 
                          orderside = "short", 
                          ordertype = "market", 
                          orderqty = "all", 
                          TxnFees = -10, 
                          replace = TRUE), 
         type = "exit", 
         label = "Exit2LONG")

add.rule(strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "exitshort", 
                          sigval = TRUE, 
                          orderside = "long", 
                          ordertype = "market", 
                          orderqty = "all", 
                          TxnFees = -10, 
                          replace = TRUE), 
         type = "exit", 
         label = "Exit2SHORT")

# check everything and get results

cwd <- getwd()
setwd("data/")
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  load(results_file)
} else {
  results <- applyStrategy(strategy.st, portfolios = portfolio.st)
  updatePortf(portfolio.st)
  updateAcct(account.st)
  updateEndEq(account.st)
  if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
    save(list = "results", file = results_file)
    save.strategy(strategy.st)
  }
}
setwd(cwd)