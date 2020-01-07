results <- applyStrategy(strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)

# for(symbol in symbols) {
 # chart.Posn(portfolio.st, Symbol = symbol, 
#             TA = "add_SMA(n = 10, col = 4); add_SMA(n = 30, col = 2)")
#}

tstats <- tradeStats(portfolio.st)
kable(t(tstats))

rets <- PortfReturns(Account = account.st)
rownames(rets) <- NULL
charts.PerformanceSummary(rets, colorset = bluefocus)

tab.perf <- table.Arbitrary(rets,
                            metrics=c(
                              "Return.cumulative",
                              "Return.annualized",
                              "SharpeRatio.annualized",
                              "CalmarRatio"),
                            metricsNames=c(
                              "Cumulative Return",
                              "Annualized Return",
                              "Annualized Sharpe Ratio",
                              "Calmar Ratio"))
kable(tab.perf)

tab.trades <- tstats %>% 
  mutate(Trades = Num.Trades, 
         Win.Percent = Percent.Positive, 
         Loss.Percent = Percent.Negative, 
         WL.Ratio = Percent.Positive/Percent.Negative) %>% 
  select(Trades, Win.Percent, Loss.Percent, WL.Ratio)

kable(t(tab.trades))

tab.wins <- tstats %>% 
  select(Avg.Trade.PL, Avg.Win.Trade, Avg.Losing.Trade, Avg.WinLoss.Ratio)

kable(t(tab.wins))

tab.profit <- tstats %>% 
  select(Net.Trading.PL, Gross.Profits, Gross.Losses, Profit.Factor)
kable(t(tab.profit))

tab.risk <- table.Arbitrary(rets,
                            metrics=c(
                              "StdDev.annualized",
                              "maxDrawdown",
                              "VaR",
                              "ES"),
                            metricsNames=c(
                              "Annualized StdDev",
                              "Max DrawDown",
                              "Value-at-Risk",
                              "Conditional VaR"))
kable(tab.risk)



# run these separately
a <- getAccount(account.st)
xyplot(a$summary, type = "h", col = 4)

# run these separately
equity <- a$summary$End.Eq
plot(equity, main = "Equity Curve")

ret <- Return.calculate(equity, method = "log")
charts.PerformanceSummary(ret, colorset = bluefocus, main = "Strategy Performance")

# run these separately
rets <- PortfReturns(Account = account.st)
chart.CumReturns(rets, colorset = rich10equal, legend.loc = "topleft", 
                 main="SPDR Cumulative Returns")

(ar.tab <- table.AnnualizedReturns(rets))
max.risk <- max(ar.tab["Annualized Std Dev",])
max.return <- max(ar.tab["Annualized Return",])
chart.RiskReturnScatter(rets,
                        main = "SPDR Performance", colorset = rich10equal,
                        xlim = c(0, max.risk * 1.1), ylim = c(0, max.return))





