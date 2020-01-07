library(fArma)
library(quantmod)
library(xts)
library(zoo)
library(pracma)
library(tseries)
library(TTR)
library(dplyr)
library(bizdays)
library(lubridate)


as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

# select the dates
begin = "2014-11-30"
end = "2019-11-30"

# get the number of trading days
Dates_begin <- as.Date(begin) 
Dates_end <- as.Date(end)
nyse <- timeDate::holidayNYSE(2000:year(Sys.Date()) +1)
NYcal <- create.calendar(name='NYSE', holidays=nyse, weekdays=c('saturday', 'sunday'))
bizdays.options$set(default.calendar='NYSE')
Num_Days <- bizdays(Dates_begin,Dates_end,NYcal)

runTest <- function(ticker, beginDate, endDate){
  # get the symbol
  ticker_data <- tryCatch(getSymbols(ticker,src="yahoo",from = beginDate, to = endDate, auto.assign = getOption('getSymbols.auto.assign',FALSE)),
                    error = function(e) {print("ERROR: Can't download the Data")})
  
  # need to get the weekdays only 1489
  #difftime(end ,begin , units = c("days"))
  
  #print(dim(ticker_data)[1])
  # if there is no error downloading data and the ticker has all the trading days needed then run the script
  if((!(ticker_data == "ERROR: Can't download the Data"))){
    if(dim(ticker_data)[1] == Num_Days){
      # just keep the close (on the fourth column)
      ticker_close <- ticker_data[,4]
      
      # select the period we want to keet
      ticker_close <- ticker_close[index(ticker_close) >= beginDate & index(ticker_close) <= endDate]
      
      # check for nan
      ticker_close[is.na(ticker_close),]
      
      # backfill missing data
      ticker_close <- na.locf(ticker_close)
      
      # calculate and store some variables
      adf <- tseries::adf.test(ticker_close,k = trunc((length(ticker_close)-1)^(1/3)))$p.value
      adfZeroK <- tseries::adf.test(ticker_close, k=0)$p.value
      hurstList <-  hurstexp(ticker_close, display = FALSE)
      Hal <- hurstList$Hal
      Ht <- hurstList$Ht
      
      # unused stats
      kpssTrend <- tseries::kpss.test(ticker_close, null="Trend", lshort = FALSE)$p.value
      kpssLevel <- tseries::kpss.test(ticker_close, null="Level", lshort = FALSE)$p.value
      aggvarhurst <- aggvarFit(ticker_close)@hurst$H
      higuchihurst <- higuchiFit(ticker_close, doplot=TRUE)@hurst$H
      rshurst <- rsFit(ticker_close)@hurst$H
      
      subtitle = paste0("ADF: ",adfZeroK, " Higuchi: ", higuchihurst)
      title = paste0(ticker)
      
      # plot close data 
      plot(index(ticker_close), ticker_close[,1],type = "l", main = title , sub = subtitle, xlab = "Dates", ylab = "Close Price")
      
      #print("yes")
      return(c(adf, adfZeroK, Hal, kpssLevel, kpssTrend, aggvarhurst, higuchihurst, rshurst))
    }
    else
    {
      #print("no")
      return(c("na","na","na","na","na","na","na","na"))
    }
  }
  #  else return NAs for everything we can drop them later
  else 
  {
    #print("no")
    return(c("na","na","na","na","na","na","na","na"))
  }
}

# get a data frame for all ticker symbols in the NASDAQ
symbol_data <- stockSymbols(exchange = "NASDAQ")
# remove the indexes
symbol_data <- na.omit(symbol_data)

# get a random sample
sampled_data <- sample_n(symbol_data, 50)

# get only the tickers
symbol_list <- sampled_data$Symbol
#symbol_list <- symbol_data$Symbol



# create a closure with the dates we want for the test
listTest <- function(data){
  results <- runTest(data, begin, end)
  return(c(data,results))}


# empty list to store stats
statslist = list()


# loop through the symbols get the data and store in a list of data frames
for( i in 1:length(symbol_list)){
  stats <- listTest(symbol_list[i])
  statsdf <-data.frame(stats[1],stats[2],stats[3],stats[4],stats[5],stats[6],stats[7],stats[8],stats[9])
  statslist[[i]] <- statsdf
}

# data frame to store the scan results
filter_data = do.call(rbind, statslist)
columnNames <- c("Symbol","ADF_P_value", "ADF_Zero","Hurst_Exponent","KPSS_Level","KPSS_Trend","aggvarHurst","higuchiHurst","rsHurst")
colnames(filter_data) <- columnNames
filter_data[ filter_data == "na" ] <- NA
filter_data <- na.omit(filter_data)

# plot relevant results
x <- as.numeric.factor(filter_data$ADF_P_value)
y <- as.numeric.factor(filter_data$higuchiHurst)
plot(x,y, xlab = "ADF", ylab = "HURST", main = " HURST by ADF")