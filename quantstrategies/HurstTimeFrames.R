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
library(gridExtra)
library(lubridate)
library(bizdays)
library(matrixStats)
library(dplyr)
library(imputeTS)

# funtion to calculate the hurst exponent for a given period in a data frame
periodicHurst <- function(data,period){
  # get the length of the loop accounting for the period
  loopLength <- length(data) - period
  # initialize an empty list to store the hurst values
  hurst_list <- vector(mode = "list", length = length(data))
  # loop through the data and get the hurst exponent and store it in the list
  for( i in 1:loopLength){
    hurstList <-  hurstexp(data[i:(i+period)], display = FALSE)
   # hurst_list[i+period] <- hurstList$Hal
    hurst_list[i+period] <- hurstList$Hs
    #hurst_list[i+period] <- higuchiFit(data[i:(i+period)])@hurst$H
  }
  # loop through list for the period and add zeros to the beggining
  for( i in 1:period){
    hurst_list[i] <- 0
  }
  # return a the values as numeric
  return(as.numeric(unlist(hurst_list)))
}

runHurst <- function(ticker, beginDate, endDate){
  
  # get the symbol
  ticker_data <- tryCatch(getSymbols(ticker,src="yahoo",from = beginDate, to = endDate, auto.assign = getOption('getSymbols.auto.assign',FALSE)),
                          error = function(e) {print("ERROR: Can't download the Data")})
  
  # if there is no error downloading data and the ticker has all the trading days needed then run the script
  if((!(ticker_data == "ERROR: Can't download the Data"))){
    if(dim(ticker_data)[1] == Num_Days){
      # just keep the close (on the fourth column)
      ticker_close <- ticker_data[,4]
      
      # select the period we want to keet
      ticker_close <- ticker_close[index(ticker_close) >= beginDate & index(ticker_close) <= endDate]
      
      # check for nan and backfill missing data
      ticker_close[is.na(ticker_close),]
      ticker_close <- na.locf(ticker_close)
      colnames(ticker_close)[1] <- "Close"
     
      # calculate hurst values at various time frames
      weeklyH <- tryCatch(periodicHurst(ticker_close, 5), error = function(e){ print("NAN"); return(NaN)})
      byWeeklyH <- tryCatch(periodicHurst(ticker_close, 10), error = function(e){ print("NAN"); return(NaN)})
      triWeeklyH <- tryCatch(periodicHurst(ticker_close, 15), error = function(e){ print("NAN"); return(NaN)})
      monthlyH <- tryCatch(periodicHurst(ticker_close, 20), error = function(e){ print("NAN"); return(NaN)})
      pentaWeeklyH <- tryCatch(periodicHurst(ticker_close, 25), error = function(e){ print("NAN"); return(NaN)})
      sextaWeeklyH <- tryCatch(periodicHurst(ticker_close, 30), error = function(e){ print("NAN"); return(NaN)})
      septaWeeklyH <- tryCatch(periodicHurst(ticker_close, 35), error = function(e){ print("NAN"); return(NaN)})
      octaWeeklyH <- tryCatch(periodicHurst(ticker_close, 40), error = function(e){ print("NAN"); return(NaN)})
      nonaWeeklyH <- tryCatch(periodicHurst(ticker_close, 45), error = function(e){ print("NAN"); return(NaN)})
      decaWeeklyH <- tryCatch(periodicHurst(ticker_close, 50), error = function(e){ print("NAN"); return(NaN)})
      decaoneWeeklyH <- tryCatch(periodicHurst(ticker_close, 55), error = function(e){ print("NAN"); return(NaN)})
      dozenWeeklyH <- tryCatch(periodicHurst(ticker_close, 60), error = function(e){ print("NAN"); return(NaN)})
      
      # store the hurst values in the data frame 
      ticker_close$Hurst1W <- weeklyH
      ticker_close$Hurst2W <- byWeeklyH
      ticker_close$Hurst3W <- triWeeklyH
      ticker_close$Hurst4W <- monthlyH
      ticker_close$Hurst5W <- pentaWeeklyH
      ticker_close$Hurst6W <- sextaWeeklyH
      ticker_close$Hurst7W <- septaWeeklyH
      ticker_close$Hurst8W <- octaWeeklyH
      ticker_close$Hurst9W <- nonaWeeklyH
      ticker_close$Hurst10W <- decaWeeklyH
      ticker_close$Hurst11W <- decaoneWeeklyH
      ticker_close$Hurst12W <- dozenWeeklyH
      
      # remove nas
      ticker_close[is.na(ticker_close)] <- 0
      # return the data frame with the hurst values
      return(ticker_close)
    }
    return(NULL)
  }
  return(NULL)
}

runTest <- function(ticker_symbol, begind, endd){
  # run the hurst calculations
  #df <- runHurst("AAPL" ,begin,end)
  df <- runHurst(ticker_symbol ,begind,endd)
 
  if(!isempty(df)){
    # calculate some means
    wholePeriodH <- hurstexp(df$Close, display = FALSE)$Hs
    W1Means <- mean(df$Hurst1W)
    W2Means <- mean(df$Hurst2W)
    W3Means <- mean(df$Hurst3W)
    W4Means <- mean(df$Hurst4W)
    W5Means <- mean(df$Hurst5W)
    W6Means <- mean(df$Hurst6W)
    W7Means <- mean(df$Hurst7W)
    W8Means <- mean(df$Hurst8W)
    W9Means <- mean(df$Hurst9W)
    W10Means <- mean(df$Hurst10W)
    W11Means <- mean(df$Hurst11W)
    W12Means <- mean(df$Hurst12W)
    
    rowmeansdf <- data.frame(c(wholePeriodH,W1Means,W2Means,W3Means,W4Means,W5Means,W6Means,W7Means,W8Means,W9Means,W10Means,W11Means,W12Means), 
                             row.names = c("Whole_Period_Hurst","1W_Mean","2W_Mean","3W_Mean","4W_Mean","5W_Mean","6W_Mean","7W_Mean","8W_Mean","9W_Mean","10W_Mean","11W_Mean","12W_Mean"))
    
    names(rowmeansdf) <- "Hurst"
    print(rowmeansdf)
    
    #df$RowMeans <- rowMeans(df[,2:7])
    #df$FibMeans <- rowWeightedMeans(df[,2:7],c(64,32,16,8,4,2,1))
   
     HurstMean <- mean(rowmeansdf$Hurst)
    WeightedHurstMean <- weighted.mean(rowmeansdf$Hurst,c(1,2,4,8,16,32,64,128,256,512,1024,2048,4096))
    
    print(paste0("Weighted Hurst Mean from all time frames is: ",WeightedHurstMean))
    
    #plot results
    pClose <- ggplot(data=df, aes(x=index(df), y=Close, group=1)) +
      geom_line() +
      ggtitle(paste0(ticker_symbol," | Weighted Mean of Hurst from all timeframes is: ", WeightedHurstMean))+
      xlab("Date")
    # pMeanHurst <- ggplot(data=df, aes(x=index(df), y=RowMeans, group=1)) +
    # geom_line()
    #pFibMeanHurst <- ggplot(data=df, aes(x=index(df), y=FibMeans, group=1)) +
    #geom_line()
    
    # grid.arrange(pClose, pMeanHurst, ncol = 1)
    
    # df$oneline <- rep(1,nrow(df))
    # df$hurstline <- rep(0.5,nrow(df))
    # df$pline <- rep(0.05,nrow(df))
    
    pHurst <- NULL
    # plot all the hurst lines
    pHurst <- ggplot(df, aes(x=index(df))) + xlab("Date") + ylab("Hurst Exponents")
    # +
    #   geom_line(aes(y = Hurst1W), color = "black") + 
    #   geom_line(aes(y = Hurst2W), color="blue") +
    #   geom_line(aes(y = Hurst3W), color="green") +
    #   geom_line(aes(y = Hurst4W), color="yellow") +
    #   geom_line(aes(y = Hurst5W), color="orange") +
    #   geom_line(aes(y = Hurst6W), color="red") +
    #   geom_line(aes(y = hurstline), color="pink")+ 
    #   geom_line(aes(y = oneline), color="pink") 
    # 
    colorGradient <- c("#4500E5","#4015E6","#3C2AEB","#383FEA","#3354EB","#2F69ED","#2B7EEF","#2793F1","#22A8F2","#1EBDF4","#1AD2F6","#16E7F8")

    for(i in 2:13){
      pHurst <- pHurst + geom_line(aes_string(y = names(df)[i]),color = colorGradient[i-1])
    }
    
    grid.arrange(pClose, pHurst, ncol = 1)
    
    # # get the ratio of readings above 0.5
    # aboveList <- NULL
    # belowList <- NULL
    # 
    # for(i in 2:13){
    #   above <- (length(df[df[,i] > 0.5][,i]) / length(df[,i]))
    #   below <- (length(df[df[,i] < 0.5][,i]) / length(df[,i]))
    #   aboveList[i-1] = above
    #   belowList[i-1] = below
    # }
    # pointfivedf <- data.frame(aboveList,belowList, row.names = c("1W","2W","3W","4W","5W","6W","7W","8W","9W","10W","11W","12W"))
    # names(pointfivedf) <- c("above_0.5", "below _0.5")
    # print(pointfivedf)
    # 
    # for(i in 2:13){
    #   above <- (length(df[df[,i] > 0.7][,i]) / length(df[,i]))
    #   below <- (length(df[df[,i] < 0.3][,i]) / length(df[,i]))
    #   aboveList[i-1] = above
    #   belowList[i-1] = below
    # }
    # 
    # pointthreesevendf <- data.frame(aboveList,belowList, row.names = c("1W","2W","3W","4W","5W","6W","7W","8W","9W","10W","11W","12W"))
    # names(pointthreesevendf) <- c("above_0.7", "below _0.3")
    # print(pointthreesevendf)
    
    
    return(WeightedHurstMean)
  } 
  return(NULL)
}

# set the time frame
begin = "2018-07-01"
end = "2019-11-30"

# get the number of trading days
Dates_begin <- as.Date(begin) 
Dates_end <- as.Date(end)
nyse <- timeDate::holidayNYSE(2000:year(Sys.Date()) +1)
NYcal <- create.calendar(name='NYSE', holidays=nyse, weekdays=c('saturday', 'sunday'))
bizdays.options$set(default.calendar='NYSE')
Num_Days <- bizdays(Dates_begin,Dates_end,NYcal)

#stat <- runTest("HOG",begin, end)

# get some random stocks
symbol_data <- stockSymbols(exchange = "NASDAQ")
# remove the indexes
symbol_data <- na.omit(symbol_data)

# get a random sample
sampled_data <- sample_n(symbol_data,20)

# get only the ticker symbols
symbol_list <- sampled_data$Symbol

# empty list to store stats
statslist = list()
for( i in 1:length(symbol_list)){
  statslist[i] <- runTest(symbol_list[i],begin,end)
}

meanHurstdf <- data.frame(symbol_list,as.numeric(as.character(statslist)))
names(meanHurstdf) <- c("Ticker","Mean_Hurst")

runTest("ORGO",begin,end)
