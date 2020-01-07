require(stats);
require(graphics);
require(fArma);
library(somebm)
library(pracma)
library(urca)
library(aTSA)


sinData <- sin(0:1000);
plot(sinData)

lineData <- 1:1000
plot(lineData)

brownNoiseData <- gbm(n=10000)
plot(brownNoiseData)

calculateHursts <- function(data){
  print(list(pengFit(data)@hurst$H,
             rsFit(data)@hurst$H,
             aggvarFit(data)@hurst$H,
             higuchiFit(data)@hurst$H,
             absvalFit(data)@hurst$H,
             diffvarFit(data)@hurst$H,
             perFit(data)@hurst$H,
             boxperFit(data)@hurst$H));
  hurstexp(data)
  udf_test <- ur.df(data, type='trend')
  print(udf_test)
  adf_test <- adf.test(data)
  print(adf_test)
  kpss_test <- kpss.test(data)
  print(kpss_test)
}

calculateHursts(brownNoiseData)
calculateHursts(lineData)
calculateHursts(sinData)

