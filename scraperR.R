# Load the Library
library(RSelenium)
library(XML)

# start the server and browser
# system commands to set everything up are:

# sudo apt install docker.io
# sudo systemctl start docker
# sudo systemctl enable docker
# sudo docker pull selenium/standalone-firefox
# sudo docker run -d -p 4445:4444 selenium/standalone-firefox

# make sure docker is behaving first stop all instances then start what we want
system("sudo docker stop $(sudo docker ps -a -q)", wait = TRUE)
system("sudo docker run -d -p 4445:4444 selenium/standalone-firefox", wait = TRUE)


# connect to the selenium server
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "firefox")
remDr$open()

buoyUrl <- "https://www.cwb.gov.tw/V8/E/M/OBS_Marine_30day.html?MID=WRA007"

getBuoyData <- function(url){
  # navigate to the buoy URL
  remDr$navigate(url)
  
  # scroll to the bottom
  webElem <- remDr$findElement("css", "body")
  for(i in 1:500){
    webElem$sendKeysToElement(list(key = "down_arrow"))
  }
  
  # get the source
  src <- remDr$getPageSource()
  # get the table
  page <- readHTMLTable(src[[1]])
  # extract the data frame  
  df <- data.frame(page[1])
  # rename the rows
  names(df) <- c("Time", "Tide_Height_m","Wave_Height_m","Wave_Dir","Wave_Period","Wind_Speed_ms",
                 "Wind_Dir_ms","Wind_Gust_ms","WTMP","ATMP","PRES_HPA","Current_Dir","Current_Speed_kts")
  
  # return the data frame
  return(df)
}

buoydf <- getBuoyData(buoyUrl)