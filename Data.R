#
# Funktion (getYahooData)  gibt historische Daten von einschlie�lich 3. Januar 2001 verf�gbar bis gestern zur�ck
#
# Input:
# singleStockSymbol - Vektor von Aktien-Symbolen (siehe Yahoo Finance, z.B. ADS.DE f�r Adidas AG Xetra), als String
# startDate, endDate - im Format "YYYY-MM-DD", z.B. "2010-12-31", als String
# discretization - d=t�gliche Werte, w�chentliche Werte, m=monatliche Werte, v=Dividendenaussch�ttungen, als String
#Smart 0 Exclusion
#
# Quelle: http://brusdeylins.info/tips_and_tricks/yahoo-finance-api/
#
getYahooData <- function(singleStockSymbol, startDate, endDate, discretization){
  # gegebenes Start- und Enddatum wird als Datumstyp gecastet
  startDate <- as.Date(startDate)
  endDate <- as.Date(endDate)
  
  # Tage und Jahre werden als Character gecastet
  startDayChar <- format(startDate,"%d")
  startYearChar <- format(startDate,"%Y")
  endDayChar <- format(endDate,"%d")
  endYearChar <- format(endDate,"%Y")
  
  # Monate werden als Integer gecastet, weil sie weiterverarbeitet werden müssen
  startMonthInt <- as.integer(format(startDate,"%m"))
  endMonthInt <- as.integer(format(endDate,"%m"))
  
  # Monate müssen im format "00", "01", ..., "11" angegeben werden
  startMonthChar <- formatC(startMonthInt - 1, width = 2, flag = "0")
  endMonthChar <- formatC(endMonthInt - 1, width = 2, flag = "0")
  
  # Datum-Strings werden zusammengesetzt
  startDate <- paste("a=", startMonthChar, "&b=", startDayChar, "&c=", startYearChar, sep="")
  endDate <- paste("d=",endMonthChar, "&e=", endDayChar, "&f=", endYearChar, sep="")
  
  # Gesamte URL wird zusammengesetzt
  url <- paste("http://ichart.finance.yahoo.com/table.csv?s=", singleStockSymbol,"&", startDate, "&", endDate, "&g=", as.character(discretization), "&ignore=.cvs", sep="")
  
  # Daten werden als Data.Frame abgelegt
  singleTimeSeries <- read.csv(url, header = TRUE, stringsAsFactors=FALSE)
  
  # Beobachtungen mit Trade-Volumen gleich 0 werden ggf. ausgeschlossen
  if ((sum(singleTimeSeries$Volume!=0)/length(singleTimeSeries$Volume) >2/7) ){ #Mindestens 2 Handelstage Pro Woche(7 Tage)
    singleTimeSeries <- subset(singleTimeSeries, Volume > 0)
  }else{
    print(paste("Zero Volume Dates NOT excluded of stock:",singleStockSymbol,sep=" "))
  }
  
  return(singleTimeSeries)
}



#
# Funktion (getMultipleYahooData)  gibt Liste von Zeitreihen verschiedener Aktien zur�ck
#
# Input:
# multiStockSymbols - Vektor von Aktien-Symbolen (siehe Yahoo Finance, z.B. ADS.DE f�r Adidas AG Xetra), als String
# startDate, endDate - im Format "YYYY-MM-DD", z.B. "2010-12-31", als String
# discretization - d=t�gliche Werte, w�chentliche Werte, m=monatliche Werte, v=Dividendenaussch�ttungen, als String
# includeZeroVolumeDays - FALSE=Beobachtungen mit Trade-Volumen gleich 0 werden ausgeschlossen, TRUE=solche Beobachtungen werden mit ausgegeben
#
getMultipleYahooData <- function(multiStockSymbols, startDate, endDate, discretization){
  #Get Dirty Time Series from getMultipleYahooData
  timeSeriesList <- list()
  for (i in 1:length(multiStockSymbols)) {
    timeSeriesList[[i]] <- getYahooData(multiStockSymbols[i], startDate, endDate, discretization)
  }
  #Calculate Intersection of Trading Dates of all Stocks
  IntersectionDates <- timeSeriesList[[1]]$Date
  for (i in 2:length(timeSeriesList)){
    nextDates <- timeSeriesList[[i]]$Date
    IntersectionDates <- IntersectionDates[IntersectionDates %in% nextDates]
  }
  #Reduce each time series to IntersectionDates
  for (i in 1:length(timeSeriesList)){
    timeSeriesList[[i]] <- timeSeriesList[[i]][timeSeriesList[[i]]$Date %in% IntersectionDates,]
  }
  return(timeSeriesList)
}




#Function (getReturnVector)   Get return Vector from single Time Series data frame
#
#Input      adjClose      Data Frame vector:singleTimeSeries$Adj.Close
#Output     returnVector  Data Frame: returnVector (1 dim less than singleTimeSeries$Adj.Close) 

getReturnVector<-function(adjustedClose){
  returnVector<-(-1)*diff(adjustedClose)/adjustedClose[2:length(adjustedClose)]
  return(returnVector)
}



#Test
# 
#  singleStockSymbol <- c("ADS.DE")
#  multiStockSymbols<-c("^GDAXI","LIN.DE","ADS.DE")
#  startDate<-"2016-05-01"
#  endDate<-Sys.Date()
#  discretization<-"d"
#  
#  singleTimeSeries <- getYahooData(singleStockSymbol, startDate, endDate, discretization)
#  print(singleTimeSeries)
#  
#  multiTimeSeries<- getMultipleYahooData(multiStockSymbols,startDate, endDate, discretization)
#  print(multiTimeSeries)
#  length(multiTimeSeries[[1]]$Date)  #Verschiedene L�ngen
#  length(multiTimeSeries[[2]]$Date)
#  length(multiTimeSeries[[3]]$Date)
#  
#  getReturnVector(multiTimeSeries[[1]])
#  getReturnVector(multiTimeSeries[[2]])
# 

