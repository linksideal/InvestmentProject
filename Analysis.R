#
# R-Code to statistically analyze Stocks/financial products based on Data from Data.R
#


#Function (Info):   Gibt Fundamentaldaten einer einzelnen Aktie aus, sowie 4 Graphen zur Visualisierung der Daten
#Input:   singleStockSymbol - Einzelnes Aktien-Symbol (siehe Yahoo Finance, z.B. ADS.DE für Adidas AG Xetra), als String
#         startDate, endDate - im Format "YYYY-MM-DD", z.B. "2010-12-31", als String
#         discretization - d=tägliche Werte, wöchentliche Werte, m=monatliche Werte, v=Dividendenausschüttungen, als String
#         GrafikOn - Boolean TRUE->Grafik ausgabe FALSE->keine Grafik ausgabe
#
#Output:  Vektor mit Komponenten:
#         startDate
#         endDate
#         numberOfDays
#         numberOfTradingDays
#         return over period
#         mean return
#         variance return
#         periodHigh
#         periodLow
#         periodDistance=periodHigh-periodLow
# 
#         4 Plots von Preisverlauf(mit polynom fit zweiten grades), Returnverlauf, Histogramm returns, Volumen verlauf

Info<-function(singleStockSymbol, startDate, endDate, discretization,GrafikOn){
  #Get Data on single stock using getYahooData from Data.R
  source("Data.R")
  singleStockData<-getYahooData(singleStockSymbol, startDate, endDate, discretization)
  #Calculate return vector from Adjusted Closing Prices
  adjustedClose<-singleStockData$Adj.Close
  returnVector<-getReturnVector(adjustedClose)
  #Create infoVector
  infoVector<-matrix(0,10,1)  #Info vektor vorbelegen
  infoVector[1]<-as.character.Date(startDate)
  infoVector[2]<-as.character.Date(endDate)
  infoVector[3]<-as.character(as.Date(endDate)-as.Date(startDate))
  infoVector[4]<-length(singleStockData$Date)
  infoVector[5]<-round((adjustedClose[1]-adjustedClose[length(adjustedClose)])/adjustedClose[length(adjustedClose)],4)
  infoVector[6]<-round(mean(returnVector),4)
  infoVector[7]<-round(var(returnVector),4)
  infoVector[8]<-max(adjustedClose)
  infoVector[9]<-min(adjustedClose)
  infoVector[10]<-max(adjustedClose)-min(adjustedClose)
  rownames(infoVector)<-c("startDate","endDate","#Days","#TradingDays","total return over period","return mean","return variance","period High Price","period Low Price","period High-Low")
  if(GrafikOn==TRUE){
    DateVector<-as.Date(singleStockData$Date)
    #Calculate trend as adjClose(T)~a*(T)^2+b*(T)+c
    DaysSinceStartDateVector<-as.matrix(DateVector)
    X<-cbind(DaysSinceStartDateVector,DaysSinceStartDateVector^2);
    Y<-adjustedClose;
    fit<-lsfit(X, Y, wt = NULL, intercept = TRUE, tolerance = 1e-07);
    coefficientsOLS<-fit$coef
    estimationFunction<-function(T){return(coefficientsOLS[1]+as.matrix(T)*coefficientsOLS[2]+as.matrix(T)^2*coefficientsOLS[3])}
    #Create plots
    dev.new()
    par(mfrow=c(2,2))
    plot(DateVector,adjustedClose,main=paste(singleStockSymbol,"- adj. Close Price / dicretization = ",as.character(discretization),sep=" "),xlab="Time",ylab="adj Close Price","l")
    curve(estimationFunction,from=as.Date(startDate),to=as.Date(endDate),add=TRUE);
    plot(DateVector[2:length(DateVector)],returnVector,main=paste(singleStockSymbol,"- Returns / dicretization = ",as.character(discretization),sep=" "),xlab="Time",ylab="return","l")
    abline(0,0)   #Zero Return Line
    hist(returnVector,main=paste(singleStockSymbol,"- Histogramm of Returns / dicretization = ",as.character(discretization),sep=" "))
    plot(DateVector,singleStockData$Volume,main=paste(singleStockSymbol,"- Trading Volume / dicretization = ",as.character(discretization),sep=" "),xlab="Time",ylab="Volume","l")
  }
    return(infoVector)
}




#Function (RandomPortfolio): Generates a Porfolio(Dataframe with 2 columns) with random weights (uniformly)
#
#Input:   multiStockSymbols - Vektor von Aktien-Symbolen (siehe Yahoo Finance, z.B. ADS.DE für Adidas AG Xetra), als String
#Output:  Portfolio - Dataframe with 2 columns ( 1st column = "multiStockSymbols",2nd column="weights" that sum to 1)

RandomPortfolio<-function(multiStockSymbols){
  #Genereate random weights
  uniformVector<-runif(length(multiStockSymbols), min = 0, max = 1)
  weights<-uniformVector/sum(uniformVector)
  Portfolio<-data.frame(multiStockSymbols,weights)
  return(Portfolio)
}


#Function (HistoricalPortfolioPerformance): Gibt Fundamentaldaten eines Portfolios aus, sowie 4 Graphen zur Visualisierung der Daten
#
#Input:   Portfolio - Dataframe with 2 columns ( 1st column = "multiStockSymbols",2nd column="weights" that sum to 1)
#         startDate - Start des Beobachtungszeitraumesim Format "YYYY-MM-DD", z.B. "2010-12-31", als String
#                 ! endDate=Sys.Date()
#         NumberOfDays - Integer, Anzahl vergangener Tage die zur Berechnung der EWMA/ExpWMA returns und varianzen genutzt wird
#                 ! sollte größer als 10 sein, damit immer handelstage in diesem Interval sind
#         GrafikOn - Boolean TRUE->Grafik ausgabe   FALSE->keine Grafik ausgabe
#         Index - String: Für porfolio aussagekräftiger Einzelnes Index Symbol (bsp "^GDAXI") 
#
#
#Output:  Vektor mit Komponenten:
#         startDate
#         endDate
#         numberOfDays
#         numberOfTradingDays
#         return over period
#         mean return
#         variance return
#         periodHigh
#         periodLow
#         periodDistance=periodHigh-periodLow
# 
#         6 Plots von Preisverlauf(mit polynom fit zweiten grades), Balkendiagramm returns, Histogramm returns,  
#                     EWMA Variance,                                ExpWMA Variance     VergleichMit ?aussagekräftigem? Index


HistoricalPortfolioPerformance<-function(Portfolio,startDate,NumberOfDays,GrafikOn,Index){
  #Get daily Data on multiple stocks from portfolio using getMultipleYahooData Data.R
  source("Data.R")
  multiStockData<-getMultipleYahooData(c(format(Portfolio$multiStockSymbols),Index), as.Date(startDate)-NumberOfDays, Sys.Date(), "d")
  #Calculate historical Portfolio Prices Extended and startToday/ Define Date Vectors Extended and startToday
  adjClosePortfolioExtended<-rep(0,length(multiStockData[[1]]$Date))
  for(i in 1:length(Portfolio$weights)){
    adjClosePortfolioExtended<-adjClosePortfolioExtended+Portfolio$weights[i]*multiStockData[[i]]$Adj.Close
  }
  adjClosePortfolioStartToday<-adjClosePortfolioExtended[1:(length(adjClosePortfolioExtended)-NumberOfDays)]
  DatesExtended<-as.Date(multiStockData[[1]]$Date)
  DatesStartToday<-DatesExtended[1:(length(DatesExtended)-NumberOfDays)]
  #Calculate Portfolio return vector from Portfolios Adjusted Closing Prices
  returnVectorPortfolioExtended<-getReturnVector(adjClosePortfolioExtended)
  returnVectorPortfolioStartToday<-returnVectorPortfolioExtended[1:(length(returnVectorPortfolioExtended)-NumberOfDays)]
  #Create infoVector  
  infoVector<-matrix(0,10,1)  #Info vektor vorbelegen
  infoVector[1]<-as.character.Date(startDate)
  infoVector[2]<-as.character.Date(Sys.Date())
  infoVector[3]<-as.character(as.Date(Sys.Date())-as.Date(startDate))
  infoVector[4]<-length(DatesStartToday)
  infoVector[5]<-round((adjClosePortfolioStartToday[1]-adjClosePortfolioStartToday[length(adjClosePortfolioStartToday)])/adjClosePortfolioStartToday[length(adjClosePortfolioStartToday)],4)
  infoVector[6]<-round(mean(returnVectorPortfolioStartToday),4)
  infoVector[7]<-round(var(returnVectorPortfolioStartToday),4)
  infoVector[8]<-round(max(adjClosePortfolioStartToday),4)
  infoVector[9]<-round(min(adjClosePortfolioStartToday),4)
  infoVector[10]<-round(max(adjClosePortfolioStartToday)-min(adjClosePortfolioStartToday),4)
  rownames(infoVector)<-c("startDate","endDate","#Days","#TradingDays","portfolio return over period","return mean","return variance","period High Price","period Low Price","period High-Low")
  
  if(GrafikOn==TRUE){
  #Calculate trend as adjClose(T)~a*(T)^2+b*(T)+c
  DatesForSquareFit<-as.matrix(DatesStartToday)
  X<-cbind(DatesForSquareFit,DatesForSquareFit^2);
  Y<-adjClosePortfolioStartToday;
  fit<-lsfit(X, Y, wt = NULL, intercept = TRUE, tolerance = 1e-07);
  coefficientsOLS<-fit$coef
  estimationFunction<-function(T){return(coefficientsOLS[1]+as.matrix(T)*coefficientsOLS[2]+as.matrix(T)^2*coefficientsOLS[3])}
  
  #Calculate EWMA and ExponentialWMA Portfolio Variance with NumberOfDays period length
  EWMAVariance<-rep(0,length(returnVectorPortfolioStartToday))
  meanReturn<-mean(returnVectorPortfolioExtended)
  for(i in 1:length(returnVectorPortfolioStartToday)){
    EWMAVariance[i]=sum((returnVectorPortfolioExtended[i:i+NumberOfDays]-meanReturn)^2)/NumberOfDays
  }
  
  ExpWMAVariance<-rep(0,length(returnVectorPortfolioStartToday))    # Credit Metrics
  ExpWMAVariance[length(ExpWMAVariance)]<-(1-0.94)*sum(0.94^(0:(length(NumberOfDays)-1))*(returnVectorPortfolioExtended[length(ExpWMAVariance):length(ExpWMAVariance)+NumberOfDays]-meanReturn)^2)/NumberOfDays
  for(i in (length(returnVectorPortfolioStartToday)-1):1){
    ExpWMAVariance[i]=0.94*ExpWMAVariance[i+1]+(1-0.94)*returnVectorPortfolioExtended[i+1]^2
  } 
  
  #Calculate normalized Index adjClose
  adjCloseIndex<-multiStockData[[length(Portfolio$multiStockSymbols)+1]]$Adj.Close[1:length(DatesStartToday)]
  NormalizedAdjCloseIndex<-adjCloseIndex*adjClosePortfolioStartToday[length(adjClosePortfolioStartToday)]/adjCloseIndex[length(adjCloseIndex)]
  
  #Create plots
  dev.new()
  par(mfrow=c(2,3))
  
  plot(DatesStartToday,adjClosePortfolioStartToday,main=paste("Portfolio adj. Close Price ",sep=" "),xlab="Time",ylab="adj Close Price","l")
  curve(estimationFunction,from=as.Date(startDate),to=as.Date(Sys.Date()),add=TRUE);
  plot(DatesStartToday[2:length(DatesStartToday)],returnVectorPortfolioStartToday,main=paste("Historical daily Portfolio Returns",sep=" "),xlab="Time",ylab="return","h")
  abline(0,0)   #Zero Return Line
  hist(returnVectorPortfolioStartToday,main=paste("Histogramm of Portfolio Returns ",sep=" "),xlab="Return",ylab="Frequency")
  plot(DatesStartToday,NormalizedAdjCloseIndex,main=paste("Comparison with Normalized Index ",Index,sep=" "),xlab="Time",ylab="Normalized Index adj Close Price","l")
  plot(DatesStartToday[1:(length(DatesStartToday)-1)],EWMAVariance,main=paste("EWMA Variance of Portfolio Returns ",sep=" "),"l") #bei dates -1 da Returnvektor 1 kürzer als adjClose :)
  plot(DatesStartToday[1:(length(DatesStartToday)-1)],ExpWMAVariance,main=paste("ExpWMA Variance of Portfolio Returns (lambda=0.94) ",sep=" "),"l") #bei dates -1 da Returnvektor 1 kürzer als adjClose :)
  #Header for Plots
  Header<-"Portfolio Composition : "
  for(i in 1:length(Portfolio$multiStockSymbols)){
    Header<-paste(Header,Portfolio$multiStockSymbols[i],round(Portfolio$weights[i],2),"/")
  }
  mtext(Header, side = 3, line = -1.5, outer = TRUE)
  }
  
  return(infoVector)
}








# #Test
# 
# graphics.off()    #Closes all Plots
# singleStockSymbol1 <- c("FRE.DE")
# singleStockSymbol2<-c("LIN.DE")
# singleStockSymbol3<-c("^GDAXI")
# startDate<-"2013-01-02"
# endDate<-Sys.Date()
# discretization<-"d"
# GrafikOn<-TRUE
# NumberOfDays<-100
# 
# #infoVector<-Info(singleStockSymbol1, startDate, endDate, discretization,GrafikOn)
# #print(infoVector)
# # 
# # infoVector<-Info(singleStockSymbol2, startDate, endDate, discretization)
# # print(infoVector)
# # 
# # infoVector<-Info(singleStockSymbol3, startDate, endDate, discretization)
# #print(infoVector)
# 
#  multiStockSymbols<-c("ADS.DE","FRE.DE","LIN.DE")     # PROBLEM=YAHOO FINANCE IST MIST WEIL DER STOCKSPLIT BEI FRESENIUS BEI ADJ.CLOSE NICHT ORDENTLICH BEHOBEN IST=>MIST VARIANZEN UND RETURN GRAFIK
#  Index<-"^GDAXI"
#  randomPortfolio<-RandomPortfolio(multiStockSymbols)
#  randomPortfolio
#  infoVector<-HistoricalPortfolioPerformance(randomPortfolio,startDate,NumberOfDays,GrafikOn,Index)
#  infoVector





