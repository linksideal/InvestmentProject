#
# R-Code to statistically analyze Stocks/financial products based on Data from yahoo.R
#


#Function (Info): 
#Input:   singleStockSymbol - Vektor von Aktien-Symbolen (siehe Yahoo Finance, z.B. ADS.DE für Adidas AG Xetra), als String
#         startDate, endDate - im Format "YYYY-MM-DD", z.B. "2010-12-31", als String
#         discretization - d=tägliche Werte, wöchentliche Werte, m=monatliche Werte, v=Dividendenausschüttungen, als String
#
#Output:  Vektor mit Eigenschaften, basierend auf 'Bereingten Daten' mit Komponenten:
#         startDate
#         endDate
#         numberOfDays
#         return over period
#         mean return
#         variance return
#         periodHigh
#         periodLow
#         periodDistance=periodHigh-periodLow
# 
#         4 Plots von Preisverlauf, Returnverlauf, Histogramm returns, Volumen verlauf

Info<-function(singleStockSymbol, startDate, endDate, discretization){
  #Get Data on single stock using getYahooData from yahoo.R
  source("Data.R")
  singleStockData<-getYahooData(singleStockSymbol, startDate, endDate, discretization)
  #Calculate return vector from Adjusted Closing Prices
  adjustedClose<-singleStockData$Adj.Close
  DateVector<-as.Date(singleStockData$Date)
  returnVector<-getReturnVector(singleStockData)
  #Create infoVector
  infoVector<-matrix(0,9,1)  #Info vektor vorbelegen
  infoVector[1]<-as.character.Date(startDate)
  infoVector[2]<-as.character.Date(endDate)
  infoVector[3]<-as.character(as.Date(endDate)-as.Date(startDate))
  infoVector[4]<-round((adjustedClose[1]-adjustedClose[length(adjustedClose)])/adjustedClose[length(adjustedClose)],4)
  infoVector[5]<-round(mean(returnVector),4)
  infoVector[6]<-round(var(returnVector),4)
  infoVector[7]<-max(adjustedClose)
  infoVector[8]<-min(adjustedClose)
  infoVector[9]<-max(adjustedClose)-min(adjustedClose)
  rownames(infoVector)<-c("startDate","endDate","#Days","total return over period","return mean","return variance","period High Price","period Low Price","period High-Low")
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
  return(infoVector)
}


#Test

graphics.off()    #Closes all Plots
singleStockSymbol1 <- c("ADS.DE")
singleStockSymbol2<-c("LIN.DE")
singleStockSymbol3<-c("^GDAXI")
multiStockSymbols<-c("ADS.DE","^GDAXI")
startDate<-"2015-01-01"
endDate<-Sys.Date()
discretization<-"d"


infoVector<-Info(singleStockSymbol1, startDate, endDate, discretization)
print(infoVector)

infoVector<-Info(singleStockSymbol2, startDate, endDate, discretization)
print(infoVector)


infoVector<-Info(singleStockSymbol3, startDate, endDate, discretization)
print(infoVector)








