library("quantmod")
library("PerformanceAnalytics")

#Trade logic - Look for mean reversion
#If price/vwap > uLim go SHORT
#If price/vwap < lLim go LONG

#Script parameters
symbol <- "SPY"     #Symbol
nlookback <- 3 #Number of days to lookback and calculate vwap
uLim <- 1.001  #If price/vwap > uLim enter a short trade
lLim <- 0.999 #If price/vwap < lLim enter a long trade


#Specify dates for downloading data
startDate <- as.Date("2012-01-01") #Specify what date to get the prices from
symbolData <- new.env() #Make a new environment for quantmod to store data in
getSymbols(Symbols = symbol, env = symbolData, src = "yahoo")
mktdata <- eval(parse(text=paste("symbolData$",sub("^", "", symbol, fixed = TRUE))))
mktdata <- head(mktdata, -1) #fix duplicate dates

#Calculate volume weighted average price
vwap <- VWAP(Cl(mktdata), Vo(mktdata), n = nlookback)
#Can calculate vwap like this, but it is slower
#vwap <- runSum(Cl(mktdata)*Vo(mktdata),nlookback)/runSum(Vo(mktdata),nlookback)

#Calulate the daily returns
dailyRet <- Delt(Cl(mktdata), k = 1, type = "arithmetic") #Daily Returns

# signal = price/vwap
signal <- Cl(mktdata) / vwap
signal[is.na(signal)] <- 1 #Setting to one means that no trade will occur for NA's
trade <- apply(signal, 1, function(x) {if(x<lLim) { return (1) } else { if(x>uLim) { return(-1) } else { return (0) }}})

#Calculate the P&L
#The daily ret is DailyRet(T)=(Close(T)-Close(T-1))/Close(T-1)
#We enter the trade on day T so need the DailyRet(T+1) as our potential profit
#Hence the lag in the line below
strategyReturns <- trade * lag(dailyRet, -1)
strategyReturns <- na.omit(strategyReturns)

#### Performance Analysis ###
#Calculate returns for the index
indexRet <- dailyRet #Daily returns
colnames(indexRet) <- "IndexRet"
zooTradeVec <- cbind(as.xts(strategyReturns), as.xts(indexRet)) #Convert to zoo object
colnames(zooTradeVec) <- c(paste(symbol, " VWAP Trade"), symbol)
zooTradeVec <- na.omit(zooTradeVec)

#BnH vs Strategy
dev.new()
charts.PerformanceSummary(R = zooTradeVec,
                          main = paste("Performance of ", symbol, " VWAP Strategy"),
                          geometric = FALSE)


#table of montly returns by year and strategy
cat("Calander Returns")
print(table.CalendarReturns(zooTradeVec))
#Calculate the sharpe ratio
cat("Sharpe Ratio")
print(SharpeRatio.annualized(zooTradeVec))

table.AnnualizedReturns(zooTradeVec)
