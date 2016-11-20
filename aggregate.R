load("USfinal.RData")
rankOfMarket <- USfinal %>% filter(Date == max(Date)) %>% 
  arrange(desc(Market.Cap))

######################### choose how many tickers
nTickets <- 200
ticketChoice <- rankOfMarket$Ticker[1:nTickets]
USfinal <- USfinal %>% filter(Ticker %in% ticketChoice) %>% 
  select(-Market.Cap) %>% arrange(Date)
library(tidyr)
USOut <- spread(USfinal, Ticker,Price)
#USOut[complete.cases(USOut),]

######################### derive log return
USOutT1 <- USOut[2:nrow(USOut),-1]
USOutT <- USOut[1:(nrow(USOut)-1),-1]
USOutTime <- USOut[2:nrow(USOut),1]
USlogReturn <- log(USOutT1/USOutT)
USReturn <- cbind(USOutTime,USlogReturn)
USReturn[1:5,1:5]

######################## start day
beginning <- min(USReturn$USOutTime) + as.Date('1899-12-30')






library(caret)
library(stats)
startDate <- 31
marketValue <- 1
historyValue <- c(marketValue)
portfolio <- USReturn[1,-1]

change <- 0
endDate <- nrow(USReturn)
for (i in startDate:endDate){
  
  ########################################### build portfolio by history
  trainset <- USReturn[1:(i-1),]
  
  ################### Clustering
  numCluster <- 40
  clusterIteNum <- 500
  clusterResult <- clustering(trainset, numCluster, clusterIteNum)
  orderClusters <- clusterResult[[1]]
  clusterSize <- clusterResult[[2]]

  
  ################### change the portfolio
  ## randomly choose one equity from each cluster and ditribute the value equally
#  chosenEquity <- c()
#  for (equity in 1:numCluster){
#    chosenEquity <- c(chosenEquity, 
#                      names(orderClusters[orderClusters == equity])[1])
#  }
#  for (e in 1: ncol(portfolio)){
#    if (names(portfolio)[e] %in% chosenEquity){
      # distribute current market value among portfolio
#      portfolio[1,e] <- 1 / length(chosenEquity) * marketValue
#    }else{
#      portfolio[1,e] <- 0 
#    }
#  }

  ################### change the portfolio 2
  ## ditribute the value equally across the clusters and among in each clusters
  portfolio[1,] <- 0
  for (equity in 1: length(orderClusters)){
    portfolio[1, names(orderClusters)[equity]] <- 
      1 / numCluster / clusterSize[as.numeric(orderClusters[equity])] * marketValue
  }
  
  ################## record change of portfolio
  # change <- length(setdiff(chosenEquity,chosenEquityB))
  # chosenEquityB <- chosenEquity 
  # print(setdiff(chosenEquity,chosenEquityB))
  # print(i)
  # print(change)
  
  ########################################### get the current return
  flush.console()
  tmp <- USReturn[i,-1]
  tmp <- exp(tmp)
  todayValue <- sum(tmp * portfolio, na.rm = T)
  marketValue <- todayValue
  historyValue <- c(historyValue, marketValue)
  
  ################## plot
  time <- (startDate-1):i
  plot(time,historyValue,type = 'l')
  Sys.sleep(.01)
}
