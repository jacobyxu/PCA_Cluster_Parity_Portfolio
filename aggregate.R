library(Hmisc); library(xlsx); library(dplyr); library(psych)
library(gtable); library(ggplot2); library(data.table)

clustering <- function(dataset, numCluster, clusterIteNum, removeOutliers = 0.999){
  
  ############################################ derive cor & cov
  UScorMatrix <- cor(dataset[,-1], use = "pairwise.complete.obs")
  UScovMatrix <- cov(dataset[,-1], use = "pairwise.complete.obs")
  
  ################# remove NAs for cor
  delete <- c()
  i <- 1
  for (col in 1:ncol(UScorMatrix)){
    if(sum(is.na(UScorMatrix[,col])) == nrow(UScorMatrix)){
      delete[i] <- col
      i = i+1 }}
  UScorMatrix <- UScorMatrix[-(delete),-(delete)]
  for (col in 1:ncol(UScorMatrix)){
    for (row in 1:nrow(UScorMatrix)){
      if (is.na(UScorMatrix[row,col]))
        UScorMatrix[row,col] <- 0 }}
  
  ################## remove NAs for cov
  delete <- c()
  i <- 1
  for (col in 1:ncol(UScovMatrix)){
    if(sum(is.na(UScovMatrix[,col])) == nrow(UScovMatrix)){
      delete[i] <- col
      i = i+1 }}
  UScovMatrix <- UScovMatrix[-(delete),-(delete)]
  for (col in 1:ncol(UScovMatrix)){
    for (row in 1:nrow(UScovMatrix)){
      if (is.na(UScovMatrix[row,col]))
        UScovMatrix[row,col] <- 0 }}
  
  ################## normalize the cov Matrix
  #  outlierLine <- as.numeric(quantile(UScovMatrix,removeOutliers))
  #  maxcov <- max(UScovMatrix, na.rm = TRUE)
  #  mincov <- min(UScovMatrix, na.rm = TRUE)
  #  normalcovMatrix <- (UScovMatrix - mincov) / (outlierLine - mincov)
  #  for (i in 1:NROW(normalcovMatrix)){
  #    for (j in 1:NCOL(normalcovMatrix)){
  #      if (normalcovMatrix[i,j] > 1){
  #        normalcovMatrix[i,j] <- 1
  #  }}}
  
  ################# change the data storage type
  corDataFrame <- data.frame(UScorMatrix)
  covDataFrame <- data.frame(UScovMatrix)
  #  covDataFrame <- data.frame(normalcovMatrix)
  
  ########################################## derive matrix for clustering
  
  for (i in 1:nrow(corDataFrame)){
    for (j in 1:ncol(corDataFrame)){
      corDataFrame[i,j] <- sqrt((1 - corDataFrame[i,j])/2)
    }}
  
  ######################################### PCA to choose initial centers
  pca.model <- prcomp(corDataFrame, center = FALSE, scale. = TRUE) 
  Rotation <- pca.model[2]
  pca.rotation1 <- Rotation[[1]][,1]
  positivePC <- pca.rotation1[order(abs(pca.rotation1), decreasing = TRUE)]
  centersNames <- names(positivePC[1:numCluster])
  
  ########################################## Clustering
  kcenters <- corDataFrame[centersNames, ]
  set.seed(2016)
  
  ###################### plot heatmap by ggplot
  #  corDataFramePlot <- corDataFrame
  #  corDataFramePlot$Name <- row.names(corDataFramePlot)
  #  corDataFrame.m <- melt(corDataFramePlot)
  #flush.console()
  #  g <- ggplot(data = corDataFrame.m, aes(x=Name, y=variable, fill=value)) + geom_tile() +
  #    theme(axis.text = element_blank(), axis.title = element_blank())+
  #      scale_fill_gradient(low = "white", high = "red")
  #  print(g)
  #  Sys.sleep(.09)
  
  ###################### Clustering
  #  for (i in 1:clusterIteNum){
  
  #    corCluster <- kmeans(corDataFrame, centers=kcenters, iter.max=1)
  #    kcenters <- data.frame(corCluster$centers)
  
  ####################### reorder matrix
  #    orderCluster <- corCluster$cluster[order(corCluster$cluster)]
  #    nameList <- names(orderCluster)
  #    newCorDataFrame <- corDataFrame[nameList,nameList]
  
  #    if (i == clusterIteNum){
  ###################### plot heatmap
  #      newCorDataFrame$Name <- row.names(newCorDataFrame)
  #      newCorDataFrame.m <- melt(newCorDataFrame)
  #flush.console()
  #      g <- ggplot(data = newCorDataFrame.m, aes(x=Name, y=variable, fill=value)) + geom_tile() +
  #        theme(axis.text = element_blank(), axis.title = element_blank())
  #      print(g)
  #      Sys.sleep(.09)
  #    }}
  ###################### inital heatmap
  tmap1 <- as.matrix(corDataFrame)
  map1 <- t(tmap1)
  
  ###################### clustering
  corCluster <- kmeans(corDataFrame, centers=kcenters, iter.max=clusterIteNum)
  kcenters <- data.frame(corCluster$centers)
  
  ####################### reorder matrix and heatmap
  orderCluster <- corCluster$cluster[order(corCluster$cluster)]
  clusterSize <- corCluster$size
  nameList <- names(orderCluster)
  newCorDataFrame <- corDataFrame[nameList,]
  newCorDataFrame <- newCorDataFrame[,nameList]
  tmap2 <- as.matrix(newCorDataFrame)
  map2 <- t(tmap2)
  
  #   ####################### plot heatmap by ggplot
  #    newCorDataFrame$Name <- nameList #row.names(newCorDataFrame)
  #    newCorDataFrame.m <- melt(newCorDataFrame)
  #flush.console()
  #    g <- ggplot(data = newCorDataFrame.m, aes(x=Name, y=variable, fill=value)) + geom_tile() +
  #        theme(axis.text = element_blank(), axis.title = element_blank())+
  #      scale_fill_gradient(low = "white", high = "red")
  #    print(g)
  
  return(list(orderCluster, clusterSize, covDataFrame, map1, map2))
}


#############################################################################################

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




###########################################################################  backtest

library(caret)
library(stats)
startDate <- 31
marketValue <- 1
historyValue <- c(marketValue)
portfolio <- USReturn[1,-1]

change <- 0
endDate <- nrow(USReturn)
# i <- startDate+1
for (i in startDate:endDate){
  
  ########################################### build portfolio by history
  trainset <- USReturn[1:(i-1),]
  
  ################### Clustering
  numCluster <- 40
  clusterIteNum <- 500
  clusterResult <- clustering(trainset, numCluster, clusterIteNum)
  orderClusters <- clusterResult[[1]]
  clusterSize <- clusterResult[[2]]
  
  
  ################### change the portfolio 1
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
  ################## record change of portfolio
  # change <- length(setdiff(chosenEquity,chosenEquityB))
  # chosenEquityB <- chosenEquity 
  # print(setdiff(chosenEquity,chosenEquityB))
  # print(i)
  # print(change)
  
  
  ################### change the portfolio 2 - (200-40)
  ## ditribute the value equally across the clusters and among in each clusters
  #  portfolio[1,] <- 0
  #  for (equity in 1: length(orderClusters)){
  #    portfolio[1, names(orderClusters)[equity]] <- 
  #      1 / numCluster / clusterSize[as.numeric(orderClusters[equity])] * marketValue
  #  }
  
  ################### change the portfolio 3
  ## little difference from paper
  covDataFrame <- clusterResult[[3]]
  portfolio[1,] <- 0
  
  ##### calculate weights across clusters by variance of clusters
  V_piaoList <- c()
  for (clusters in 1:numCluster){
    equityInCluster <- names(orderClusters[orderClusters == clusters])
    covMatrixInCluster <- as.matrix(covDataFrame[equityInCluster,equityInCluster])
    if (nrow(covMatrixInCluster) > 1)
    {diagCovMatrix <- diag(diag(covMatrixInCluster))}else{
      diagCovMatrix <-covMatrixInCluster}
    inverseDiagCovMatrix <- solve(diagCovMatrix)
    w_piao <- diag(inverseDiagCovMatrix) / tr(inverseDiagCovMatrix)
    V_piao <- as.numeric(crossprod(w_piao,covMatrixInCluster) %*% w_piao)
    V_piaoList <- c(V_piaoList,V_piao)
  }
  wAcrossCluster <- 1 / V_piaoList / sum(1 / V_piaoList)

  ##### calculate weights in each clusters by variance of equities
  for (clustera in 1:numCluster){
    clusterW <- wAcrossCluster[clustera]
    equityInCluster <- names(orderClusters[orderClusters == clustera])
    if (length(equityInCluster) > 1){
      varianceVectorInCluster <- diag(as.matrix(covDataFrame[equityInCluster,equityInCluster]))
      wInCluster <- 1 / varianceVectorInCluster / sum(1 / varianceVectorInCluster)
      totalWListInCluster <- clusterW * wInCluster}
    else{
      portfolio[1, equityInCluster] <- clusterW
    }
    for (equities in 1:length(totalWListInCluster)){
      portfolio[1, names(totalWListInCluster)[equities]] <- 
        totalWListInCluster[names(totalWListInCluster)[equities]] * marketValue
    }
  }
  
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
  title(main = paste0(as.character(nTickets), " equities, in ",
                      as.character(numCluster), " clusters"))
  Sys.sleep(.001)
}
