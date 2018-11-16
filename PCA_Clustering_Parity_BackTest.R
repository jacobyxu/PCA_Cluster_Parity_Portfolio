library(Hmisc); library(xlsx); library(dplyr); library(psych)
library(gtable); library(ggplot2); library(data.table);
rm(list = ls())

show_heatmap <- TRUE

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
  if (length(delete) > 0) UScorMatrix <- UScorMatrix[-(delete),-(delete)]
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
  if (length(delete) > 0) UScovMatrix <- UScovMatrix[-(delete),-(delete)]
  for (col in 1:ncol(UScovMatrix)){
    for (row in 1:nrow(UScovMatrix)){
      if (is.na(UScovMatrix[row,col]))
        UScovMatrix[row,col] <- 0 }}
  
  ################# change the data storage type
  corDataFrame <- data.frame(UScorMatrix)
  covDataFrame <- data.frame(UScovMatrix)
  #  covDataFrame <- data.frame(normalcovMatrix)
  formap <- corDataFrame
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
  
  if (show_heatmap) {
    corDataFramePlot <- corDataFrame
    corDataFramePlot$Name <- row.names(corDataFramePlot)
    corDataFrame.m <- melt(corDataFramePlot)
  flush.console()
    g <- ggplot(data = corDataFrame.m, aes(x=Name, y=variable, fill=value)) + geom_tile() +
      theme(axis.text = element_blank(), axis.title = element_blank())+
        scale_fill_gradient(low = "white", high = "red")
    print(g)
    Sys.sleep(.09)
    ###################### Clustering
    for (i in 1:clusterIteNum){
        corCluster <- kmeans(corDataFrame, centers=kcenters, iter.max=1)
        kcenters <- data.frame(corCluster$centers)
  
  ####################### reorder matrix
      orderCluster <- corCluster$cluster[order(corCluster$cluster)]
      nameList <- names(orderCluster)
      newCorDataFrame <- corDataFrame[nameList,nameList]
      if (i == clusterIteNum){
  ###################### plot heatmap
        newCorDataFrame$Name <- row.names(newCorDataFrame)
        newCorDataFrame.m <- melt(newCorDataFrame)
  flush.console()
        g <- ggplot(data = newCorDataFrame.m, aes(x=Name, y=variable, fill=value)) + geom_tile() +
          theme(axis.text = element_blank(), axis.title = element_blank())
        print(g)
        Sys.sleep(.09)
      }}
  }
  ###################### inital heatmap
  tmap1 <- as.matrix(formap)
  map1 <- t(tmap1)
  
  ###################### clustering
  corCluster <- kmeans(corDataFrame, centers=kcenters, iter.max=clusterIteNum)
  kcenters <- data.frame(corCluster$centers)
  
  ####################### reorder matrix and heatmap
  orderCluster <- corCluster$cluster[order(corCluster$cluster)]
  clusterSize <- corCluster$size
  nameList <- names(orderCluster)
  newCorDataFrame <- formap[nameList,]
  newCorDataFrame <- newCorDataFrame[,nameList]
  tmap2 <- as.matrix(newCorDataFrame)
  map2 <- t(tmap2)
  
  #   ####################### plot heatmap by ggplot
  if (show_heatmap) {
      newCorDataFrame$Name <- nameList #row.names(newCorDataFrame)
      newCorDataFrame.m <- melt(newCorDataFrame)
  flush.console()
      g <- ggplot(data = newCorDataFrame.m, aes(x=Name, y=variable, fill=value)) + geom_tile() +
          theme(axis.text = element_blank(), axis.title = element_blank())+
        scale_fill_gradient(low = "white", high = "red")
      print(g)
  }
  
  return(list(orderCluster, clusterSize, covDataFrame, map1, map2))
}


#############################################################################################

load("USfinal.RData")
rankOfMarket <- USfinal %>% filter(Date == max(Date)) %>% 
  arrange(desc(Market.Cap))

######################### choose how many tickers
nTickets <- 150
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
startDate <- 128
oneYear <- 252
intervial <- 21
marketValue <- 1
historyValue <- c(marketValue)
portfolio <- USReturn[1,-1]

change <- 0
endDate <- nrow(USReturn)
# i <- startDate
updateOrNor <- 0
for (i in startDate:endDate){
  if (updateOrNor %% intervial == 0){
    ########################################### build portfolio by history
    trainset <- USReturn[max(1,i-oneYear):(i-1),]
    
    ################### Clustering
    numCluster <- 30
    clusterIteNum <- 500
    clusterResult <- clustering(trainset, numCluster, clusterIteNum)
    orderClusters <- clusterResult[[1]]
    clusterSize <- clusterResult[[2]]
    
    ################### change the portfolio
    ## real reasonable portfolio
    covDataFrame <- clusterResult[[3]]
    portfolio[1,] <- 0
    
    ##### calculate weights across clusters by variance of clusters
    V_piaoList <- c()
    for (clustera in 1:numCluster){
      ## for each cluster, calculate the distribution of portfolio to get minimum cluster variance
      equityInCluster <- names(orderClusters[orderClusters == clustera])
      aalpha <- as.vector(rep(1,length(equityInCluster)))
      if (length(equityInCluster) > 1){
        covMatrixInCluster <- as.matrix(covDataFrame[equityInCluster,equityInCluster])
        divider <- as.numeric(aalpha %*% 
                                solve(covMatrixInCluster + diag(rep(0.00001,length(equityInCluster)))) 
                              %*% aalpha)
        wInCluster <- t(solve(covMatrixInCluster + diag(rep(0.00001,length(equityInCluster))))
                        %*% aalpha / divider)
        V_piao <- as.numeric(wInCluster %*% covMatrixInCluster %*% t(wInCluster))
      }
      ### Inf for delete outlier
      else{
        V_piao <- Inf#as.numeric(covDataFrame[equityInCluster,equityInCluster])
      }
      V_piaoList <- c(V_piaoList,V_piao)
    }
    
    ## distribute weights across clusters
    wAcrossCluster <- 1 / V_piaoList / sum(1 / V_piaoList)
      
    ##### calculate weights in each clusters by covariance matrix of equities
    for (clustera in 1:numCluster){
      clusterW <- wAcrossCluster[clustera]
        
      ## for each cluster, calculate the distribution of portfolio to get minimum cluster variance
      equityInCluster <- names(orderClusters[orderClusters == clustera])
      aalpha <- as.vector(rep(1,length(equityInCluster)))
      
      if (length(equityInCluster) > 1){
          covMatrixInCluster <- as.matrix(covDataFrame[equityInCluster,equityInCluster])
          divider <- as.numeric(aalpha %*% solve(covMatrixInCluster + diag(rep(0.00001,length(equityInCluster))))
                                %*% aalpha)
          wInCluster <- t(solve(covMatrixInCluster + diag(rep(0.00001,length(equityInCluster))))
                          %*% aalpha / divider)
          totalWListInCluster <- wInCluster * clusterW
          for (equities in 1:ncol(totalWListInCluster)){
            portfolio[1, colnames(totalWListInCluster)[equities]] <- 
              totalWListInCluster[1 ,colnames(totalWListInCluster)[equities]]
          }
      }
    }
  
    ########## check risk parity among clusters
    test <- FALSE
    if (test) { 
      ## inital 0 data.frame
      covDataTest <- covDataFrame
      for (i in 1:nrow(covDataTest)){
        for (j in 1:ncol(covDataTest)){
          covDataTest[i,j] <- 0
        }
      }
      ## impute sub-covMatrix of clusters
      for (clusterc in 1:numCluster){
        equityInCluster <- names(orderClusters[orderClusters == clusterc])
        covDataTest[equityInCluster,equityInCluster] <- covDataFrame[equityInCluster,equityInCluster] 
      }
      
      riskDistribution <- as.matrix(portfolio[1, portfolio != 0]) %*% as.matrix(covDataTest)
      riskAcrossClusters <- c()
      
    }
  }
  updateOrNor <- updateOrNor + 1
  ########################################### get the current return
  flush.console()
  tmp <- USReturn[i,-1]
  tmp <- exp(tmp)
  ## treat stop board
  tmp[is.na(tmp)] <- 1
#  print(max(abs(portfolio)))
  print(sum(portfolio))
  todayValue <- sum(tmp * portfolio, na.rm = T) * marketValue
  marketValue <- todayValue
  historyValue <- c(historyValue, marketValue)
  
  ################## plot
  time <- (startDate-1):i
  plot(time,historyValue,type = 'l')
  title(main = paste0(as.character(nTickets), " equities, in ",
                      as.character(numCluster), " clusters"))
  Sys.sleep(.001)
  print(USReturn[i,1]+ as.Date("1899-12-30"))
}
# save(historyValue, file = "historyValue.RData")
