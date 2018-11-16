    #setwd("/home/creatrol/GHack")
    library(Hmisc); library(xlsx); library(dplyr); library(gtable); library(ggplot2); library(data.table)

Load raw data and transformation considering currencies
-------------------------------------------------------

    if (FALSE){
      ############################################# Load data
      Equities_data <- read.csv('data/Equities_data.csv',1)
      Equities_metadata <- read.csv('data/Equities_metadata.csv',1)
      Currencies_data <- read.csv('data/Currencies_data.csv',1)
      Currencies_metadata <- read.csv('data/Currencies_metadata.csv',1)
      
      ######################### select open price as daily price & join datasets
      Edata <- select(Equities_data, 1:3,8)
      rm(Equities_data)
      EdataM <- select(Equities_metadata, 1,3)
      Cdata <- select(Currencies_data,1:3)
      CdataM <- select(Currencies_metadata,1,3)
      semiData <- left_join(Edata, EdataM)
      
      ######################### treat date
      semiData$Date <- as.numeric(
        as.Date(as.character(semiData$Date), origin = '1899-12-30') - as.Date('1899-12-30'))
      semiCurrencies <- left_join(Cdata, CdataM)
      semiCurrencies$Date <- as.numeric(
        as.Date(as.character(semiCurrencies$Date), origin = '1899-12-30') - as.Date('1899-12-30'))
      semiCurrencies <- semiCurrencies %>% rename(Rate = Open)%>%select(-1)
      
      ######################## use data.table to realize rolling for missing data
      semiCurrenciesTable <- data.table(semiCurrencies)
      rm(semiCurrencies)
      semiDataTable <- data.table(semiData)
      rm(semiData)
      setkey(semiCurrenciesTable, CRNCY, Date)
      setkey(semiDataTable, CRNCY, Date)
      mydata <- semiCurrenciesTable[semiDataTable, roll = +Inf]
      myData <- data.frame(mydata)
      rm(mydata)
      
      ######################### focus on US market
      myData <- myData %>% mutate(Rate2 = ifelse(grepl("_US_",as.character(Ticker)),1,Rate)) %>% 
        mutate(Price = Open/Rate2) %>%
        select(Ticker, Price, Date, Market.Cap)
      finalOut <- myData[complete.cases(myData),]
      USfinal <- finalOut[grepl("_US_",as.character(finalOut$Ticker)),]
      
      ######################### save data
      save(USfinal, file = "USfinal.RData")
      head(USfinal, 5)
      rm(list = ls())
    }

### using logathemic return

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

    ##   USOutTime AAPL_US_Equity ABBV_US_Equity ABT_US_Equity ACN_US_Equity
    ## 2     38232    0.034381708             NA   0.008834112  0.0083778099
    ## 3     38233   -0.013339354             NA   0.014400450 -0.0114417724
    ## 4     38237    0.010535411             NA   0.000000000 -0.0080878546
    ## 5     38238    0.008427569             NA  -0.011076721  0.0199087739
    ## 6     38239    0.011153257             NA  -0.001898630 -0.0003791469

    ######################## start day
    beginning <- min(USReturn$USOutTime) + as.Date('1899-12-30')

Build Clustering Function
-------------------------

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

      return(list(orderCluster, clusterSize, covDataFrame, map1, map2))
    }

back test
=========

    library(caret)

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:survival':
    ## 
    ##     cluster

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
      #print(sum(portfolio))
      todayValue <- sum(tmp * portfolio, na.rm = T) * marketValue
      marketValue <- todayValue
      historyValue <- c(historyValue, marketValue)
      
      ################## plot
      time <- (startDate-1):i

      #Sys.sleep(.001)
      #print(USReturn[i,1]+ as.Date("1899-12-30"))
    }
    plot(time,historyValue,type = 'l')
    title(main = paste0(as.character(nTickets), " equities, in ",
                        as.character(numCluster), " clusters"))

![](PCA_Clustering_Parity_BackTest_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    # save(historyValue, file = "historyValue.RData")
