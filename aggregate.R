setwd("F:/mywd/riskParity/code")
library(Hmisc); library(tidyr); library(dplyr); library(psych)
library(gtable); library(ggplot2); library(data.table); library(caret)
library(CVXfromR); library(vioplot); library(quadprog)
setup.dir = "D:/Matlab/packages/cvx-w64/cvx"


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
rf = 0.0162067
rf = 0.0078
globalUniverse = names(USReturn)
######################## start day
beginning <- min(USReturn$USOutTime) + as.Date('1899-12-30')




###########################################################################  backtest

library(caret)
library(stats)
startDate <- 128
marketValue <- 1
historyValueMvo <- c(marketValue)
historyValueAo <- c(marketValue)
historyValueRp <- c(marketValue)
historyValueCrp <- c(marketValue)
marketValueMvo <- 1
marketValueAo <- 1
marketValueRp <- 1
marketValueCrp <- 1
portfolio <- USReturn[1,-1]

change <- 0
endDate <- nrow(USReturn)
interval <- 1
# endDate <- 120
# i <- startDate+1

numCluster <- 30
clusterIteNum <- 500

for (i in seq(from = startDate, to = endDate, by  =interval)){
  
  ########################################### build portfolio by history
  featureStart = max(1,i-252)
  trainset <- USReturn[featureStart:(i-1),]
  ################### Clustering

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


  

  if ((i-startDate)%%21==0){
    portfolio[1,] <- 0
    pflMvo <- t(as.matrix(portfolio)); 
    pflAo <- t(as.matrix(portfolio));
    pflRp <- t(as.matrix(portfolio));
    pflCrp <- t(as.matrix(portfolio));
    #insert portfolios here
    nUniverse = nrow(covDataFrame)
    vec1 <- as.matrix(rep.int(1,nUniverse))
  
    #markovitz
    modifiedCov <- as.matrix(covDataFrame)+(1/10000)*diag(nrow(covDataFrame))
    
#     invCov = solve(modifiedCov)
#     tmp_pflMvo = (invCov%*%vec1)/as.numeric((t(vec1)%*%invCov%*%vec1))
    dvec <- rep(0,nUniverse)
    Amat <- cbind(1,diag(nUniverse))
    bvec <- c(1,rep(0,nUniverse))
    meq <- 1
    
    kn<-0
    mvoFlag = 0
    while (mvoFlag == 0){
      tryCatch({
        mvResult <- solve.QP(modifiedCov,dvec,Amat,bvec = bvec,meq = meq)
        mvoFlag <- 1
      },warning = function(w){
        
      },error = function(e){
      },finally = {
        if(mvoFlag ==0){
          kn <- kn +1
          modifiedCov <- modifiedCov + (2^(kn)/10000)*diag(nrow(covDataFrame))
          mvoFlag <- 0
        }
      })
    }
    
    mvResult$solution[mvResult$solution <=1e-5] <- 0
    tmp_pflMvo = mvResult$solution
  
    #1/n
    tmp_pflAo <- rep.int(1,nUniverse)/nUniverse
  
    #riskParity
    myCov = as.matrix(covDataFrame)
    #Number of equities
    
    myCov = modifiedCov+(1/10000)*diag(nrow(covDataFrame))
  
    myCov.eig <- eigen(myCov)
    eigValue = myCov.eig$values
    eigValue[eigValue<0] = 0
    myCov.sqrt <- myCov.eig$vectors %*% diag(sqrt(eigValue)) %*% solve(myCov.eig$vectors)
  
    sqCov <- myCov.sqrt
  
    currentUniverse <- names(covDataFrame)
    n <- nUniverse
    #Level of 
    c <- 2
    
  #                      "minimize(w'*myCov*w)",
    cvxcode <- paste("cvx_precision low",
                     "variables w(n)",
                     "minimize(norm(sqCov*w))",
                     "subject to",
                     "geo_mean(w) >= exp(c)^(1/n)",
                     sep=";")
    rp <- CallCVX(cvxcode, const.vars=list(c=c, n=n,sqCov = sqCov),
                  opt.var.names="w", setup.dir=setup.dir)
    
    w = as.matrix(rp$w)
    tmp_pflRp = as.matrix(rp$w)
    tmp_pflRp = tmp_pflRp/sum(tmp_pflRp)
  
    #CRP
  #### calculate weights across clusters by variance of clusters
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
  
  # distribute weights across clusters
  wAcrossCluster <- 1 / V_piaoList / sum(1 / V_piaoList)
  
  #### calculate weights in each clusters by covariance matrix of equities
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
        pflCrp[colnames(totalWListInCluster)[equities],] <- 
          totalWListInCluster[1 ,colnames(totalWListInCluster)[equities]]
      }
    }

  }
    #HRP
  
    for (j in 1:nUniverse){
      if(currentUniverse[j] %in% globalUniverse){
        pflMvo[currentUniverse[j],] <-tmp_pflMvo[j]
        pflAo[currentUniverse[j],] <-tmp_pflAo[j]
        pflRp[currentUniverse[j],] <-tmp_pflRp[j]
      }else{
        #do nothing
      }
    }
  }
  ########################################### get the current return
  flush.console()
  tmp <- USReturn[i:(i+interval-1),-1]
  tmp <- exp(tmp)
  tmp[is.na(tmp)] <- 1
#   todayValue <- sum(tmp * portfolio, na.rm = T)
#   marketValue <- todayValue
  historyValueMvo <- c(historyValueMvo, marketValueMvo * rowSums(tmp * pflMvo, na.rm = T))
  marketValueMvo <- historyValueMvo[length(historyValueMvo)]
#   print(rowSums(tmp * pflMvo, na.rm = T))
  
  historyValueAo <- c(historyValueAo, marketValueAo * rowSums(tmp * pflAo, na.rm = T))
  marketValueAo <- historyValueAo[length(historyValueAo)]
  historyValueRp <- c(historyValueRp, marketValueRp*rowSums(tmp * pflRp, na.rm = T))
  marketValueRp <- historyValueRp[length(historyValueRp)]
  historyValueCrp <- c(historyValueCrp, marketValueCrp * rowSums(tmp * pflCrp, na.rm = T))
  marketValueCrp <- historyValueCrp[length(historyValueCrp)]
  
  
################## plot
cl = c("cadetblue3","chartreuse3","brown3","darkgoldenrod3")
time <- (startDate-1):i

plot(startDate,1,xlim = c(startDate-1,i),ylim = c(0.70,max(marketValueMvo,marketValueAo,marketValueRp,marketValueCrp)),type = 'n')
lines(time,historyValueMvo,col = cl[1],type = 'l')
lines(time,historyValueAo,col = cl[2],type = 'l')
lines(time,historyValueRp,col = cl[3],type = 'l')
lines(time,historyValueCrp,col = cl[4],type = 'l')
legend("topleft", legend = c("MV Portfolio","1/n Portfolio","Risk Parity Portfolio","Cluster Risk Parity"), col=cl, pch=1)
title(main = paste0(as.character(nTickets), " equities, in ",
                    as.character(numCluster), " clusters"))


#   Sys.sleep(.0001)
}

strategyList = c('MV','1/n','RP','CRP')
N = length(historyValueRp)
# N = 800
stdMvo = sd(log(historyValueMvo[1:N]),na.rm=F)*sqrt(252/N)
stdAo = sd(log(historyValueAo[1:N]),na.rm=F)*sqrt(252/N)
stdRp = sd(log(historyValueRp[1:N]),na.rm=F)*sqrt(252/N)
stdCrp = sd(log(historyValueCrp[1:N]),na.rm=F)*sqrt(252/N)



muMvo = log(historyValueMvo[N])*252/N
muAo = log(historyValueAo[N])*252/N
muRp = log(historyValueRp[N])*252/N
muCrp = log(historyValueCrp[N])*252/N

sharpMvo = (muMvo-rf)/stdMvo
sharpAo = (muAo-rf)/stdAo
sharpRp = (muRp-rf)/stdRp
sharpCrp = (muCrp-rf)/stdCrp

rcMvo = tmp_pflMvo*(myCov %*% tmp_pflMvo)
rcAo = tmp_pflAo*(myCov %*% tmp_pflAo)
rcRp = tmp_pflRp*(myCov %*% tmp_pflRp)
rcCrp = matrix(pflCrp)*(myCov %*% matrix(pflCrp))

print(c(muMvo,stdMvo,sharpMvo,sd(rcMvo)))
print(c(muAo,stdAo,sharpAo,sd(rcAo)))
print(c(muRp,stdRp,sharpRp,sd(rcRp)))
print(c(muCrp,stdCrp,sharpCrp,sd(rcCrp)))#

par(mfrow = c(2,2)) 
ymin1 = min(c(abs(rcMvo),abs(rcAo),abs(rcRp),abs(rcCrp)))
ymax1 = max(c(abs(rcMvo),abs(rcAo),abs(rcRp),abs(rcCrp)))
ylim1 = c(ymin1,ymax1)
# vioplot(abs(rcMvo),abs(rcAo),abs(rcRp),abs(rcCrp),names = c("MV","1/n","RP","CRP"),col = "antiquewhite1")
barplot(abs(rcMvo),main = "MV portfolio",beside = T,axes = F, col = cl[1],ylim = ylim1)
barplot(abs(rcAo),main = "1/n portfolio",beside = T,axes = F, col = cl[2],ylim = ylim1)
barplot(abs(rcRp),main = "Risk Parity portfolio",beside = T,axes = F, col = cl[3],ylim = ylim1)
barplot(abs(rcCrp),main = "Cluster Risk Parity portfolio",beside = T,axes = F, col =cl[4],ylim = ylim1)

par(mfrow = c(2,2))
ymin2 = min(c(abs(pflMvo)/sum(abs(pflMvo)),abs(pflAo)/sum(abs(pflAo)),abs(pflRp)/sum(abs(pflRp)),abs(pflCrp)/sum(abs(pflCrp))))
ymax2 = max(c(abs(pflMvo)/sum(abs(pflMvo)),abs(pflAo)/sum(abs(pflAo)),abs(pflRp)/sum(abs(pflRp)),abs(pflCrp)/sum(abs(pflCrp))))
ylim2 = c(ymin2,ymax2)
barplot(abs(pflMvo)/sum(abs(pflMvo)),main = "MV portfolio",beside = T, axes = F, col = cl[1],ylim = ylim2)
barplot(abs(pflAo)/sum(abs(pflAo)),main = "1/n portfolio",beside = T, axes = F, col = cl[2],ylim = ylim2)
barplot(abs(pflRp)/sum(abs(pflRp)),main = "Risk Parity portfolio",beside = T,axes = F, col = cl[3],ylim = ylim2)
barplot(abs(pflCrp)/sum(abs(pflCrp)),main = "Cluster Risk Parity portfolio",beside = T,axes = F, col = cl[4],ylim = ylim2)

#====================================================================
window = c(1,741+252*4);N = window[2]-window[1]+1
cl = c("cadetblue3","chartreuse3","brown3","darkgoldenrod3")
recTable = rbind(historyValueMvo,historyValueAo,historyValueRp,historyValueCrp)
time <- (startDate-1+window[1]):(startDate-1+window[2])
stat = c()

par(mfrow = c(2,2))
plot(time[1],1,xlim = c(time[1],time[length(time)]),ylim = c(0.50,3),xlab = "Time",ylab = "Return",type = 'n')
Axis(side=2, labels=FALSE)
for (i in 1:nrow(recTable)){
  valRec <- recTable[i,window[1]:window[2]]/recTable[i,window[1]]
  lines(time,valRec, col=cl[i],type = 'l')
  mu = log(valRec[N])*252/N
  vol = sd(log(valRec),na.rm=F)*sqrt(252/N)
  sharpe = (mu-rf)/vol
  stat = rbind(stat,c(mu,vol,sharpe))
}
legend("topleft", legend = strategyList, col=cl, pch=1)
title(sub = "Profit and loss curve",outer = F) #sub = paste0(as.character(nTickets), " equities, in ",as.character(numCluster), " clusters"),

plot(startDate,1,xlim = c(time[1],time[length(time)]),ylim = c(0.80,1.3),xlab = "Time",ylab = "Net Change in Return",type = 'n')
Axis(side=2, labels=FALSE)
for (i in 1:nrow(recTable)){
  valRec <- recTable[i,window[1]:window[2]]/recTable[i,window[1]]
  lines(time[2:length(time)],valRec[2:length(valRec)]/valRec[1:length(valRec)-1], col=cl[i],type = 'l')
}
legend("topright",cex - 0.8,legend = strategyList, col=cl, pch=1)
title(sub = "Net Change in P/L")

plot(1,0,xlim = c(1,4),ylim = c(-0.3,0.3),xaxt='n',xlab = "Approaches",ylab = "Mean Return and CI",type = 'n')
Axis(side=2, labels=FALSE)
arrows(1:4,stat[,1]-stat[,2],1:4,stat[,1]+stat[,2],code=3,length=0.2,angle=90,col='black')
points(1:4,stat[,1],pch=19,col = cl,cex = 1.5)
axis(1, at=1:4, labels=strategyList)
legend("bottomleft", legend = strategyList, col=cl, pch=1)
title(sub = "Mean Return and Confident Interval")

plot(1,0,xlim = c(0.06,0.12),ylim = c(0.02,0.1),xlab = "Return",ylab = "Volatility",type = 'n')
points(stat[,1],stat[,2],col = cl,cex = 2, pch = 19)
Axis(side=2, labels=T)
legend("bottomright", legend = strategyList, col=cl, pch=19)

statDF = data.frame(stat)
row.names(statDF) = strategyList
names(statDF) = c('Return','Vol','Sharpe')
print(statDF)

