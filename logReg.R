rm(list = ls())
via <- c("yes","yes","no","yes","no","no","no")
mon <- c("yes","no","yes","no","yes","no","no")
groc <- c("no","no","yes","no","yes","yes","no")
unsub <- c("yes","no","no","no","yes","no","yes")
cls <- c("spam","spam","ham","ham","ham","ham","spam")
mydf <- data.frame(via=via,mon=mon,groc=groc,unsub=unsub,cls=cls,stringsAsFactors = F)
mydf
mydf[,-ncol(mydf)] <- apply(mydf[,-ncol(mydf)],2, function(dfcol){dfcol <- c("yes"=1,'no'=0)[ as.character(dfcol)]})
mydf$cls <- c("ham"=1,'spam'=0)[ as.character(mydf$cls)]
mydf
#求导函数
f_prime <- function(x_old){
  return(-2*x_old+3)
}
cal <- function(){
  x_old <- 0
  x_new <- 6
  alpha <- 0.01
  precision <- 1e-5
  while(abs(x_new-x_old)>precision){
    x_old <- x_new
    x_new <- x_old+alpha*f_prime(x_old)
  }
  x_new
}
cal()#函数最大值

library(R6)
LogReg <- R6Class("LogReg")
sigmoid <- function(inX){
  return(1/(1+exp(-inX)))
}
gradAscent <- function(dataset,alpha=0.001,maxCycles=500){
  dataMatrix <- as.matrix(dataset[,1:(ncol(dataset)-1)])
  labelMat <- dataset[,ncol(dataset)]
  weights <- matrix(1,nc=1,nr=ncol(dataMatrix))
  for(k in 1:maxCycles){
    h <- private$sigmoid(dataMatrix %*% weights)
    error <- labelMat - h
    weights <- weights + alpha*(t(dataMatrix)%*%error)
  }
  weights#列向量
}
LogReg$set("private","sigmoid",sigmoid,overwrite = T)
LogReg$set("public","gradAscent",gradAscent,overwrite = T)
logReg <- LogReg$new()
weights <- logReg$gradAscent(mydf)
as.matrix(mydf[,1:(ncol(mydf)-1)]) %*% weights
dt <- read.table("E:/数统研究生/描述统计/R语言/data/c05/testSet.txt")
dt$V0 <- 1.0
dt <- dt[,c(4,1:3)]
names(dt) <- paste0("X",0:3)
logReg <- LogReg$new()
weights <- logReg$gradAscent(dt)
weights
plot(X2~X1, data=dt, pch=X3, col=as.factor(X3))

tmpx <- dt$X1
tmpy <- -1/weights[3]*(weights[2]*tmpx+weights[1])
plot(tmpx,tmpy,type='l')

plot(X2~X1, data=dt, pch=X3, col=as.factor(X3))
lines(tmpx,tmpy,lwd=3,col="blue")

stocGradAscent0 <- function(dataset, alpha=0.01){
  options('digits'=20)
  dataMatrix <- as.matrix(dataset[,1:(ncol(dataset)-1)])
  classLabels <- dataset[,ncol(dataset)]
  weights <- rep(1,ncol(dataMatrix))
  for(i in 1:nrow(dataset)){
    h <- private$sigmoid(sum(dataMatrix[i,]*weights))
    error <- classLabels[i] - h
    weights <- weights + alpha*error*dataMatrix[i,]
  }
  options('digits'=7)
  weights
}
LogReg$set("public","stocGradAscent0",stocGradAscent0,overwrite = T)

logReg <- LogReg$new()
weights <- logReg$stocGradAscent0(dt)
weights

tmpx <- dt$X1
tmpy <- -1/weights[3]*(weights[2]*tmpx+weights[1])
plot(X2~X1, data=dt, pch=X3, col=as.factor(X3))
lines(tmpx,tmpy,lwd=3,col="blue")

stocGradAscent0_tmp <- function(dataset, alpha=0.01, iterNum=200){
  options('digits'=20); dataMatrix <- as.matrix(dataset[,1:(ncol(dataset)-1)])
  classLabels <- dataset[,ncol(dataset)]; weights <- rep(1,ncol(dataMatrix))
  res <- list()
  for(j in 1:iterNum){
    for(i in 1:nrow(dataset)){
      h <- private$sigmoid(sum(dataMatrix[i,]*weights))
      error <- classLabels[i] - h
      weights <- weights + alpha*error*dataMatrix[i,]
      res[[length(res)+1]] <- weights
    }
  }
  options('digits'=7);  res
}
LogReg$set("public","stocGradAscent0_tmp",stocGradAscent0_tmp,overwrite = T)
logReg <- LogReg$new()
weightsList <- logReg$stocGradAscent0_tmp(dt)
tmpdf <- as.data.frame(t(as.data.frame(weightsList)))
row.names(tmpdf) <- 1:nrow(tmpdf)
names(tmpdf) <- c('w0','w1','w2')
library(ggplot2)
tmpdt <- reshape::melt(tmpdf)
## Using  as id variables
tmpdt$x <- rep(1:nrow(tmpdf),3)
g <- ggplot(tmpdt,aes(x=x,y=value,color=variable))
g <- g + geom_line() + facet_grid(variable~.,scales = "free_y")
plot(g)

logReg <- LogReg$new()
weightsList <- logReg$stocGradAscent0_tmp(dt)
tmpdf <- as.data.frame(t(as.data.frame(weightsList)))
row.names(tmpdf) <- 1:nrow(tmpdf)
names(tmpdf) <- c('w0','w1','w2')
library(ggplot2)
tmpdt <- reshape::melt(tmpdf)
tmpdt$x <- rep(1:nrow(tmpdf),3)
g <- ggplot(tmpdt,aes(x=x,y=value,color=variable))
g <- g + geom_line() + facet_grid(variable~.,scales = "free_y")
plot(g)

stocGradAscent1 <- function(dataset, numIter=150){
  options('digits'=20); dataMatrix <- as.matrix(dataset[,1:(ncol(dataset)-1)])
  classLabels <- dataset[,ncol(dataset)]; weights <- rep(1,ncol(dataMatrix))
  for(j in 1:numIter){
    dataIndex <- 1:nrow(dataMatrix)
    for(i in 1:nrow(dataset)){
      alpha <- 4/(1.0+j+i)+0.0001  #Focus
      randIndex <- sample(1:length(dataIndex),1) #Focus
      h <- private$sigmoid(sum(dataMatrix[randIndex,]*weights))
      error <- classLabels[randIndex] - h
      weights <- weights + alpha*error*dataMatrix[randIndex,]
      dataIndex <- dataIndex[-randIndex]
    }
  }
  options('digits'=7);weights
}
LogReg$set("public","stocGradAscent1",stocGradAscent1,overwrite = T)
logReg <- LogReg$new()
weights <- logReg$stocGradAscent1(dt)
tmpx <- dt$X1
tmpy <- -1/weights[3]*(weights[2]*tmpx+weights[1])
plot(X2~X1, data=dt, pch=X3, col=as.factor(X3))
lines(tmpx,tmpy,lwd=3,col="blue")

stocGradAscent1_tmp <- function(dataset, numIter=150){
  options('digits'=20); dataMatrix <- as.matrix(dataset[,1:(ncol(dataset)-1)])
  classLabels <- dataset[,ncol(dataset)]; weights <- rep(1,ncol(dataMatrix))
  res <- list()
  for(j in 1:numIter){
    dataIndex <- 1:nrow(dataMatrix)
    for(i in 1:nrow(dataset)){
      alpha <- 4/(1.0+j+i)+0.0001  #Focus
      randIndex <- sample(1:length(dataIndex),1) #Focus
      h <- private$sigmoid(sum(dataMatrix[randIndex,]*weights))
      error <- classLabels[randIndex] - h
      weights <- weights + alpha*error*dataMatrix[randIndex,]
      dataIndex <- dataIndex[-randIndex]
      res[[length(res)+1]] <- weights
    }
  }
  options('digits'=7);  res
}
LogReg$set("public","stocGradAscent1_tmp",stocGradAscent1_tmp,overwrite = T)
logReg <- LogReg$new()
weightsList <- logReg$stocGradAscent1_tmp(dt)
tmpdf <- as.data.frame(t(as.data.frame(weightsList)))
row.names(tmpdf) <- 1:nrow(tmpdf)
names(tmpdf) <- c('w0','w1','w2')

library(ggplot2)
tmpdt <- reshape::melt(tmpdf)
tmpdt$x <- rep(1:nrow(tmpdf),3)
g <- ggplot(tmpdt,aes(x=x,y=value,color=variable))
g <- g + geom_line() + facet_grid(variable~.,scales = "free_y")
plot(g)

classify <- function(inX,weights){
  prob <- sigmoid(sum(inX*weights))
  ifelse(prob > 0.5, 1.0,0.0)
}
LogReg$set("public","classify",classify,overwrite = T)
logReg <- LogReg$new()
weights <- logReg$gradAscent(mydf)
t(weights)
logReg$classify(mydf[1,1:(ncol(mydf)-1)],weights)
