rm(list=ls())
library(R6)
KNN <- R6Class("KNN",
               public = list(
                 CreateDataSet = function(){
                   group <- matrix(c(1.0,1.1,1.0,1.0,0,0,0,0.1),ncol=2,byrow=T)
                   labels <- rep(c('A','B'),each=2)
                   dt <- as.data.frame(group)
                   dt$labels <- labels
                   dt
                 }
               )
)
knn <- KNN$new()
known <- knn$CreateDataSet()
plot(V2~V1,data=known,xlim=c(-0.2,1.2),ylim=c(-0.2,1.2),xlab="",ylab="")
text(known$V1,known$V2,known$labels,adj=c(1.5,0.5))

library(magrittr)
CalDistance <- function(item1,item2)
{
  (item1-item2)**2 %>% sum %>% sqrt
}
KNN$set("private","CalDistance",CalDistance,overwrite = T)

Classify0 <- function(known,unknown,k)
{
  #此处应该有很多关于程序健壮性的语句 
  mat <- as.matrix(known[,-ncol(known)])
  diffMat <- mat - matrix(rep(unlist(unknown),nrow(mat)),ncol=ncol(mat),byrow=T)#已知对象行数的未知
  distances <- diffMat**2 %>% rowSums() %>% sqrt
  sortedDistIndicies <- order(distances)#由小到大
  classCount <- table(known[sortedDistIndicies[1:k],ncol(known)])
  classCount <- sort(classCount,decreasing = T)
  return(names(classCount)[1])
}
KNN$set("public","Classify0",Classify0,overwrite = T)
unknown <- data.frame(V1=0,V2=0)
knn <- KNN$new()
knn$Classify0(known,unknown,3)
file2Dataframe <- function(filename,sep="\t",header=F)#读取文本文档数据
{
  tmp <- read.delim(filename,header=header,sep=sep)
}
KNN$set("public","file2Dataframe",file2Dataframe,overwrite = T)
knn <- KNN$new()
cwd <- getwd()
setwd("E:/数统研究生/描述统计/R语言/data")
known <- knn$file2Dataframe("datingTestSet2.txt")
setwd(cwd)
names(known) <- c("ffm","vgame","icecream","label")
plot(icecream~vgame,data=known,
     xlab="Percentage of Time Spent Playing Video game",
     ylab="Liters of Ice Cream Consumed Per Week") 
plot(vgame~ffm,data=known,col=as.factor(known$label),
     xlab="Frequent Flyier Miles Earned Per Year",
     ylab="Percentage of Time Spent Playing Video game")
legend("topleft",legend=unique(known$label),col=1:3)

autoNorm <- function(mat)
{
  normCol <- function(matCol){
    minVals <- min(matCol)
    maxVals <- max(matCol)
    return((matCol-minVals)/(maxVals-minVals))
  }
  return(apply(mat,2,FUN=normCol))
}
KNN$set("public","autoNorm",autoNorm,overwrite = T)
knn <- KNN$new()
hoRation <- 0.10
m <- dim(known)[1]
numTestVecs <- ceiling(m*hoRation)
unknown <- known[1:numTestVecs,-ncol(known)]
rightLabel <- known$label[1:numTestVecs]
known[,1:(ncol(known)-1)] <- knn$autoNorm(known[,1:(ncol(known)-1)])
unknown <- as.data.frame(knn$autoNorm(unknown))
predictLabel <- apply(unknown,1,FUN=knn$Classify0,known=known,k=3)
table(rightLabel == predictLabel)
