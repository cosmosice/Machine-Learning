library(R6)
#创建类
NBayes <- R6Class("NBayes")
#录入数据
loadDataSet <- function(){
  postingList <- list(
    c('my', 'dog', 'has', 'flea', 'problems', 'help', 'please'),
    c('maybe', 'not', 'take', 'him', 'to', 'dog', 'park', 'stupid'),
    c('my', 'dalmation', 'is', 'so', 'cute', 'I', 'love', 'him'),
    c('stop', 'posting', 'stupid', 'worthless', 'garbage'),
    c('mr', 'licks', 'ate', 'my', 'steak', 'how', 'to', 'stop', 'him'),
    c('quit', 'buying', 'worthless', 'dog', 'food', 'stupid')
  )
  postingList
}
NBayes$set("private","loadDataSet",loadDataSet,overwrite = T)
#取出出现过的词（1次）
createVocabList <- function(dataset){
  vocabset <- c()
  for(document in dataset){
    vocabset <- union(vocabset,document)
  }
  return(vocabset)
}
NBayes$set("private","createVocabList",createVocabList,overwrite = T)
#
createData <- function(){
  setofWords2Vec <- function(vocabList,inputset){#标记Voc里是否有输入的词，Y1N0
    as.numeric(vocabList %in% inputset)
  }
  tmp <- private$loadDataSet()
  vocabList <- private$createVocabList(tmp)
  tmp1 <- lapply(tmp, setofWords2Vec,vocabList=vocabList)#按对象检测0、1
  res <- as.data.frame(t(as.data.frame(tmp1)))
  row.names(res) <- 1:nrow(res)
  names(res) <- vocabList
  res$cls <- c("not","abusive","not","abusive","not","abusive")
  res
}
NBayes$set("public","createData",createData,overwrite = T)
nbayes <- NBayes$new()
nbayes$createData()
nbayes <- NBayes$new()
demoData <- nbayes$createData()
rn1 <- nrow(subset(demoData,my==0 & cls=='abusive'))
rn2 <- nrow(subset(demoData,cls=='abusive'))
rn1/rn2

featPropTbl <- function(dataset){
  featProb <- function(feat){
    tbl <- table(dataset[,ncol(dataset)],dataset[,feat])
    prop.table(tbl,margin = 1)
  }
  res <- lapply(names(dataset)[1:(ncol(dataset)-1)],featProb)
  names(res) <- names(dataset)[1:(ncol(dataset)-1)]
  res
}
NBayes$set("public","featPropTbl",featPropTbl,overwrite = T)
nbayes <- NBayes$new()
nbayesProbTbl <- nbayes$featPropTbl(demoData)

#tbl <- nbayesProbTbl[["flea"]]
#tbl[row.names(tbl)=="abusive",colnames(tbl)=="1"]

trainNB0 <- function(trainingData){
  model <- self$featPropTbl(trainingData)
  tbl <- table(trainingData[,ncol(trainingData)])
  tbl <- prop.table(tbl)
  attr(model,"priorProb") <- tbl ##a skill!
  model
}
NBayes$set("public","trainNB0",trainNB0,overwrite = T)

nbayes <- NBayes$new()
model <- nbayes$trainNB0(demoData)
attr(model,"priorProb")

  classifyNB <- function(model,w){
    lookupProbTbl <- function(cls,feat,featVal){#get先验概率P{X|Y=y}
      tbl <- model[[feat]]
      tbl[row.names(tbl)==as.character(cls),colnames(tbl)==as.character(featVal)]
    }
    getProb4Cls <- function(w,cls){#对W的每个属性计算P{X=x|Y=y}
      sapply(1:length(w),function(i){lookupProbTbl(cls,names(w)[i],w[i])})
    }
    priorTbl <- attr(model,"priorProb")
    res <- c()
    for(cls in names(priorTbl)){
      res <- c(res,prod(getProb4Cls(w,cls))*priorTbl[cls])#计算P{X=x|Y=y}*P{Y=y}
    }
    print(res)
    names(priorTbl)[which.max(res)[1]]
  }
NBayes$set("public","classifyNB",classifyNB,overwrite = T)

via <- c("yes","yes","no","yes","no","no","no")
mon <- c("yes","no","yes","no","yes","no","no")
groc <- c("no","no","yes","no","yes","yes","no")
unsub <- c("yes","no","no","no","yes","no","yes")
cls <- c("spam","spam","ham","ham","ham","ham","spam")
mydf <- data.frame(via=via,mon=mon,groc=groc,unsub=unsub,cls=cls,stringsAsFactors = F)
mydf

newemail <- c('yes', 'no', 'no', 'yes')
names(newemail) <- names(mydf)[1:(ncol(mydf)-1)]
nbayes <- NBayes$new()
model <- nbayes$trainNB0(mydf)
nbayes$classifyNB(model,newemail)

nbayes <- NBayes$new()
model <- nbayes$trainNB0(demoData)
nbayes$classifyNB(model,unlist(demoData[1,-ncol(demoData)]))