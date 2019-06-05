library(R6)
DecisionTree <- R6Class("DecisionTree",
  private = list(
    tree = NULL
  ),
  public = list(
    initialize = function(){
      private$tree <- igraph::graph.empty()
    },
    getTree = function(){
      private$tree
    }
  )
)

createDataSet <- function(){
  canLiveInWater <- c('yes','yes','yes','no','no')
  hasFlipper <- c('yes','yes','no','yes','yes')
  isFish <- c("yes","yes","no","no","no")
  dataSet <- data.frame(canLiveInWater,hasFlipper,isFish,stringsAsFactors = F)
  dataSet
}
DecisionTree$set("public","createDataSet",createDataSet,overwrite=T)

calcShannonEnt <- function(dataset){
  numCols <- ncol(dataset)
  clasCol <- dataset[,numCols]
  tbl <- table(clasCol)
  tbl <- tbl[!tbl==0]
  prob <- tbl/sum(tbl)
  -sum(prob*log(prob,base=2))
}
DecisionTree$set("public","calcShannonEnt",calcShannonEnt,overwrite=T)
dT <- DecisionTree$new()
myDat <- dT$createDataSet()
dT$calcShannonEnt(myDat)

splitDataSet <- function(dataset, axis){
  retDataSet <- list()
  featureCol <- dataset[axis]
  vals <- unique(unlist(featureCol))
  for(val in vals){
    tmp <- subset(dataset,dataset[axis]==val)
    tmp <- tmp[-axis]
    retDataSet[[as.character(val)]] <- tmp
  }
  retDataSet
}
DecisionTree$set("public","splitDataSet",splitDataSet,overwrite = T)

chooseBestFeatureToSplit <- function(dataset){
  numFeatures <- ncol(dataset) -1
  H_ori <- self$calcShannonEnt(dataset)
  H_new <- c()
  for(i in 1:numFeatures){
    datasets <- self$splitDataSet(dataset,i)#按i分后的列表datasets
    H_tmp <- c()
    for(j in 1:length(datasets)){
      H_tmp <- c(H_tmp,self$calcShannonEnt(datasets[[j]]))
    }
    H_new <- c(H_new,sum(sapply(datasets,nrow)/nrow(dataset)*H_tmp))#h_new迭代、条件熵
  }
  
  infoGain <- H_ori - H_new
  return(which(infoGain==max(infoGain))[1])#[1]以防有相同的信息增益
}
DecisionTree$set("public","chooseBestFeatureToSplit",chooseBestFeatureToSplit,overwrite = T)
#The Process of data splitting:

dT <- DecisionTree$new()
myDat <- dT$createDataSet()
bestFeat_1 <- dT$chooseBestFeatureToSplit(myDat)
datasets_step1 <- dT$splitDataSet(myDat,bestFeat_1)

bestFeat_2 <- dT$chooseBestFeatureToSplit(datasets_step1[[1]])
datasets_step2 <- dT$splitDataSet(datasets_step1[[1]],bestFeat_2)

#绘制决策树：
library(igraph)
## Warning: package 'igraph' was built under R version 3.4.4
tree <- graph.empty() #生成一棵空树
#确立最佳划分特征
bestFeat_1 <- dT$chooseBestFeatureToSplit(myDat) 
bestFeatName_1 <- names(myDat)[bestFeat_1]
node <- vertex(vcount(tree)+1) #创建一个节点（这是根节点）
node$shape <- "circle" #该节点不是叶节点，所以指定圆形
node$label <- bestFeatName_1 #指定该节点的标签（即特征名称）
#添加该节点对应的数据集（会有用的）
node$data <- paste(rownames(myDat),collapse = ",") 
#按多数表决，添加该节点对应的分类信息（会有用的）
tmp <- table(myDat[ncol(myDat)])
node$cls <- names(tmp)[which.max(tmp)[1]] 
tree <- tree + node # add node to tree
#将刚刚生成的节点记录为当前父节点（以便从它生成后面的枝条）???
curParNode <- V(tree)[vcount(tree)]
#由于没有父节点，所以无需考虑边
plot(tree,vertex.label=V(tree)$lbl,edge.label=E(tree)$lbl,
     vertex.size=40,layout=layout_as_tree)

#x<-c(1,4,5)
#names(x)<-c("v1","v2","v3")
#m<-matrix(1:9,nr=3)
#names(m)
#row.names(m)

datasets_step1 <- dT$splitDataSet(myDat,bestFeat_1)
datasets_step1

bestFeat_2 <- dT$chooseBestFeatureToSplit(datasets_step1[[1]]) #确立最佳划分特征
bestFeatName_2 <- names(datasets_step1[[1]])[bestFeat_2]
node <- vertex(vcount(tree)+1) #创建一个新节点
node$shape <- "circle" #该节点不是叶节点，所以指定圆形
node$label <- bestFeatName_2 #指定该节点的标签（即特征名称）
node$data <- paste(rownames(datasets_step1[[1]]),collapse = ",") #添加该节点对应的数据集（会有用的）
tmp <- table(datasets_step1[[1]][ncol(datasets_step1[[1]])])
node$cls <- names(tmp)[which.max(tmp)[1]] #按多数表决，添加该节点对应的分类信息（会有用的）
tree <- tree + node
#由于有父节点，所以需要考虑边
eg <- edge(curParNode,V(tree)[vcount(tree)])    #创建从父节点指向该节点的边
eg$lbl <- names(datasets_step1)[1]          #指定该边的标签（其实就是父节点的一个取值）
tree <- tree+eg
plot(tree,vertex.label=V(tree)$lbl,edge.label=E(tree)$lbl,
     vertex.size=40,layout=layout_as_tree)

node <- vertex(vcount(tree)+1) #创建一个新节点
node$shape <- "rectangle" #该节是叶节点，所以指定矩形
#指定该节点的标签（即实例所属的类别）
node$label <- unique(datasets_step1[[2]][ncol(datasets_step1[[2]])]) 
#添加该节点对应的数据集（会有用的）
node$data <- paste(rownames(datasets_step1[[2]]),collapse = ",") 
node$cls <- node$label #该节点对应的分类信息（会有用的）
tree <- tree + node
#由于有父节点，所以需要考虑边
eg <- edge(curParNode,V(tree)[vcount(tree)])    #创建从父节点指向该节点的边
eg$lbl <- names(datasets_step1)[2]          #指定该边的标签（其实就是父节点的一个取值）
tree <- tree+eg
plot(tree,vertex.label=V(tree)$lbl,edge.label=E(tree)$lbl,
     vertex.size=40,layout=layout_as_tree)
#再次划分
datasets_step2 <- dT$splitDataSet(datasets_step1[[1]],bestFeat_2)
datasets_step2

curParNode <- V(tree)[2] 
node <- vertex(vcount(tree)+1) 
node$shape <- "rectangle" 
node$label <- unique(datasets_step2[[1]][ncol(datasets_step2[[1]])]) 
node$data <- paste(rownames(datasets_step2[[1]]),collapse = ",") 
node$cls <- node$label 
tree <- tree + node
eg <- edge(curParNode,V(tree)[vcount(tree)])    
eg$lbl <- names(datasets_step2)[1]         
tree <- tree+eg
plot(tree,vertex.label=V(tree)$lbl,edge.label=E(tree)$lbl,
     vertex.size=40,layout=layout_as_tree)



node <- vertex(vcount(tree)+1) 
node$shape <- "rectangle" 
node$label <- unique(datasets_step2[[2]][ncol(datasets_step2[[2]])]) 
node$data <- paste(rownames(datasets_step2[[2]]),collapse = ",") 
node$cls <- node$label 
tree <- tree + node
eg <- edge(curParNode,V(tree)[vcount(tree)])    
eg$lbl <- names(datasets_step2)[2]         
tree <- tree+eg
plot(tree,vertex.label=V(tree)$lbl,edge.label=E(tree)$lbl,
     vertex.size=40,layout=layout_as_tree)
