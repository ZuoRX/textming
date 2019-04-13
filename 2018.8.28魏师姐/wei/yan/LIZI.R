rm(list=ls()) 
library(NLP)
library(tm)
library(rJava)
library(Rwordseg)
library(SnowballC)
library(MASS)
library(RColorBrewer)
library(wordcloud)
library(pcaPP)
library(rainbow)
library(Rcpp)
library(cluster)
library(mvtnorm)
library(hdrcde)
library(locfit)
library(ash)
library(KernSmooth)
library(misc3d)
library(rgl)
library(ks)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(sp)
library(maptools)
library(grid)
library(vcd)
library(topicmodels)
library(randomForest)
library(rFerns)
library(ranger)
library(Boruta) 
library(lattice)
library(caret)
library(slam)
library(Matrix)
library(foreach)
library(glmnet)

train <- read.csv("C:/Users/apple/Desktop/train 0-1 100.csv")
csv <- train

#安装词典：
installDict("C:\\Users\\apple\\Desktop\\医疗\\law.scel", "law")
installDict("C:\\Users\\apple\\Desktop\\医疗\\med.scel", "med")
installDict("C:\\Users\\apple\\Desktop\\医疗\\lawbook.scel", "lawbook")

#人名识别开启
segment.options(isNameRecognition = TRUE) #分离人名

#开始分词
segword<-function(csv)
{
  pp<-as.character(csv$text)
  rf<-segmentCN(pp,nature=TRUE)#nature:是否分离词性，比如动词、名词等
  rf
}


nvword<-function(wf)
{
  wf.n=wf
  for(i in 1:length(wf)){
   
    wf.n[[i]]<-wf[[i]][which(names(wf[[i]])=='n'|names(wf[[i]])=='v'|names(wf[[i]])=='lawbook'|names(wf[[i]])=='nz'|names(wf[[i]])=='med'|names(wf[[i]])=='law')]
    wf.n[[i]]<-wf.n[[i]][which(is.na(match(wf.n[[i]],c("是","有","可以"))))]#初步降维
  }#nz:其他专业名词 which(is.na()):返回缺失值位置
  ovid<-Corpus(VectorSource(wf.n))#生成新的语料库
  ovid
}



modelword<-function(ovid,csv)
{
  dtm<-DocumentTermMatrix(ovid,control = list(wordLengths = c(2,Inf)))#生成词频矩阵 c为concatenate的缩写，是把一系列参数转换成向量的
  rtdtm<-removeSparseTerms(dtm, 0.9)#剔除稀疏词汇 sparse=0.9                    #INF:infinity  
  rtdtm<-rtdtm[row_sums(rtdtm)>0,]#R使用rowSums函数对行求和，使用colSums函数对列求和
  tfidf<-weightBin(rtdtm)
  #tfidf<-weightTfIdf(rtdtm1)
  dtmtx<-as.matrix(tfidf)
  doc<-as.integer(dimnames(rtdtm[row_sums(rtdtm)>0,])$Docs) #dimnames(x):重新设置对象的名称
  nb<-as.matrix(csv$type[doc])
  modeldata<-data.frame(nb,dtmtx)
  rtn<-list(modeldata,doc)
  rtn
}


wf1<-segword(csv)
ovid1<-nvword(wf1)
rtn1<-modelword(ovid1,csv)
modeldata1<-rtn1[[1]]
doc1<-rtn1[[2]]

#特征选择提取
#方法1：lasso
#输出建议选择变量个数
dd<-as.matrix(modeldata1[,-1])
cvfit<-cv.glmnet(dd,modeldata1$nb,family="multinomial",type.measure="mae") 
plot(cvfit)

res<-list()
vf<-as.matrix(coef(cvfit)$'0')
res[[1]]<-names(vf[vf!=0,])[-1]
vf<-as.matrix(coef(cvfit)$'1')
res[[2]]<-names(vf[vf!=0,])[-1]
modelky1<-unique(unlist(res))

#方法2：Boruta（随机森林，运行慢）
library(Boruta)
na.omit(modeldata1)->modeldata
Boruta(as.factor(nb)~.,data=modeldata,doTrace=2)->Boruta.res
modelky2<-getSelectedAttributes(Boruta.res)
plot(Boruta.res,sort=FALSE)
modelly2


#方法3：DFx
kk <-list()

ddb1<-subset(modeldata1,modeldata1$nb=="0")#选取modeldata1中元素
dfn1<-apply(ddb1[,-1],2,sum)
kk[[1]] <- names(dfn1[which(dfn1>10)])

ddb2<-subset(modeldata1,modeldata1$nb=="1")
dfn2<-apply(ddb2[,-1],2,sum)
kk[[2]] <- names(dfn2[which(dfn2>30)])

modelky3 <-unique(unlist(kk))
modelky3

#方法4:卡方CHI
chisq<-function(x)
{mx<-table(modeldata1$nb,x)
ct<-chisq.test(mx)
ct$statistic}
mdc<-apply(modeldata1[,-1],2,chisq)
summary(mdc)
modelky4<-names(mdc[which(mdc>3)])
modelky4

chisq<-function(x)
{mx<-table(modeldata1$nb,x)
ct<-chisq.test(mx)
ct$statistic}
mdc<-apply(modeldata1[modelky3],2,chisq)
summary(mdc)
modelky4<-names(mdc[which(mdc>2)])


#6.建模数据准备
prepword<-function(modelky,ovid,unf)
{
  dic<-c(as.character(modelky))
  pdtm<-DocumentTermMatrix(ovid,list(dictionary=dic,wordLengths = c(2,Inf)))
  pdtm<-pdtm[row_sums(pdtm)>0,]
  pbin<-weightBin(pdtm)
  pbintx<-as.matrix(pbin)
  pdoc<-as.integer(dimnames(pdtm[row_sums(pdtm)>0,])$Docs)
  pnb<-as.matrix(csv$type[pdoc])
  pmodeldata<-data.frame(pnb,pbintx)
  prtn<-list(pmodeldata,pdoc)
}

prepword1<-function(modelky,ovid,unf)
{
  dic<-c(as.character(modelky))
  pdtm<-DocumentTermMatrix(ovid,list(dictionary=dic,wordLengths = c(2,Inf)))
  pdtm<-pdtm[row_sums(pdtm)>0,]
  pbin<-weightBin(pdtm)
  pbintx<-as.matrix(pbin)
  pdoc<-as.integer(dimnames(pdtm[row_sums(pdtm)>0,])$Docs)
  pnb<-as.matrix(test$type[pdoc])
  pmodeldata<-data.frame(pnb,pbintx)
  prtn<-list(pmodeldata,pdoc)
}

wf1<-segword(train)
ovid1<-nvword(wf1)
prtn1<-prepword(modelky3,ovid1,train)  
traindata<-prtn1[[1]]
pdoc1<-prtn1[[2]]
zz<-c(1:length(train[,1]))
qyn1<-train[which(is.na(match(zz,pdoc1))),]$type

test <- read.csv("C:/Users/apple/Desktop/test 0-1 60.csv")
wf2<-segword(test)
ovid2<-nvword(wf2)
prtn2<-prepword1(modelky3,ovid2,test)
testdata<-prtn2[[1]]
pdoc2<-prtn2[[2]]
tt<-c(1:length(test[,1]))
qyn2<-test[which(is.na(match(tt,pdoc2))),]$type


-----------------------------------------------------------------
  wf1<-segword(train)
ovid1<-nvword(wf1)
prtn1<-prepword(modelky4,ovid1,train)  
traindata<-prtn1[[1]]
pdoc1<-prtn1[[2]]
zz<-c(1:length(train[,1]))
qyn1<-train[which(is.na(match(zz,pdoc1))),]$type

test <- read.csv("C:/Users/apple/Desktop/test 0-1 60.csv")
wf2<-segword(test)
ovid2<-nvword(wf2)
prtn2<-prepword1(modelky4,ovid2,test)
testdata<-prtn2[[1]]
pdoc2<-prtn2[[2]]
tt<-c(1:length(test[,1]))
qyn2<-test[which(is.na(match(tt,pdoc2))),]$type
-----------------------------------------------------------------------
  
  #SVM
  library(kernlab)
sample_ksvm  <-  ksvm(pnb~., data=traindata,type = "C-svc", kernel = "rbfdot",prob.model = TRUE)
svmCl  <-  predict(sample_ksvm,traindata[,-1],type = "probabilities")
svmpred<-apply(svmCl,1,function(x) which(x==max(x)))
svmpred<-c(svmpred,rep(1,2,length(qyn1)))
trainyn<-c(traindata$pnb,qyn1)
svmTable <-table(SVM=svmpred, sample=trainyn)
svmTable
svmpred

#训练集合的准确率，覆盖率
v1 = sum(diag(svmTable))/sum(svmTable)
v2 = sum(diag(svmTable))/100
f=2/(1/v1 + 1/v2)
f


svmT <-  predict(sample_ksvm,testdata[,-1],type = "probabilities")
svmpred<-apply(svmT,1,function(x) which(x==max(x)))
svmpred<-c(svmpred,rep(1,2,length(qyn2)))
testyn<-c(testdata$pnb,qyn2)
svmTable <-table(SVM=svmpred, sample=testyn)
svmTable
svmpred
#测试集合准确率，召回率，F值
v1 = sum(diag(svmTable))/sum(svmTable)
v2 = sum(diag(svmTable))/60
f=2/(1/v1 + 1/v2)
f

#神经网络
library(RSNNS)
tTargets = decodeClassLabels(traindata$pnb)
model.net <- mlp(traindata[,-1], tTargets, size=5, learnFunc="Quickprop", learnFuncParams=c(0.1, 2.0, 0.0001, 0.1),maxit=100)
predictions <- predict(model.net,traindata[,-1])
nntpred<-apply(predictions,1,function(x) which(x==max(x)))
nntpred<-c(nntpred,rep(1,2,length(qyn1)))
trainyn<-c(traindata$pnb,qyn1)
nntTable <-table(NNT=nntpred, sample=trainyn)
nntTable
nntpred
sum(diag(nntTable))/sum(nntTable)


predictions <- predict(model.net,testdata[,-1])
nntpred<-apply(predictions,1,function(x) which(x==max(x)))
nntpred<-c(nntpred,rep(1,2,length(qyn2)))
testyn<-c(testdata$pnb,qyn2)
nntTable <-table(NNT=nntpred, sample=testyn)
nntTable
nntpred
v1 = sum(diag(nntTable))/sum(nntTable)
v2 = sum(diag(nntTable))/60
f=2/(1/v1 + 1/v2)
f

#KNN
library(class)
sample_knnCl  <-  knn(traindata[,-1], traindata[,-1], traindata$pnb)
prfpred<-c(as.integer(sample_knnCl),rep(1,2,length(qyn1)))
trainyn<-c(traindata$pnb,qyn1)
kn <-table(KNN=prfpred, sample=trainyn)
kn
sum(diag(kn))/sum(kn)

sample_knnT  <-  knn(traindata[,-1], testdata[,-1], traindata$pnb)
prfpred<-c(as.integer(sample_knnT),rep(1,2,length(qyn2)))
testyn<-c(testdata$pnb,qyn2)
kn <-table(KNN=prfpred, sample=testyn)
kn
v1 = sum(diag(kn))/sum(kn)
v2 = sum(diag(kn))/60
f=2/(1/v1 + 1/v2)
f

#随机森林
library(randomForest)
set.seed(2013)
traindata.rf <- randomForest(as.factor(pnb) ~ .,data=traindata,importance=TRUE,proximity=TRUE)
print(traindata.rf)
#变量重要性
imp<-round(importance(traindata.rf), 2)
impvar <- imp[order(imp[, 3], decreasing=TRUE),]

prf<-predict(traindata.rf, traindata[,-1])
prfpred<-c(as.integer(prf),rep(1,2,length(qyn1)))
trainyn<-c(traindata$pnb,qyn1)
rp <-table(RF=prfpred, sample=trainyn)
rp
sum(diag(rp))/sum(rp)



prf<-predict(traindata.rf, testdata[,-1])
prfpred<-c(as.integer(prf),rep(1,2,length(qyn2)))
testyn<-c(testdata$pnb,qyn2)
rp <-table(RF=prfpred, sample=testyn)
rp
v1 = sum(diag(rp))/sum(rp)
v2 = sum(diag(rp))/60
f=2/(1/v1 + 1/v2)
f

