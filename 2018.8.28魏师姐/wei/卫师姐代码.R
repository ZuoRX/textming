rm(list=ls()) 
library(NLP)
library(tm)
library(tokenizers)
library(rJava)
library(Rwordseg)#中文分词
# 中文分词比较有名的包非Rwordseg和jieba莫属，他们采用的算法大同小异，
# 但有一个地方有所差别：Rwordseg在分词之前会去掉文本中所有的符号，
# 这样就会造成原本分开的句子前后相连，本来是分开的两个字也许连在一起就是一个词了。 
# 而jieba分词包不会去掉任何符号，而且返回的结果里面也会有符号。
library(SnowballC)
library(MASS)
library(RColorBrewer)
library(wordcloud)#画词云图
library(wordcloud2)
#能更有效的利用词与词的间隔来插入数据，可以根据图片或者文字来绘制定制化的词云
library(pcaPP)
library(Rcpp)
library(cluster)
library(mvtnorm)
library(locfit)
library(ash)
library(KernSmooth)
library(misc3d)
library(rgl)
library(ks)
library(ggplot2)
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
library(rainbow)
library(hdrcde)
library(ggmap)

#导入训练集的数据
train <- read.csv("C:\\Users\\lenovo\\Desktop\\wei/lizi.csv")
test<-read.csv("C:\\Users\\lenovo\\Desktop\\wei/test.csv")
csv <- train

#corpus语料库
#parsing解析
#term-document matrix 文档矩阵

listDict()  
#词典分词可能会产生冲突，就卸载有冲突的词典
#uninstallDict(removedict = listDict()[[2]],remove=T)

#安装词典： 
installDict("C:\\Users\\lenovo\\Desktop\\wei\\yansougou/law.scel", "law")
installDict("C:\\Users\\lenovo\\Desktop\\wei\\yansougou/medicine.scel", "med")
installDict("C:\\Users\\lenovo\\Desktop\\wei\\yansougou/lawbool.scel", "lawbook")


installDict(dictpath = "C:\\Users\\lenovo\\Desktop\\wei\\yansougou/dep.txt",
            dictname = "dep",dicttype = "text",load = "TRUE")
# #### ：词典新增词汇。为了分词的准确性，有时需要自定义词典 ####
# 新增词汇前：
# segmentCN("生产管理系统测试应用服务器")
insertWords(c("民事","行政","律师事务所","上诉人","判决")) 

#人名识别开启
segment.options(isNameRecognition = TRUE) #分离人名
getOption("isNameRecognition")
#查看人名识别功能的状态，结果为TRUE表明能够识别；


segword<-function(csv){
  pp<-as.character(csv$text)
  rf<-segmentCN(pp,nature=TRUE)#nature:是否分离词性，比如动词、名词等
  rf
}


nvword<-function(wf){
  wf.n=wf
  for(i in 1:length(wf)){
    wf.n[[i]]<-wf[[i]][which(names(wf[[i]])=='n'|names(wf[[i]])==
                            'v'|names(wf[[i]])=='lawbook'|names(wf[[i]])==
                            'nz'|names(wf[[i]])=='med'|names(wf[[i]])=='law')]
    wf.n[[i]]<-wf.n[[i]][which(is.na(match(wf.n[[i]],c("是","有","可以"))))]
  }#nz:其他专业名词 which(is.na()):返回缺失值位置
  ovid<-Corpus(VectorSource(wf.n))#建立动态语料库   向量结构
  ovid#“数据库”
}

modelword<-function(ovid,csv){
  # 为了后续建模的需要，一般需要对语料库创立词条-文档关系矩阵，
  # 创建词条-文档关系矩阵所用到的函数为： 
  # TermDocumentMatrix(x, control = list()) 
  # DocumentTermMatrix(x, control = list()) 
  # 它们创建的矩阵互为转置矩阵。
  dtm<-DocumentTermMatrix(ovid,
                          control = list(wordLengths = c(2,Inf)))
  #c为concatenate的缩写，是把一系列参数转换成向量的
  rtdtm<-removeSparseTerms(dtm, 0.9)#可以选择去除权重较小的项
  rtdtm<-rtdtm[row_sums(rtdtm)>0,]#去除空行 
  #R使用rowSums函数对行求和，使用colSums函数对列求和
  tfidf<-weightBin(rtdtm)  #Binary weight a term-document matrix.权重矩阵
  #tfidf<-weightTfIdf(rtdtm1)
  dtmtx<-as.matrix(tfidf)
  doc<-as.integer(dimnames(rtdtm[row_sums(rtdtm)>0,])$Docs)
  #检索或设置对象的名称。 #dimnames(x):重新设置对象的名称
  nb<-as.matrix(csv$type[doc])
  modeldata<-data.frame(nb,dtmtx)
  rtn<-list(modeldata,doc)
  rtn
}


wf1<-segword(csv)          #分词
ovid1<-nvword(wf1)         #构建动态语料库
ovid1[[1]]
rtn1<-modelword(ovid1,csv) #构建词条-文本矩阵
modeldata1<-rtn1[[1]]
#每个分词作为一个变量，文档中出现记为1，没出现记为0
doc1<-rtn1[[2]]  #记载对应的文档编号

#特征选择
#方法1：lasso
# 全称Least absolute shrinkage and selection operator。该方法是一种压缩估计。
# 它通过构造一个惩罚函数得到一个较为精炼的模型，使得它压缩一些系数，
# 同时设定一些系数为零。
# 因此保留了子集收缩的优点，是一种处理具有复共线性数据的有偏估计。
#输出建议选择变量个数

# dd<-as.matrix(modeldata1[,-1])
# cvfit<-cv.glmnet(dd,modeldata1$nb,family="multinomial",type.measure="mae") 
# plot(cvfit)
# 
# res<-list()
# vf<-as.matrix(coef(cvfit)$'0')
# res[[1]]<-names(vf[vf!=0,])[-1]
# vf<-as.matrix(coef(cvfit)$'1')
# res[[2]]<-names(vf[vf!=0,])[-1]
# modelky1<-unique(unlist(res))
# 
# #方法2：Boruta（随机森林，运行慢）
# library(Boruta)
# na.omit(modeldata1)->modeldata
# Boruta(as.factor(nb)~.,data=modeldata,doTrace=2)->Boruta.res
# modelky2<-getSelectedAttributes(Boruta.res)
# plot(Boruta.res,sort=FALSE)
# modelly2

# modeldata1
# nb law lawbook med nz 案件 案件受理费 办法 
# 办公室 包 报告 被告 本案争议 变更 表示 病历 病例 并非 驳回 补偿 不服

#方法3：DF
kk <-list()
#subset返回满足条件的向量、矩阵或数据帧的子集。
ddb1<-subset(modeldata1,modeldata1$nb=="0")#选取modeldata1中元素
dfn1<-apply(ddb1[,-1],2,sum)#去掉第一列，然后对每一列求和
kk[[1]] <- names(dfn1[which(dfn1>3)])
#原来是10  收集频数大于3的名称

ddb2<-subset(modeldata1,modeldata1$nb=="1")#按“nb"区分的原因？
dfn2<-apply(ddb2[,-1],2,sum)
kk[[2]] <- names(dfn2[which(dfn2>30)])

modelky3 <-unique(unlist(kk))#返回唯一的元素
modelky3

#方法4:卡方 用于检验类别变量之间的独立性或确定关联性。
chisq<-function(x){
  mx<-table(modeldata1$nb,x)
  ct<-chisq.test(mx)
  ct$statistic
}

mdc<-apply(modeldata1[,-1],2,chisq)
summary(mdc)
modelky4<-names(mdc[which(mdc>3)])
modelky4

# mdc<-apply(modeldata1[modelky3],2,chisq)
# summary(mdc)
# modelky4<-names(mdc[which(mdc>2)])


#6.建模数据准备

prepword<-function(modelky,ovid,unf){
  dic<-c(as.character(modelky))
  pdtm<-DocumentTermMatrix(ovid,list(dictionary=dic,wordLengths = c(2,Inf)))
  pdtm<-pdtm[row_sums(pdtm)>0,]
  pbin<-weightBin(pdtm)        #求权重，频率
  pbintx<-as.matrix(pbin)
  pdoc<-as.integer(dimnames(pdtm[row_sums(pdtm)>0,])$Docs)
  pnb<-as.matrix(csv$type[pdoc])  #训练集
  pmodeldata<-data.frame(pnb,pbintx)
  prtn<-list(pmodeldata,pdoc)
}

#先看结果，然后分步运算，看

prepword1<-function(modelky,ovid,unf){
  dic<-c(as.character(modelky))
  pdtm<-DocumentTermMatrix(ovid,list(dictionary=dic,wordLengths = c(2,Inf)))
  pdtm<-pdtm[row_sums(pdtm)>0,]
  pbin<-weightBin(pdtm)
  pbintx<-as.matrix(pbin)
  pdoc<-as.integer(dimnames(pdtm[row_sums(pdtm)>0,])$Docs)
  pnb<-as.matrix(test$type[pdoc])  #测试集
  pmodeldata<-data.frame(pnb,pbintx)
  prtn<-list(pmodeldata,pdoc)
}

#组合特征提取DF-CHI
#训练集
wf1<-segword(train)
ovid1<-nvword(wf1)
#modelky3是DF后的结果，ovid1是分词后构建的动态语料库
prtn1<-prepword(modelky3,ovid1,train)  
traindata<-prtn1[[1]]
pdoc1<-prtn1[[2]]
zz<-c(1:length(train[,1]))
qyn1<-train[which(is.na(match(zz,pdoc1))),]$type
#测试集
test <- read.csv("C:/Users/apple/Desktop/test 0-1 60.csv")
wf2<-segword(test)
ovid2<-nvword(wf2)
prtn2<-prepword1(modelky3,ovid2,test)
testdata<-prtn2[[1]]
pdoc2<-prtn2[[2]]
tt<-c(1:length(test[,1]))
qyn2<-test[which(is.na(match(tt,pdoc2))),]$type
# -----------------------------------------------------------------
#   #训练集
# wf1<-segword(train)
# ovid1<-nvword(wf1)
# prtn1<-prepword(modelky4,ovid1,train)  
# traindata<-prtn1[[1]]
# pdoc1<-prtn1[[2]]
# zz<-c(1:length(train[,1]))
# qyn1<-train[which(is.na(match(zz,pdoc1))),]$type
# 
# test <- read.csv("C:/Users/apple/Desktop/test 0-1 60.csv")
# wf2<-segword(test)
# ovid2<-nvword(wf2)
# prtn2<-prepword1(modelky4,ovid2,test)
# testdata<-prtn2[[1]]
# pdoc2<-prtn2[[2]]
# tt<-c(1:length(test[,1]))
# qyn2<-test[which(is.na(match(tt,pdoc2))),]$type
-----------------------------------------------------------------------
  
#SVM
library(kernlab)
sample_ksvm  <-  ksvm(pnb~., data=traindata,type = "C-svc", kernel = "rbfdot",prob.model = TRUE)
#type 意味着是选择分类、回归还是检测
#rbfdot是核函数的一种高斯函数
svmCl  <-  predict(sample_ksvm,traindata[,-1],type = "probabilities")
svmpred<-apply(svmCl,1,function(x) which(x==max(x)))
svmpred<-c(svmpred,rep(1,2,length(qyn1)))
trainyn<-c(traindata$pnb,qyn1)
svmTable <-table(SVM=svmpred, sample=trainyn)
svmTable
svmpred

#训练集合的准确率，覆盖率
v1 = sum(diag(svmTable))/sum(svmTable)#准确率
v2 = sum(diag(svmTable))/100#召回率
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
tTargets = decodeClassLabels(traindata$pnb)#类解码为二进制
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
sum(diag(kn))/sum(kn) #查看分类正确率

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
# library(randomForest)
# set.seed(2013)
# traindata.rf <- randomForest(as.factor(pnb) ~ .,data=traindata,importance=TRUE,proximity=TRUE)
# print(traindata.rf)
# #变量重要性
# imp<-round(importance(traindata.rf), 2)
# impvar <- imp[order(imp[, 3], decreasing=TRUE),]
# 
# prf<-predict(traindata.rf, traindata[,-1])
# prfpred<-c(as.integer(prf),rep(1,2,length(qyn1)))
# trainyn<-c(traindata$pnb,qyn1)
# rp <-table(RF=prfpred, sample=trainyn)
# rp
# sum(diag(rp))/sum(rp)
# 
# 
# 
# prf<-predict(traindata.rf, testdata[,-1])
# prfpred<-c(as.integer(prf),rep(1,2,length(qyn2)))
# testyn<-c(testdata$pnb,qyn2)
# rp <-table(RF=prfpred, sample=testyn)
# rp
# v1 = sum(diag(rp))/sum(rp)
# v2 = sum(diag(rp))/60
# f=2/(1/v1 + 1/v2)
# f




