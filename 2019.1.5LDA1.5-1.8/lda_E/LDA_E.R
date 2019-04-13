library(tm)
library(SnowballC)
library(slam)
library(topicmodels)
library(lda)
library(LDAvis)
library(NLP)
library(magrittr)  #管道符
#简练代码
abstract<-readLines("c:/users/lenovo/desktop/lda_E/Endnote.txt")

#============================#
#=====第一步：分词与清洗=====#
#============================#
firstc<-Corpus(VectorSource(abstract))

#数据清洗
firstc<-tm_map(firstc,PlainTextDocument)
firstc<-tm_map(firstc,tolower)
firstc<-tm_map(firstc,removePunctuation)
firstc<-tm_map(firstc,removeNumbers)
#firstc<-tm_map(firstc,removeWords,
#               stopwords("SMART"))
firstc<-tm_map(firstc,stripWhitespace)

class(firstc)

summary(firstc)
#summary并不能查看具体信息，要用到inspect函数
inspect(firstc[1:3])

#生成TF-IDF特征  约束条件
control<-list(removePunctuation=T,
              minDocFreq=1,
              #bounds = list(global = c(5,Inf)), # 设置词的最小频率
              removeNumbers = TRUE,   #removeNumbers设置是否移除数字
              wordLengths = c(1,Inf),# 限制词长
        #词频率权重，以TF（词频），还可以设置weighTfIdf，weightBin,weightSMART
              weighting=weightTfIdf
              )

#构建文档-词条矩阵
dtm <- DocumentTermMatrix(firstc)
# inspect(dtm[1:10, 1:3])
# class(dtm)






#=============================#
#=====第二步：LDA格式准备=====#
#=============================#

get.terms <- function(x) {
  index <- match(x, vocab) 
  index <- index[!is.na(index)] 
  rbind(as.integer(index - 1), as.integer(rep(1, length(index)))) 
}

documents <- lapply(doc.list, get.terms)


#=========================#
#=====第三步：LDA建模=====#
#=========================#
x<-dtm
k=3

fit <-LDA(x, k, method = "VEM", control = NULL, model = NULL)


K <- 5 #主题数
G <- 5000 #迭代次数
alpha <- 0.10 
eta <- 0.02

set.seed(357) 

fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, 
                                   vocab = vocab, num.iterations = G, 
                                   alpha = alpha, eta = eta, initial = NULL, 
                                   burnin = 0, compute.log.likelihood = TRUE)

summary(fit)


#==============================#
#=====第四步：LDAvis可视化=====#
#==============================#

#1.参数准备
#文档—主题分布矩阵
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x))) 
#主题-词语分布矩阵
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x))) 
#词频
term.frequency <- as.integer(term.table)
#每篇文章的长度，即有多少个词
doc.length <- sapply(documents, function(x) sum(x[2, ])) 

#2.调用LDAvis包
json <- createJSON(phi = phi, theta = theta, 
                   doc.length = doc.length, vocab = vocab,
                   term.frequency = term.frequency)
serVis(json, out.dir = 'c:/users/lenovo/desktop/lda_E/LDA/vis', open.browser = FALSE)

#3.中文格式乱码问题
writeLines(iconv(readLines("c:/users/lenovo/desktop/lda_E/LDA/vis/lda.json"), 
                 from = "GBK", to = "UTF8"), 
           file("c:/users/lenovo/desktop/lda_E/LDA/vis/lda.json", encoding="UTF-8"))

#



#=================#
#===代码调试ing===#
#=================#
textfile<-"c:/users/lenevo/desktop/Endnote.txt"
first<-readLines(textfile)
#first.vec<-VectorSource(first)
#每条摘要向量化处理
first.vec<-VectorSource(abstract)
head(first.vec)
#构建语料库
firstc<-Corpus(first.vec)
firstc1<-VCorpus(VectorSource(abstract))
#summary并不能查看具体信息，要用到inspect函数
inspect(firstc[1:2])
#查看某个文档元数据
meta(firstc[[1]])
#查看单个文档内容
as.character(firstc[[1]])
summary(firstc)
head(firstc)


#tm_map(firstc,stemDocument)这行原始代码没用
#数据清洗
#转换为小写
firstc<-tm_map(firstc,tolower)
#去除标点
firstc<-tm_map(firstc,removePunctuation)
#去除数字
firstc<-tm_map(firstc,removeNumbers)
#去停词   在线检索
firstc<-tm_map(firstc,removeWords,stopwords("english"))
#消除空格
firstc<-tm_map(firstc,stripWhitespace)
#纯文本处理
firstc<-tm_map(firstc,PlainTextDocument)

#再过滤一次
firstb <- Corpus(VectorSource(firstc))






#构建文档词条矩阵
dtm<-DocumentTermMatrix(firstb)
dtm<-DocumentTermMatrix(firstb,
                        control = list
                        (bounds = list(global = c(3, Inf)),steming=TRUE,
                          stopwords=TRUE,minWordLength=3,encoding="UTF-8",
                          removeNumber=TRUE,removePunctuation=TRUE))    

library(sampling)      
sub<-sample(1:nrow(dtm),round(nrow(dtm)*9/10)) 
length(sub) 
dtrain<-dtm[sub,]     
dtest<-dtm[-sub,]

TPnum = seq(130, 200, by = 10) 
per <- NULL
for (m in TPnum){
  Gibbs <- LDA(dtrain,k=m, method = "Gibbs",
               control = list(seed = 1206, burnin = 100,
                              thin = 100, iter = 1000))
  per <- c(per,perplexity(Gibbs, newdata=dtest))
}
print(per)
