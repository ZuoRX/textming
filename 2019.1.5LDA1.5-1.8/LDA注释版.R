library(jiebaR)   #分词包
library(topicmodels) 
library(lda)
library(LDAvis)


#==============================#
#=====第一步：数据初步清洗=====#
#==============================#

#导入数据
guangzhou <- read.csv("C:/Users/lenovo/Desktop/LDA/data/guangzhou.csv")

guangzhou_duty<-guangzhou[,c(1,7)]
#删除重复项
guangzhou_duty<-guangzhou_duty[!duplicated(guangzhou_duty),]
names(guangzhou_duty)<-c("job","duties")

#挑选一个职业（营销人员）     待推广到其他职业
salesman<-guangzhou_duty[grep(pattern="营销",guangzhou_duty[,1]),]

write.table(slaesman$duties,"c:/users/lenovo/desktop/LDA/salesman.txt")



#==========================#
#=====第二步：中文分词=====#
#==========================#
#创建分词器，其中bylines是否按行来分，user用户词典，stop_word停用词典
cutter <- worker(bylines = T,encoding='UTF-8',detect=T,
                 user = "c:/users/lenovo/desktop/LDA/userword.txt",
                 stop_word = "c:/users/lenovo/desktop/LDA/停词.txt")

#文件分词，直接输入文件地址，分完后自动保存成文件
comments_seg <- cutter["c:/users/lenovo/desktop/LDA/salesman.txt"] 



#==================================#
#=====第三步：分词的数字化处理=====#
#==================================#

#将文本转化为向量，用数字来代替文本，从而方便运算。
#读取分词结果  
comments_segged<- readLines("c:/users/lenovo/desktop/LDA/salesman.segment.2019-01-07_10_27_32.txt",
                            encoding="UTF-8") #文件名每次都要改
#将向量转化为列表
comments <- as.list(comments_segged) 
#将每行文本，按照空格分开，每行变成一个词向量，储存在列表里
doc.list <- strsplit(as.character(comments),split=" ") 



#====================================#
#=====第四步：对每个分词进行编号=====#
#====================================#

#table把结果变成一个交叉表式的factor
term.table <- table(unlist(doc.list)) 
#按照词频降序排列
term.table <- sort(term.table, decreasing = TRUE) 



#============================#
#=====第五步：分词再清洗=====#
#============================#

# 单字去掉，同时把出现次数少于5次的词去掉。
del <- term.table < 5| nchar(names(term.table))<2 #把不符合要求的筛出来
term.table <- term.table[!del] #去掉不符合要求的
vocab <- names(term.table) #创建词库



#=============================#
#=====第六步：LDA格式准备=====#
#=============================#

get.terms <- function(x) {
  index <- match(x, vocab) # 获取词的ID
  index <- index[!is.na(index)] 
  rbind(as.integer(index - 1), as.integer(rep(1, length(index)))) 
  }

documents <- lapply(doc.list, get.terms)



#=========================#
#=====第七步：LDA建模=====#
#=========================#

#------参数设置-----#
# 其中α，大家可以调大调小了试试看，调大了的结果是每个文档接近同一个topic，
# 其中的β，调大的结果体现在每个topic更集中在几个词汇上面
# 或者而每个词汇都尽可能的百分百概率转移到一个topic上。

K <- 3 #主题数
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
#=====第八步：LDAvis可视化=====#
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

#json为作图需要数据，下面用servis生产html文件，通过out.dir设置保存位置
json <- createJSON(phi = phi, theta = theta, 
                   doc.length = doc.length, vocab = vocab,
                   term.frequency = term.frequency)
serVis(json, out.dir = 'c:/users/lenovo/desktop/LDA/vis', open.browser = FALSE)

#3.中文格式乱码问题
writeLines(iconv(readLines("c:/users/lenovo/desktop/LDA/vis/lda.json"), 
                 from = "GBK", to = "UTF8"), 
           file("c:/users/lenovo/desktop/LDA/vis/lda.json", encoding="UTF-8"))


#


























































