#导入需要的包
library(readxl)
library(jiebaR)
library(plyr)
library(wordcloud2)


# 读入评论数据
evaluation <-read.csv("C:/Users/lenovo/Desktop/评论.csv",quote = "",sep = "\"", header = T, stringsAsFactors = F)
###也可以这样导入
#evaluation <- read_excel(file.choose())
head(evaluation)
str(evaluation)
##导入正/负面词典以及停用词
pos <- readLines(file.choose())
neg <- readLines(file.choose())
stopwords <- readLines(file.choose())
#看一下正负面词典和停用词
pos
stopwords

# 合并情感词库
mydict <- c(pos, neg)
##看一下合并的
mydict
# 为jieba分词准备工作引擎
engine <- worker()
sentence <- '超韧细密，湿水不易破'
segment(sentence, engine)
# 添加自定义词汇
##举例
new_user_word(engine, '不易破')
segment(sentence, engine)
# 添加入正/负面词典
new_user_word(engine, mydict)
## 对每一条评论进行切词
segwords <- sapply(evaluation$content, segment, engine)
#展示一下结果
segwords
head(segwords)

# 删除停止词（即对分析没有意义的词，如介词、虚词等）
## 自定义函数：用于删除停止词
removewords <- function(target_words,stop_words){
  target_words = target_words[target_words%in%stop_words== FALSE]
  return(target_words)
}
segwords2 <- sapply(segwords, removewords, stopwords)
#展示一下去除停用词后的
head(segwords2)
#自定义情感类型得分函数
fun <- function( x, y) x %in% y
getEmotionalType <- function( x,pwords,nwords){
  pos.weight = sapply(llply( x,fun,pwords),sum)
  neg.weight = sapply(llply( x,fun,nwords),sum)
  total = pos.weight - neg.weight
  return(data.frame( pos.weight, neg.weight, total))
}
# 计算每条评论的正负得分
score <- getEmotionalType(segwords2, pos, neg)
#展示一下
head(score)
#合并一下结果
evalu.score<- cbind(evaluation, score)
#展示
evalu.score
#进一步给结果贴上标签pos/neg
evalu.score <- transform(evalu.score,
emotion = ifelse(total>= 0, 'Pos', 'Neg'))
#展示贴上pos/neg标签的结果
evalu.score
# 随机挑选10条评论，做一个验证
set.seed( 1)
validation <- evalu.score[sample( 1:nrow(evalu.score),size = 10),]
validation
#输出结果到桌面
write.csv(evalu.score,"c:/users/lenovo/desktop/情感分析结果.csv")


# 计算词频(去掉停用词后的)
wf <- unlist(segwords2)
wf <- as.data.frame(table(wf))
wf <- arrange(wf, desc(Freq))
head(wf)
wordcloud2(wf[ 1: 25,], backgroundColor = 'black')
#自定义函数：保留至少2个字符长度的词语
more2words <- function(x){
  words = c()
  for(word in x) {
    if(nchar(word)> 1) words = c(words,word)
  }
  return(words)
}
#重新定义丢下的词
segwords3 <- more2words(unlist(segwords2))
# 计算词频
wf2 <- unlist(segwords3)
wf2 <- as.data.frame(table(wf2))
wf2 <- arrange(wf2, desc(Freq))
head(wf2)
wordcloud2(wf2[ 1: 25,], backgroundColor = 'black')
wordcloud2(wf2[2:51,], backgroundColor = 'black')
