library(jiebaR) 
#library(Rwordseg)#中文分词
library(data.table)
library(xlsx)
library(wordcloud2)
library(magrittr)
library(textclean)

# data<-readLines("c:/users/lenovo/desktop/word/classify/China.txt")
# data<-readLines("c:/users/lenovo/desktop/word/classify/France.txt")
# data<-readLines("c:/users/lenovo/desktop/word/classify/England.txt")
# data<-readLines("c:/users/lenovo/desktop/word/classify/Japan.txt")
# data<-readLines("c:/users/lenovo/desktop/word/classify/Mexico.txt")
# 
# data<-read.csv("c:/users/lenovo/desktop/word/classify/America.csv",header = F)
# write.table(data,"c:/users/lenovo/desktop/word/classify/America.txt",row.names = F)
data<-readLines("c:/users/lenovo/desktop/word/classify/America.txt")

head(data)
#英文的话就不需要去特殊分词了，直接以空格为基准拆分文本就好了
data <- data %>% replace_non_ascii() %>% 
  replace_white() %>% replace_contraction()

data1<-paste0(data,collapse = ' ')#把各行粘在一起
data2<-gsub("[^a-zA-Z]"," ",data1)#除去非字母字符
data3<-tolower(data2)  #小写  

engine_s <- worker(stop_word="c:/users/lenovo/desktop/word/stop.txt") 
seg <- segment(data3,engine_s)  ##分词

wordsFreq <- freq(seg)##统计词频

wordsFreq[,1]<-as.character(wordsFreq[,1])#转化为character类型
#wordsFreq1<-wordsFreq[-which(nchar(wordsFreq$freq)==1),]#过滤词频 
wordsFreq1<-wordsFreq[order(wordsFreq[,2],decreasing = T),]#给词频排序
fwrite(wordsFreq1,"c:/users/lenovo/desktop/word/classify/wordFreq/AmericawordFreq.csv")
#词频清洗
head(wordsFreq1,10)

wordsFreq1$freq[1:3]<-300#变小第一大词频

wordsFreq2<-wordsFreq1[1:300,]

#默认云图
wordcloud2(data = wordsFreq2,size=0.5)
#心形
wordcloud2(data = wordsFreq2,size=0.5,shape = 'cardioid')
#钻石
wordcloud2(data = wordsFreq2,size=0.5,shape = 'diamond')
#前三角
wordcloud2(data = wordsFreq2,size=0.5,shape = 'triangle-forward')
#五角形
wordcloud2(data = wordsFreq2,size=0.5,shape = 'pentagon')

#自定义
# path="c:/users/lenovo/desktop/word/Trump.jpg"
# path=system.file("examples/Trump.jpg",package = "wordcloud2")
# 
# letterCloud(wordsFreq3, "T",size=0.5,color = 'random-light')
# 
# wordsFreq3<-wordsFreq1[1:500,]
# wordcloud2(wordsFreq3,size=0.5, color = 'random-light',
#            figPath = path)





















