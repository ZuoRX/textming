#导入需要的包
library(readxl)
library(jiebaR)
library(plyr)
library(wordcloud2)
library(SentimentAnalysis)
library(textclean)
# require(devtools)
# install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
# 
# require(sentiment)
# ls("package:sentiment")

data<-readLines("c:/users/lenovo/desktop/word/emotion/data1.txt")





##导入正/负面词典以及停用词
pos<-readLines("c:/users/lenovo/desktop/word/emotion/negative-words.txt")
neg<-readLines("c:/users/lenovo/desktop/word/emotion/positive-words.txt")
stopwords<-readLines("c:/users/lenovo/desktop/word/emotion/stop.txt")
# pos <- readLines(file.choose())
# neg <- readLines(file.choose())
# stopwords <- readLines(file.choose())

#http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html


# 文本清洗的必要性是我通过反复试验发现的，比如说I'll 这个词，
# 如果不替换掉缩略词，直接移除标点，就变成ill了。
# 文本清洗可以使用textclean这个包，里面有replace_non_ascii、
# replace_white、replace_contraction等函数，可以实现我们需要的功能：

# 去除非ASCII字符、替换掉缩略词(contraction)等。
data_clean <- data %>% replace_non_ascii() %>% 
  replace_white() %>% replace_contraction()






#==========================================================================#

#------------------#
#------第一节------#
#------------------#

#--------1.1--------#
#文本极性计算，按人分别计算，以及可视化
library(qdap)
library(magrittr)
library(data.table)
# Examine the text data
text_df<-data.table(person=c('Nick','Jonathan','Martijn','Nicole',
                             'Nick','Jonathan','Martijn','Nicole'),
                    text=c('DataCamp courses are the best',
                           'I like talking to students',
                           'Other online data science curricula are boring.',
                           'What is for lunch?',
                           'DataCamp has lots of great content!',
                           'Students are passionate and are excited to learn',
                           'Other data science curriculum is hard to learn and difficult to understand',
                           'I think the food here is good.'
                           )
                    )

# Calc overall polarity score
text_df %$% polarity(text)

# Calc polarity score by person  括号的作用，既保存到变量，又print出结果
(datacamp_conversation <- text_df %$% polarity(text,person))

# Counts table from datacamp_conversation
counts(datacamp_conversation)

# Plot the conversation polarity
plot(datacamp_conversation)


#--------1.2--------#
#语料库的构建与清洗
library(tm)

# the custom function
clean_corpus<-function(corpus){
  #先替换缩写，否则去标点符号容易产生错误
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "coffee"))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

tm_define<-data.table(c("Text mining is the process of distilling actionable insights from text.",
                        "Sentiment analysis represents the set of tools to extract an author's feelings towards a subject.")
                      )

# clean_corpus(), tm_define are pre-defined
clean_corpus
tm_define

# Create a VectorSource
(tm_vector <- VectorSource(tm_define))

# Apply VCorpus
tm_corpus <- VCorpus(tm_vector)

# Examine the first document's contents
content(tm_corpus[[1]])

# Clean the text
tm_clean <- clean_corpus(tm_corpus)
#content(tm_clean[[2]])

# Reexamine the contents of the first doc
content(tm_clean[[1]])


#--------1.3--------#
#文档词频矩阵的应用， 注意矩阵格式处理
# clean_text is pre-defined
clean_text

# Create tf_dtm
tf_dtm <- DocumentTermMatrix(clean_text)

# Create tf_dtm_m
tf_dtm_m <- as.matrix(tf_dtm)

# Dimensions of DTM matrix
dim(tf_dtm_m)

# Subset part of tf_dtm_m for comparison
tf_dtm_m[16:20, 2975:2985]


#--------1.4--------#第二个视频
#美元管道符  js画图
library(lexicon)
library(tidytext)
library(metricsgraphics)

sb_words<-fread("c:/users/lenovo/desktop/word/emotion/sb_words.txt",sep=" ")

# Examine sb_words
head(sb_words)

#write.csv(sb_words,"c:/users/lenovo/desktop/sb_words.csv")
#str(sb_words)

# Create expectations
sb_words$expectations <- sb_words %$% 
  {freq[1] / freq}

# Create metrics plot
sb_plot <- mjs_plot(sb_words, x = rank, y = freq, show_rollover_text = F)

# Add 1st line
sb_plot <- mjs_line(sb_plot)

# Add 2nd line
sb_plot <- mjs_add_line(sb_plot, expectations)

# Add legend
sb_plot <- mjs_add_legend(sb_plot, legend = c("Frequency", "Expectation"))

# Display plot
sb_plot











































































































































































































