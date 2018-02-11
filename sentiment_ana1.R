#Installing/Calling Libraries
library(readr)
library('stringr')
library('readr')
library('wordcloud')
library('tm')
library('SnowballC')
library('RWeka')
library('RSentiment')
library(DT)

#tweet_data = read.csv("newtwitter.small.csv")
tweet_data = read_csv("E:/R/sentiment analysis/newtwitter.small.csv")
tweet_data

#####Using tm package: Method-1#########

#Conducting analysis using "tm" package

#Extracting relevent data
text.in.tweet = as.character(tweet_data$`#NewTwitter kya apa to emg'e?? ga isa nyoba new twitter dr laptop~ =(`)
text.in.tweet

#Data Preprocessing
set.seed(100)
sample = sample(text.in.tweet,length(text.in.tweet))

#Creating Corpus i.e. bag of words

corpus = Corpus(VectorSource(list(sample)))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords('english'))
corpus = tm_map(corpus, stemDocument)
dtm_up = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))
freq_twt <- colSums(as.matrix(dtm_up))


#Sentiment Calculation
sentiments_twt = calculate_sentiment(names(freq_twt))
sentiments_twt = cbind(sentiments_twt, as.data.frame(freq_twt))
sent_post = sentiments_twt[sentiments_twt$sentiment == 'Positive',]
sent_neg = sentiments_twt[sentiments_twt$sentiment == 'Negative',]
ratio_pos_to_neg.sent = sum(sent_post$freq_twt)/sum(sent_neg$freq_twt)
#See positive words
DT::datatable(sent_post)
#printing word cloud
library(wordcloud)
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
set.seed(100)
wordcloud(sent_post$text,sent_post$freq,min.freq=1,colors=brewer.pal(6,"Dark2"))



##########Using syuzhet Package : Method-2##########
library(syuzhet)
