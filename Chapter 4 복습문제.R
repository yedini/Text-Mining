####Chapter 4 복습과제####


#1.1
library(tidyverse)
amazon <- read_csv("amazon_raw_data.csv")
amazon

#1.2
library(tidytext)
amazon.word <- amazon %>% unnest_tokens(word, sentences) %>% select(-(sentiment)) 
amazon.word

#1.3
data("sentiments")
bing <- sentiments %>% filter(lexicon=="bing") %>% select(-(score))
(bing.word <- inner_join(amazon.word, bing))

#1.4
(bing.word <- bing.word %>% count(sentiment, index=X1))

#1.5
(bing.word <- bing.word %>% spread(sentiment, value=n, fill=0))

#1.6
(result <-bing.word %>% group_by(index) %>%
    summarise(pos.sum=sum(positive),
              neg.sum=sum(negative),
              score=pos.sum-neg.sum)) 

#1.7
(result <- result %>% select(index, score))

#1.8
result$sentiment <- ifelse(result$score>=0, 1, 0)
id <- result$index
amazon_bing <- amazon[id,]
#1.9
sum(amazon_bing$sentiment==result$sentiment)/nrow(amazon_bing)
#예측률이 0.8173191 => 약 82퍼센트!

#1.10
library(gmodels)
CrossTable(result$sentiment, amazon_bing$sentiment, dnn=c('predicted', 'actual'))


#2.1
library(qdap)
library(tm)
pol <- polarity(removePunctuation(removeNumbers(tolower(amazon$sentences))))

#2.2
a.pol <- data.frame(doc=1:nrow(amazon), score=pol$all$polarity)

#2.3
zero <- which(a.pol$score==0)
a.pol <- a.pol[-zero,]

#2.4

####실제 데이터에도 인덱스를 적용해달라는 말이 score가 0인 텍스트를 dataframe에서 제거한것처럼
####실제데이터도 해당 인덱스를 제거하라는 얘기 맞나요...? 일단 그렇게 이해하고 풀었슴다..
amazon2 <- amazon[-zero,]


#2.5
a.pol$pred <- ifelse(a.pol$score>0, 1, 0)

#2.6
sum(a.pol$pred==amazon2$sentiment)/nrow(a.pol)
##0.8983891 => 예측률이 약 89.8퍼센트

#2.7
CrossTable(a.pol$pred, amazon2$sentiment,
           dnn=c('predicted', 'actual'))
