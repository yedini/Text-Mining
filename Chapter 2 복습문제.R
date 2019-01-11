###Text mining Chapter2 복습과제###

library(tidyverse)

#1.1
lyrics <- read.csv("love_separation.csv")
str(lyrics)

#1.2
separation <- lyrics %>% filter(type=="separation")
love <- lyrics %>%filter(type=="love ")
love.df <- data.frame(doc_id=seq(1:nrow(love)), text=love$lyrics)
separation.df <- data.frame(doc_id=seq(1:nrow(separation)), text=separation$lyrics)

#1.3
library(tm)
tryTolower <- function(x) {
  y=NA
  try_error=tryCatch(tolower(x), error=function(e) e)
  if(!inherits(try_error, 'error'))
    y=tolower(x)
  return(y)
}

#1.4
clean.corpus <- function(corpus) {
  corpus <- tm_map(corpus,
                   content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords('english')))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)
}

#1.5
love_corpus <- VCorpus(DataframeSource(love.df))
love_corpus <- clean.corpus(love_corpus)
separation_corpus <- VCorpus(DataframeSource(separation.df))
separation_corpus <- clean.corpus(separation_corpus)

#1.6
love_tdm <- TermDocumentMatrix(love_corpus, control=list(weighting=weightTf))
separation_tdm <- TermDocumentMatrix(separation_corpus, control=list(weighting=weightTf))

#1.7
love_m <- as.matrix(love_tdm)
separation_m <- as.matrix(separation_tdm)

love_freq <- rowSums(love_m)
love_freq_df <- data.frame(word=names(love_freq),
                           frequency=love_freq) %>%
  arrange(desc(frequency))

separation_freq <- rowSums(separation_m)
separation_df <- data.frame(word=names(separation_freq),
                            frequency=separation_freq) %>%
  arrange(desc(frequency))

love_freq_df[1:10,]
separation_df[1:10,]
#사랑노래에서는 love, want, got, baby, know, like 등이 많이 나왔다.
#사랑노래이다보니 사랑한다, 좋아한다는 표현이 많은 것으로 보이고
#want, got, need가 여러번 나온 것을 통해 사랑하는 사람을 갖고싶어 한다고 해석할 할 수 있다.

#이별노래에서는 back, come, know, heart등의 단어들이 나왔다.
#back, come 등의 단어에서 사랑하는 사람을 떠나보낸 상황이 많음을 알 수 있고,
#이별 노래에서 쓰인 heart는 너가 떠나서 마음이 무너져내린다, 맘이아프다ㅠㅠ의 의미로 볼 수 있다.



#2.
x <- read.csv("x.csv")
x
y <- as.character(x$emails)
z <- data.frame(emails=regmatches(y, regexpr("[a-z_\\-]+@[a-z\\.]+", y)))
z
