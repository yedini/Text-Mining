####Chapter 8 복습문제####
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL', "C")
library(gridExtra)
library(ggmap)
library(ggthemes)
library(NLP)
library(openNLP)
library("openNLPmodels.en", lib.loc="~/R/win-library/3.4") #library 위치는 install 할때 지정했던 주소로 지정함.
library(pbapply)
library(stringr)
library(rvest)
library(doBy)
library(tm)
library(cshapes)

setwd('C:/Users/dcng/Documents/Review_Texts_tripadvisor')


#1
dats <- list.files(pattern='*.dat')
temp <- list()
for (i in 1:round(length(dats)/1000)) {
  raw <- assign(dats[i], readLines(dats[i]))
  txt <- ''
  for (j in 1:length(raw)) {
    txt <- paste(txt, raw[j], sep=" ")
  }
  temp<- append(temp, txt)
} 

#2
txt.clean <- function(x) {
  x <- paste(x, collapse=" ")
  x <- removeWords(x, stopwords('en'))
  x <- as.String(x)
  return(x)
}

#3
all <- pblapply(temp, txt.clean)

#4
person <- Maxent_Entity_Annotator(kind='person')
location <- Maxent_Entity_Annotator(kind='location')
date <- Maxent_Entity_Annotator(kind='date')
money <- Maxent_Entity_Annotator(kind="money")
organization <- Maxent_Entity_Annotator(kind="organization")
percentage <- Maxent_Entity_Annotator(kind="percentage")

#5
sent <- Maxent_Sent_Token_Annotator(language="en")
word <- Maxent_Word_Token_Annotator(language="en")
pos <- Maxent_POS_Tag_Annotator(language="en")

#6
annotations <- annotate(all,
                        list(sent, word, pos, person, location, date, money, organization, percentage))

#7
df <- as.data.frame(annotations)
df$features <- unlist(as.character(df$features))

#8
chars <- NULL
for (i in 1:nrow(df))
  chars[i] <- ((substr(all, df[i, 3], df[i,4])))
df$words <- chars
tail(df)

#9
subset(df$words, grepl("*date", df$features)==TRUE)
subset(df$words, grepl("*location", df$features)==TRUE)
subset(df$words, grepl("*percentage", df$features)==TRUE)
subset(df$words, grepl("*organization", df$features)==TRUE)
subset(df$words, grepl("*money", df$features)==TRUE)
subset(df$words, grepl("*person", df$features)==TRUE)
#person 집합에서 5-6pm, 10-minute등 date집합에 더 알맞을만한 단어들이 잘못 분류되었다고 볼 수 있다.
#다른 집합들과 달리 date집합에는 <Author>로 시작하는 몇개의 단어들이 있는데,
#해당 review 작성자의 아이디가 date로 잘못 분류되었음을 추측해볼 수 있다.
