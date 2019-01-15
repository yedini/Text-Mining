options(stringsAsFactors = FALSE) 
Sys.setlocale("LC_ALL", "C")
setwd('C:/Users/dcng/Documents')
library(tidyverse)

#1.1
review <- read_csv("some_review1.csv", col_names=FALSE)

#1.2
review$X2 <- str_replace_all(review$X2, "\\W", " ")

#1.3
colnames(review) <- c("doc_id", "text")

#1.4
library(tm)
corpus <- VCorpus(DataframeSource(review))

#1.5
tryTolower <- function(x) {
  y=NA
  try_error=tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, 'error'))
    y=tolower(x)
  return(y)
}
clean.corpus <- function(corpus) {
  corpus <-tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords('english')))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)
}
corpus <- clean.corpus(corpus)

#1.6
tdm <- TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm.m <- as.matrix(tdm)

#1.7
freq <- rowSums(tdm.m)
freq.df <- data.frame(word=names(freq), freq=freq) %>% arrange(desc(freq))
freq.df$word <- factor(freq.df$word, levels=unique(as.character(freq.df$word)))

#1.8
ggplot(freq.df[1:30,], aes(word, freq))+
  geom_bar(stat="identity", fill="brown")+coord_flip()+
  geom_text(aes(label=freq), colour="white")

#1.9
ass <- findAssocs(tdm, "old", 0.25) %>% as.data.frame()
ass
ass$term <- row.names(ass)
ass$term <- factor(ass$term, levels=ass$term)
ggplot(ass, aes(term, old))+geom_bar(stat="identity", fill="brown")+
  geom_text(aes(label=old), colour="white", vjust=0.9, size=4.5)

#1.10
haven <- review[grep("haven", review$text, ignore.case = TRUE),]

#1.11
h.corpus <- VCorpus(DataframeSource(haven[1:3,]),)
h.corpus <- clean.corpus(h.corpus)
h.tdm <- TermDocumentMatrix(h.corpus, control=list(weighting=weightTf))
h.m <- as.matrix(h.tdm)

library(igraph)
adj <- h.m %*% t(h.m)
adj <-graph.adjacency(adj, weighted=TRUE, mode="undirected", diag=TRUE) 
adj <- simplify(adj)
plot.igraph(adj, vertex.shape="none", vertex.label.font=2, vertex.label.color="darkred",
            vertex.lavel.cex=0.7, edge.color="grey85")
library(qdap)
word_network_plot(haven$text[1:3])


#2
trump <- read_csv("trump.csv")
trump$text <- str_replace_all(trump$text, "\\W", " ")
t.corpus <- VCorpus(DataframeSource(trump))
t.corpus <- clean.corpus(t.corpus)
tdm <- TermDocumentMatrix(t.corpus, control=list(weighting=weightTf))
trump.m <- as.matrix(tdm)
m.freq <- rowSums(trump.m)
freq.df <- data.frame(word=names(m.freq), freq=m.freq)

#2.1
tdm2 <- removeSparseTerms(tdm, sparse=0.8)

#2.2
dist(tdm2, method="euclidean")

#2.3
hc <- hclust(dist(tdm2, method="euclidean"), method="complete")
plot(hc, yaxt="n")
rect.hclust(hc, k=4)

#2.4
hcd <- as.dendrogram(hc)
clusMember<- cutree(hc,4)
labelColors <- brewer.pal(4, "Dark2")
dend.change <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(
      names(clusMember) == a$label
    )]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col=labCol)
  }
  n
}
clusDendro <- dendrapply(hcd, dend.change)

#2.5
plot(clusDendro, type="triangle", yaxt="n")

#2.6
df2 <- freq.df %>% filter(freq>=4)
library(wordcloud)
wordcloud(df2$word, df2$freq, colors=brewer.pal(8, "Purples")[-(1:4)])
