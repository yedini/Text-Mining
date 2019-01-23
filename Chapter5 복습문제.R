#다는 못했지만 한만큼이라도 낼게요....ㅎㅎㅎ......




#1.
job <- read.csv("jobkorea.csv")
str(job)

#2.
job <- job[,3:5]

#3.
job <- paste(job$총평, job$장점, job$단점, sep=" ")

#4.
library(KoNLP)
useSejongDic()

#5.
nouns <- sapply(job, extractNoun, USE.NAMES = F)

#6.
library(tm)
for (i in 1:length(nouns)){
  nouns[[i]] <- gsub("[[:punct:]]", "", nouns[[i]])
  nouns[[i]] <- gsub("[a-zA-Z0-9]", "", nouns[[i]]) 
  nouns[[i]] <- subset(nouns[[i]], nchar(nouns[[i]])>=2)
  nouns[[i]] <- paste(nouns[[i]][1:length(nouns[[i]])], collapse=" ")
}

#7.
unlist_nouns <- unlist(nouns)

#8.
job_corpus <- VCorpus(VectorSource(unlist_nouns))

#9.
clean.corpus <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords,
                   c("회사", "직장"))
}
job_corpus <- clean.corpus(job_corpus)

#10.
job_dtm <- DocumentTermMatrix(job_corpus,
                              control=list(weighting=weightTf))
rowTotals <- apply(job_dtm, 1, sum)
job_dtm <- job_dtm[rowTotals>0,]
job_dtm_s <- scale(job_dtm, scale=TRUE)

#11.
set.seed(1234)
library(skmeans)
library(cluster)
library(clue)
library(fpc)
set.seed(1234)
job_clusters <- kmeans(job_dtm_s, 3)

#12.
barplot(job_clusters$size)
plotcluster(cmdscale(dist(job_dtm)), job_clusters$cluster)
plot(silhouette(job_clusters$cluster, dist(job_dtm_s)))
#2,3cluster에 속한 데이터가 없다시피 하고 average silhouette width가 0.44로
#현재 cluster에서 다른 cluster로 이동할 수 있는 여지가 어느정도 있음을 알 수 있다.
#따라서 완전히 잘 분류되었다고 볼 수 없다.

#13.
job_skmeans<-skmeans(job_dtm, 3, m=1.2, control=list(nruns=5, verbose=TRUE))
barplot(table(job_skmeans$cluster))
plotcluster(cmdscale(dist(job_dtm)), job_skmeans$cluster)
plot(silhouette(job_skmeans))
#Average silhouette width가 0.07로 k-means clustering을 했을 때 보다 안좋은 분류결과라고 볼 수 있다.

#14.
proto <- t(cl_prototypes(job_clusters))
library(wordcloud)
comparison.cloud(proto, max.words = 100)

#15.
library(ldatuning)
result <- FindTopicsNumber(job_dtm, 
                           topics=seq(10,30,2),
                           metrics=c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                           method="Gibbs", control=list(seed=77),
                           mc.cores = 8, verbose=TRUE)
FindTopicsNumber_plot(result)
#토픽 개수를 15개로 정함

#16.
library(lda)
corpus <- lexicalize(job)

#17.
wc <- word.counts(corpus$documents, corpus$vocab)
doc.length <- document.lengths(corpus$documents)

#18.
corpus$vocab <- gsub("[c(),\\]", "", corpus$vocab)

#19.
result <- lda.collapsed.gibbs.sampler(documents=corpus$documents,
                                      K=15, vocab=corpus$vocab,
                                      num.iterations = 1000,
                                      alpha=0.1, eta=0.1, initial=NULL,
                                      burnin=0, compute.log.likelihood = T)

#20.
top.topic.words(result$topics, 500, by.score = TRUE)

#21.
library(pbapply)
phi <- t(pbapply(t(result$topics)+0.1, 2, function(x) x/sum(x)))
theta <- t(pbapply(result$document_sums+0.1, 2, function(x) x/sum(x)))

#22.
library(LDAvis)
#article.json <- createJSON(phi=phi, theta=theta, doc.length = doc.length,
#                           vocab=corpus$vocab, term.frequency = as.vector(wc))  ->에러뜸 힝굴
#.....어디서부터 잘못됐는지..............노답....포기각....

#23.
doc.assignment <- function(x) {
  x <- table(x)
  x <- as.matrix(x)
  x <- t(x)
  x <- max.col(x)
}
#19번에서 result에 문제가 있는것같은데(책 예제 풀때도 같은 에러났었어요ㅠ)
#뭘 고쳐야되는지..모르겠어요오.... 한만큼이라도 내겠슴니다...
