###Chapter 7. Predictive Modeling: Using Text for Classifying and Predicting Outcomes


Sys.setlocale('LC_ALL', 'C')
library(data.table)
library(pbapply)
library(text2vec)
library(caret)
library(glmnet)
library(qdap)
library(pROC)
library(tm)
library(Metrics)
library(tidyverse)
library(ggthemes)


###1번 문제

#1-1
teacher <- fread('https://raw.githubusercontent.com/jhan0317/sys-6018-final-project/master/data/allreviews.csv', encoding='UTF-8')
str(teacher)


#1-2
clean.function <- function(x) {
  x <- replace_contraction(x)
  x <- removePunctuation(x)
  x <- stripWhitespace(x)
  x <- removeNumbers(x)
  x <- tolower(x)
  x <- stemDocument(x)
  return(x)
}


#1-3
comments.clean <- clean.function(teacher$comments)


#1-4
y <- teacher$overallRate

#1-5
train <- createDataPartition(y, p=0.75, list=FALSE)
train.teacher <- comments.clean[train]
train.y <- y[train]
test.teacher <- comments.clean[-train]
test.y <- y[-train]

#1-6
iter.maker <- itoken(train.teacher, tokenizer=word_tokenizer)
v_en <- create_vocabulary(iter.maker, stopwords=c(stopwords('en'), 'teacher', 'class', 'lecture', 'professor'))
head(v_en)
###en: stopwords 178개
v_smart <- create_vocabulary(iter.maker, stopwords = c(stopwords('SMART'), 'teacher', 'class', 'lecture', 'professor'))
head(v_smart)
###smart: stopwords 575개 ===> smart를 활용하여 불용어 제거

#1-7
pruned.v <- prune_vocabulary(v_smart, term_count_min = 10, doc_proportion_max = 0.5,
                             doc_proportion_min = 0.001)

#1-8
vectorizer <- vocab_vectorizer(pruned.v)
it <- itoken(train.teacher, tokenizer=word_tokenizer)
dtm <- create_dtm(it, vectorizer)

#1-9
find.best.folds <- function(dtm=NULL, train.label=NULL, alpha=1, family=NULL,
                            type.measure=NULL, nfolds=NULL, intercept=TRUE, prediction=TRUE) {
  test=ifelse(prediction==TRUE, 'prediction', 'classification')
  test_standard=ifelse(prediction==TRUE, 'rmse', 'auc')
  folds <- nfolds
  cv.model <- list()
  cv.preds <- list()
  cv.results <- vector(length=length(folds))
  for(i in 1:length(folds)){
    cv.model[[i]] <- cv.glmnet(dtm, train.label, alpha=alpha,
                               family=family, type.measure = type.measure, nfolds=folds[i],
                               intercept=intercept)
    if(test=='prediction') {
      cv.preds[[i]] <- predict(cv.model[[i]], dtm, s=cv.model[[i]]$lambda.min)
      cv.results[i] <- mse(train.label, cv.preds[[i]][,1])
    } else {
      cv.preds[[i]] <- as.logical(predict(cv.model[[i]], dtm, type='class',
                                          s=cv.model[[i]]$lambda.1se))
      cv.results[i] <- as.numeric(roc((train.label*1), cv.preds[[i]]*1)$auc)
    }
  }
  names(cv.results) <- nfolds
  return(list(
    method=paste('Test for', test,'and evaluation standard is', test_standard, sep=' '),
    folds.result=cv.results,
    best.value=cv.results[max(cv.results)==cv.results]
  ))
  return(cv.results)
}

find.best.folds(dtm=dtm, train.label=train.y, family='gaussian', type.measure='mse',
                nfolds=c(3:13), intercept=TRUE, prediction=TRUE)

##최적의 fold 갯수는 3개!


#1-10
text.cv <- cv.glmnet(dtm, train.y, alpha=1, family='gaussian', type.measure = 'mse',
                     nfolds=3, intercept=TRUE)
plot(text.cv);title("Teacher Reviews prediction")

#1-11
text.preds <- predict(text.cv, dtm, s=text.cv$lambda.min)
train.df <- data.frame(actual=train.y, preds=text.preds[,1])
rmse(train.df$actual, train.df$preds)
mae(train.df$actual, train.df$preds)

#1-12
train.df %>% ggplot(aes(actual, preds))+geom_point(color='orange', shape=1)+
  geom_smooth(method=lm)+theme_gdocs()


#1-13
test.it <- itoken(test.teacher, preprocess_function=tolower, tokenizer=word_tokenizer)
test.dtm <- create_dtm(test.it, vectorizer)
test.preds <- predict(text.cv, test.dtm, s=text.cv$lambda.min)

#1-14
test.df <- data.frame(actual=test.y, preds=test.preds[,1])
ggplot(test.df, aes(actual, preds))+geom_point(color='orange', shape=1)+
  geom_smooth(method=lm)+theme_gdocs()
##실제 review값이 2점인 경우의 예측 범위가 가장 넓은 것으로 봤을 때 review가 2점인 경우가 가장 예측이 어렵다고 볼 수 있다. 또한 3,4,5점에 비해 1,2점대의 예측 범위가 넓은 것을 봤을 때 평점이 낮은 경우가 높은 경우에 비해 예측하기 어려웠다고 할 수 있다.


###2번문제

#2-1-1
teacher <- teacher %>% mutate(comments.clean=comments.clean)


#2-1-2

teacher <- teacher %>% mutate(overallGood=ifelse(overallRate<=3.5, FALSE, TRUE))


#2-2
train <- createDataPartition(teacher$overallGood, p=0.75, list=FALSE)
train.teachers.cl <- teacher[train,]
test.teachers.cl <- teacher[-train,]


#2-3
iter.maker <- itoken(train.teachers.cl$comments.clean, preprocess_function=tolower, tokenizer=word_tokenizer)
v <- create_vocabulary(iter.maker, stopwords=c(stopwords('en'), 'teacher', 'class', 'lecture', 'professor'))
vectorizer <- vocab_vectorizer(v)
it <- itoken(train.teachers.cl$comments.clean, preprocess_function=tolower, tokenizer = word_tokenizer)
dtm <- create_dtm(it, vectorizer, weighting='TfIdf')


#2-4

find.best.folds(dtm=dtm, train.label=train.teachers.cl$overallGood, family='binomial', type.measure='auc',
                nfolds=c(3:8), intercept=FALSE, prediction=TRUE)
###fold갯수를 7개로 선택!
text.cv <- cv.glmnet(dtm, y=as.factor(train.teachers.cl$overallGood), alpha=1, family="binomial",
                     type.measure = "auc", nfolds=7, intercept=FALSE)
plot(text.cv)


#2-5
no.text <- model.matrix(~.,train.teachers.cl[,-c(1,8,14,20,23,25,27,30,31)])
no.text.cv <- cv.glmnet(no.text, y=as.factor(train.teachers.cl$overallGood), alpha=1, family='binomial',
                        type.measure='auc', nfolds=7, intercept=FALSE)
plot(no.text.cv) ; title("GLMNET No Text")


#2-6
all <- cBind(dtm, no.text)
all.cv <- cv.glmnet(all, y=as.factor(train.teachers.cl$overallGood), alpha=1, family='binomial',
                    type.measure='auc', nfolds=7, intercept=FALSE)
plot(all.cv) ; title("GLMNET All Data")


#2-7
text.preds <- as.logical(predict(text.cv, dtm, type='class', s=text.cv$lambda.1se))
no.text.preds <- as.logical(predict(no.text.cv, no.text, type="class", s=no.text.cv$lambda.1se))
all.preds <- as.logical(predict(all.cv, all, type='class', s=all.cv$lambda.1se))
memory.limit(size=80000)
text.roc <-roc((train.teachers.cl$overallGood*1), text.preds*1)
no.text.roc <- roc((train.teachers.cl$overallGood*1), no.text.preds*1)
all.roc <- roc((train.teachers.cl$overallGood*1), all.preds*1)
plot(text.roc, col="blue", main="Text:Blue  No Text:Red  All:Green", adj=0)
plot(no.text.roc, add=TRUE, col="red", lty=2)
plot(all.roc, add=TRUE, col="green", lty=3)
#텍스트가 없는 모델과 텍스트와 텍스트가 아닌 자료가 모두들어간 모델은 쓸모없다고 볼 수 있음.


#2-8
confusion <- confusionMatrix(factor(text.preds), factor(train.teachers.cl$overallGood))
(recall <- confusion$byClass['Sensitivity'])
(precision <- confusion$byClass['Pos Pred Value'])
(f1.score <- 2*((precision*recall)/(precision+recall)))

#2-9
text.cv.10f <- cv.glmnet(dtm, y=as.factor(train.teachers.cl$overallGood), alpha=1, family="binomial",
                         type.measure = "auc", nfolds=10, intercept=FALSE)
plot(text.cv.10f) ; title("Teacher Reviews Prediction")


#2-10
test.it <- itoken(test.teachers.cl$comments.clean, preprocess_function=tolower, tokenizer=word_tokenizer)
test.dtm <- create_dtm(test.it, vectorizer)
test.preds <- predict(text.cv.10f, test.dtm, type="class", s=text.cv.10f$lambda.1se)
test.confusion <- confusionMatrix(factor(test.preds), factor(test.teachers.cl$overallGood))
(test.recall <- test.confusion$byClass['Sensitivity'])
(test.precision <- test.confusion$byClass['Pos Pred Value'])
(test.f1.score <- 2*((test.precision*test.recall)/(test.precision+test.recall)))
