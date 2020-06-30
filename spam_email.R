library(tm)
library(caTools)
library(rpart)
library(rpart.plot)
emails = read.csv("emails.csv" , stringsAsFactors = FALSE)
str(emails)
table(emails$spam)
#max no of characters in a mail
max(nchar(emails$text))
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus , tolower)
corpus = tm_map(corpus ,removePunctuation)
corpus = tm_map(corpus ,removeWords , stopwords("english"))
corpus = tm_map(corpus ,stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm
#keep terrms that appear in more than 5% of mails
sptdm = removeSparseTerms(dtm , 0.95))
sptdm
emailsparse= as.data.frame(as.matrix(sptdm))
names(emailsparse) = make.names(names(emailsparse))
emailsparse$spam = as.factor(emails$spam)
set.seed(123)
split =sample.split(emailsparse$spam , SplitRatio=0.7)
train = subset(emailsparse , split == TRUE)
test = subset(emailsparse , split == FALSE)
spamCART = rpart(spam ~. , data = train , method ="class")
set.seed(123)
library(randomForest)
spamRF = randomForest(spam~. ,data = train)
predCART = predict(spamCART)[,2]
predRF = predict(spamRF , type="prob")[,2]
#pred on test set for threshold greater than 0.5
table(train$spam , predCART >0.5)
table(train$spam , predRF>0.5)
prp(spamCART)
library(ROCR)
#compute auc value for cart model on train
predCART = prediction(predCART , train$spam)
as.numeric(performance(predCART ,"auc")@y.values)

predRF = prediction(predRF , train$spam)
as.numeric(performance(predRF ,"auc")@y.values)

predCART = predict(spamCART , newdata = test)[,2]
predRF = predict(spamRF , newdata = test , type="prob")[,2]
table(test$spam , predRF>0.5)

#auc value for rf on test set
predRF = prediction(predRF , test$spam)
as.numeric(performance(predRF ,"auc")@y.values)
