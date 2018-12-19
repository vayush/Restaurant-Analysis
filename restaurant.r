dataset_original = read.delim('Restaurant_Reviews.tsv', quote = '', stringsAsFactors = FALSE)

install.packages('tm')
install.packages('SnowballC')
library(tm)
library(SnowballC)
corpus = VCorpus(VectorSource(dataset_original$Review))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)

dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)
dataset = as.data.frame(as.matrix(dtm))
dataset$Liked = dataset_original$Liked

install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 10)


y_pred = predict(classifier, newdata = test_set[-692])


cm = table(test_set[, 692], y_pred)
cm


library(rpart)
classifier = rpart(formula = Liked ~ .,
                   data = training_set)


y_pred2 = predict(classifier, newdata = test_set[-692], type = 'class')


cm2 = table(test_set[, 692], y_pred2)
cm2
cm


install.packages('e1071')
library(e1071)
classifier = naiveBayes(x = training_set[-692],
                        y = training_set$Liked)


y_pred3 = predict(classifier, newdata = test_set[-692])


cm3 = table(test_set[, 3], y_pred3)


library(class)
y_pred4 = knn(train = training_set[, -692],
              test = test_set[, -692],
              cl = training_set[, 692],
              k = 5,
              prob = TRUE)

library(e1071)
classifier = svm(formula = Liked ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')


y_pred5 = predict(classifier, newdata = test_set[-692])


cm5 = table(test_set[, 692], y_pred5)


cm4 = table(test_set[, 692], y_pred4)
library(caret) 
confusionMatrix(cm) #random forest
confusionMatrix(cm2)#decision tree
confusionMatrix(cm3)#naive bayes
confusionMatrix(cm4)#knn
confusionMatrix(cm5)#svm
