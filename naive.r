data=(iris)
View(data)
head(data)
summary(data)
dim(data)
split=sample.split(iris,SplitRatio = 0.80)
trset=subset(iris,split=="TRUE")
tsset=subset(iris,split=="FALSE")
tescale=scale(trset[1:4])
tsscale=scale(tsset[1:4])
dim(trset)
dim(tsset)
set.seed(1)
library("naivebayes")
classifier_naive=naive_bayes(Species~.,data=trset)
classifier_naive
summary(classifier_naive)
y_pred=predict(classifier_naive, newdata=tsset)
conf_mat=table(tsset$Species,y_pred)
conf_mat






































































































































