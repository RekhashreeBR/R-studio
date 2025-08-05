
tit=read.csv("C:/Users/hp/Desktop/titanic_dataset.csv")


titanic=tit[-c(1,4,9,11)]

titanic$Age[is.na(titanic$Age)]=mean(titanic$Age,na.rm=TRUE)
mean(titanic$Age)

c6=aov(Survived~(Age),data=titanic)
summary(c6)
chisq.test(titanic$Survived,titanic$Sex)
table(tit$Embarked)
titanic$Embarked[is.na(titanic$Embarked)]="S"

sam=sample.split(titanic$Survived,SplitRatio=0.80)
trset=subset(titanic,sam==TRUE)
tsset=subset(titanic,sam==FALSE)
mdl=glm(Survived~.,data=trset)
tsset$pred=predict(mdl,tsset)
tsset$binary=ifelse(tsset$pred>0.50,1,0)
table(tsset$Survived,tsset$binary)

