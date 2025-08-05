
tit=read.csv("C:/Users/hp/Desktop/titanic_dataset.csv")
View(tit)

c3=aov(Survived~(Pclass),data = tit)
summary(c3)
c7=chisq.test(tit$Survived,tit$SibSp)
c7
c8=chisq.test(tit$Survived,tit$Parch)
c8
c10=aov(Survived~(Fare),data = tit)
summary(c10)
titanic=tit[-c(1,4,9,11)]
View(titanic)
c5=chisq.test(titanic$Survived,titanic$Sex)
c5
titanic$Age[is.na(titanic$Age)]=mean(titanic$Age,na.rm=TRUE)
mean(titanic$Age)
View(titanic)
c6=aov(Survived~(Age),data=titanic)
summary(c6)

table(tit$Embarked)
titanic$Embarked[is.na(titanic$Embarked)]="S"
c12=chisq.test(titanic$Survived,titanic$Embarked)
c12
sam=sample.split(titanic$Survived,SplitRatio=0.80)
trset=subset(titanic,sam==TRUE)
tsset=subset(titanic,sam==FALSE)
mdl=randomForest(Survived~.,data=trset,na.action=na.exclude)
tsset$pred=predict(mdl,tsset)
View(tsset)
tsset$binary=ifelse(tsset$pred>0.50,1,0)
table(tsset$Survived,tsset$binary)
