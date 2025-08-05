getwd()
setwd("E:/")
data=read.csv("ParisHousing.csv")
View(data)

#comparing price with every colums

cor(data$price,data$squareMeters)
#has correlation   (CONSIDERED)

cor(data$price,data$numberOfRooms)
#very less cor   (considered)

yad=aov(price~(hasYard),data=data)
summary(yad)
#p!<0.05         (not considered)

pool=aov(price~(hasPool),data=data)
summary(pool)
#p!<0.05         (not considered)

cor(data$price,data$floors)
#less cor 

cor(data$price,data$cityCode)
#less -ve cor

cpr=aov(price~(cityPartRange),data=data)
summary(cpr)

pro=aov(price~(numPrevOwners),data=data)
summary(pro)

cor(data$price,data$made)

inbt=aov(price~(isNewBuilt),data=data)
summary(inbt)

stp=aov(price~(hasStormProtector),data=data)
summary(stp)

cor(data$price,data$basement)

cor(data$price,data$attic)

cor(data$price,data$garage)

str=aov(price~(hasStorageRoom),data=data)
summary(str)

gtr=aov(price~(hasGuestRoom),data=data)
summary(gtr)

y=x[-c(6,7,9,11,12,13,15)]
View(y)
sample=sample.split(y$price,SplitRatio=0.80)
trainingset=subset(data,sample==TRUE)
testset=subset(data,sample==FALSE)
model=lm(price~.,data=trainingset)
testset$prediction=predict(model,testset)

cor(testset$prediction,testset$price)

