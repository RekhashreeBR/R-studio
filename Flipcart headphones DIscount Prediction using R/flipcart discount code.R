getwd()
setwd("F:/ds projects")

data=read.csv("Flipkart Headphones.csv")
View(data)

#dependent variable is discount
c1=aov(Discount~(Model),data=data)
summary(c1)
#consider
c2=aov(Discount~(Company),data = data)
summary(c2)
#consider
c3=aov(Discount~(Color),data = data)
summary(c3)
#consider
c4=aov(Discount~(Type),data=data)
summary(c4)
#consider
c5=aov(Discount~(Average.Rating),data = data)
summary(c5)
#not considered
c6=cor(data$Discount,data$Number.of.Ratings)
c6
#consider
c7=cor(data$Discount,data$Selling.Price)
c7
#not consider
c8=cor(data$Discount,data$Maximum.Retail.Price)
c8
#not consider

disdata=data[-c(5,7,8)]
split_data=sample.split(disdata$Discount,SplitRatio=0.80)
trset=subset(disdata,split_data)
tsset=subset(disdata,split_data)
mdl=lm(Discount~.,data=trset)
pred=predict(mdl,tsset)
cor(pred,tsset$Discount)
