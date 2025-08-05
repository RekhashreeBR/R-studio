cs=c(23,19,30,22,23,29,35,36,33,25)
ts=c(430,430,333,410,390,377,325,310,328,375)
plot(cs,ts,main="scatter plot of class and test score" ,xlab = "class",ylab = "test score")


avg=mean(ts)
avg
var=var(ts)
var
sd=sd(ts)
sd
cov=cov(ts,cs)
cov
cor=cor(ts,cs)
cor

df=data.frame(cs,ts)
library(AER)
mod=lm(data = df)
mod  
avg_cs=mean(cs)       
var_cs=var(cs)
sd_cs=sd(cs)
quantiles=c(0.25,0.5,0,75)

qunt_ts=quantile(data=ts,quantiles)
qunt_cs=quantile(data=cs,quantiles)
qunt_ts
dist_summary=data.frame(avg=c(avg_cs,avg),var=c(var_cs,var),sd=c(sd_cs,sd),quantiles=rbind(qunt_cs,qunt_ts))
dist_summary


abline(a=350,b=-2)
  
mod_summary=summary(mod)
mod_summary
SSR <- sum(mod_summary$residuals^2) 
TSS <- sum((ts - mean(ts))^2) 
R2 <-(1 - SSR/TSS )
R2

data= data.frame(cs,ts)
mod_in= lm(ts~cs+0,data=data)
mod_in

coef=summary(mod_in)$coefficients
coef

t_stat=coef[1,"estimate"]/coef[1,"std error"]
t_stat

plot(ts~cs,main="scatter plot",xlab="class size",ylab="testscore",col="red")
abline(a=380,b=0.3)

ssr=sum(residual(mod)^2)
ssr
tss=sum(ts-mean(ts)^2)
tss
