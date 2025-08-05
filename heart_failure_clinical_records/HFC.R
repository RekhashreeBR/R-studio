getwd()
setwd("F:/ds projects")
hfc=read.csv("heart_failure_clinical_records_dataset.csv")
View(hfc)

#independent variable here is DEATH_EVENT which is catogarical variable
c1=aov(DEATH_EVENT~(age),data = hfc)
summary(c1)
#c1 to be considered
c2=chisq.test(hfc$DEATH_EVENT,hfc$anaemia)
c2
#c2 is not considered (coz p-val>0.05)
c3=aov(DEATH_EVENT~(creatinine_phosphokinase),data = hfc)
summary(c3)
#c3 is not considered (coz p>0.05)
c4=chisq.test(hfc$DEATH_EVENT,hfc$diabetes)
c4
#c4 is not considered (coz p-val>0.05)
c5=aov(DEATH_EVENT~(ejection_fraction),data = hfc)
summary(c5)
#c5 to be considered
c6=chisq.test(hfc$DEATH_EVENT,hfc$high_blood_pressure)
c6
#c6 is considered (though p-val>0.05)
c7=aov(DEATH_EVENT~(platelets),data = hfc)
summary(c7)
#c7 is  not consiedered (coz p>0.05)
c8=aov(DEATH_EVENT~(serum_creatinine),data = hfc)
summary(c8)
#c8 to be considered
c9=aov(DEATH_EVENT~(serum_sodium),data = hfc)
summary(c9)
#c9 to be considered
c10=chisq.test(hfc$DEATH_EVENT,hfc$sex)
c10
#c10 is not considered (coz p-val>0.05)
c11=chisq.test(hfc$DEATH_EVENT,hfc$smoking)
c11
#c11 is not considered (coz p-val>0.05)
c12=aov(DEATH_EVENT~(time),data = hfc)
summary(c12)
#c12 is considered

HFC=hfc[-c(2,3,4,7,10,11)]
View(HFC)
sam=sample.split(HFC$DEATH_EVENT,SplitRatio = 0.80)
trset=subset(HFC,sam==TRUE)
tsset=subset(HFC,sam==FALSE)
mdl=glm(DEATH_EVENT~.,data = trset)
summary(mdl)
tsset$prob_of_prediction=predict(mdl,tsset)
tsset$binary=ifelse(tsset$prob_of_prediction>0.50,1,0)
table(tsset$DEATH_EVENT,tsset$binary)
