d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)
#there are several (382) students that belong to both datasets . 
#These students can be identified by searching for identical attributes
#that characterize each student.

df=rbind(d1, d2) 
str(df)


name=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

library(mgcv)
byname=subset(df,select=name)
unique=uniquecombs(byname)

uniqueIndex<-attr(unique,"index")
d3=df[uniqueIndex,]

library(dplyr)

d3<-mutate(d3,ALc.total = (d3$Walc*2+d3$Dalc*5)/7) 
 summary(d3$ALc.total)


contrasts(d3$Mjob) = contr.treatment(5)
contrasts(d3$Fjob) = contr.treatment(5)
contrasts(d3$famsize) = contr.treatment(2)
contrasts(d3$Pstatus) = contr.treatment(2)
contrasts(d3$reason) = contr.treatment(4)
contrasts(d3$guardian) = contr.treatment(3)
contrasts(d3$romantic) = contr.treatment(2)

library(caret)





library(ggplot2) 

