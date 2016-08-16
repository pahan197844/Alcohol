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

plot(unique)
uniqueIndex<-attr(unique,"index")
d3=df[df-uniqueIndex,]

uniquecombs(d3)
str(d3)
contr.treatment(2)
contr.treatment(3)
contr.treatment(4)
contr.treatment(5)
contrasts(d3$Mjob) = contr.treatment(5)
contrasts(d3$Fjob) = contr.treatment(5)
contrasts(d3$famsize) = contr.treatment(2)
contrasts(d3$Pstatus) = contr.treatment(2)

contrasts(d3$reason) = contr.treatment(4)
contrasts(d3$guardian) = contr.treatment(3)
contrasts(d3$romantic) = contr.treatment(2)

library(caret)







p1 <- ggplot(d3,aes(log(absences),fill=factor(Dalc),colour=factor(Dalc)))+
  geom_density(alpha = 0.1)
p1
p2 <- ggplot(d3,aes(log(goout),fill=factor(Dalc),colour=factor(Dalc)))+
  geom_density(alpha = 0.1)
p2
p3 <- ggplot(d3,aes(log(health),fill=factor(Dalc),colour=factor(Dalc)))+
  geom_density(alpha = 0.1)
p3

