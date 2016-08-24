## Code for capstone project:  PREDICTING SCHOOL STUDENT ALCOHOL CONSUMPTION AND OVERALL PERFORMANCE

#installing packages

wants <- c("mlogit", "nnet", "VGAM","nnet","rpart.plot","caret","lift","nnet","ggplot2","reshape2","caTools","mlbench")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

#loading data
setwd("C:/Users/111/Desktop/Alcohol-master")
d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)
#there are severalstudents that belong to both datasets . 
#These students can be identified by searching for identical attributes
#that characterize each student.

df=rbind(d1, d2) 
str(df)

#creating the list of the attributes to search by
name=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

#creating the uniqe index using mgcv, and getting the final data set d3
library(mgcv)
byname=subset(df,select=name)
unique=uniquecombs(byname)
uniqueIndex<-attr(unique,"index")
d3=df[uniqueIndex,]

#visualizing and exploratory analysing
table(d2$Dalc) #weekday alcohol consumption 1-5 score
table(d2$Walc) #weekend alcohol consumption 1-5 score

#storing my themes
library(ggplot2)
mytheme2=theme(panel.grid.major = element_line(colour = "white")) + theme(panel.border = 
                                                                           element_rect(linetype = "solid", colour = "white"))
mytheme1=theme_bw(base_size = 12, base_family = "")

ggplot (aes(x = Dalc,fill=sex),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(sex~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Dalc,fill=age),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(age~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Dalc,fill=Pstatus),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(Pstatus~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Dalc,fill=Fjob),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(Fjob~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Dalc,fill=schoolsup),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(schoolsup~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Dalc,fill=activities),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(activities~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Dalc,fill=goout),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(goout~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = G3,y=Dalc),data = d3) + geom_point()+geom_jitter(alpha = 0.2)


plot(density(d3$Dalc))  
plot(density(d3$health))  
plot(density(d3$absences))  
plot(density(d3$G1))  
plot(density(d3$G2))  
plot(density(d3$G3))  

#linear regression model
linear<-lm(Dalc ~ ., d3)
summary(linear)
predict.lm<-predict(linear,d3)
summary(predict.lm)
plot(linear)
#visualisation the most significant relationships
ggplot(d3, aes(x = sex, y = Dalc,y ~ x)) +
  geom_jitter(alpha = 0.2)+
  geom_point()+geom_smooth(method = "lm")
ggplot(d3, aes(x = age, y = Dalc,y ~ x)) +
  geom_jitter(alpha = 0.2)+
  geom_point()+geom_smooth(method = "lm")
ggplot(d3, aes(x = Pstatus, y = Dalc,y ~ x)) +
  geom_jitter(alpha = 0.2)+
  geom_point()+geom_smooth(method = "lm")
ggplot(d3, aes(x = Fjob, y = Dalc,y ~ x)) +
  geom_jitter(alpha = 0.2)+
  geom_point()+geom_smooth(method = "lm")
ggplot(d3, aes(x = schoolsup, y = Dalc,y ~ x)) +
  geom_jitter(alpha = 0.2)+
  geom_point()+geom_smooth(method = "lm")
ggplot(d3, aes(x = activities, y = Dalc,y ~ x)) +
  geom_jitter(alpha = 0.2)+
  geom_point()+geom_smooth(method = "lm")
ggplot(d3, aes(x = goout, y = Dalc,y ~ x)) +
  geom_jitter(alpha = 0.2)+
  geom_point()+geom_smooth(method = "lm")

#Splitting into training and testing sets.
library(caTools) 
set.seed(76)
sample.d3 = sample.split(d3$Dalc, SplitRatio=0.7,group = NULL )
trainIdx = which(sample.d3 == TRUE)
trainData = d3[trainIdx,]
testIdx = which(sample.d3 == FALSE)
testData = d3[testIdx,]

#Display of distributed data
dim(trainData)
dim(testData)

#Logistic regression
set.seed(123)
#using the train function on the training set logistic regression
train.glm<- glm(Dalc~ ., data=trainData)
summary(train.glm)


#predicting 
predicted.glm=predict(train.glm,testData)
head(predicted.glm)
str(predicted.glm)



#Confusion Matrix  
confusionMatrix=table(predicted.glm, testData$Dalc)
str(confusionMatrix)
















