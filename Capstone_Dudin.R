## Code for capstone project:  PREDICTING SCHOOL STUDENT DAYLY ALCOHOL CONSUMPTION AND OVERALL PERFORMANCE

#installing packages

wants <- c("mlogit","mgcv", "nnet","e1071" ,"VGAM","nnet","rpart.plot",
           "caret","lift","nnet","ggplot2","reshape2","caTools","mlbench")

has   <- wants %in% rownames(installed.packages())

if(any(!has)) install.packages(wants[!has])

#loading data

setwd("C:/Users/111/Desktop/Alcohol-master")

d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

#there are severalstudents that belong to both datasets . 
#These students can be identified by searching for identical attributes
#that characterize each student.

#binding datasets
df=rbind(d1, d2) 

str(df)

#creating the list of the attributes to search by
name=c("school","sex","age","address","famsize","Pstatus",
       "Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

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

mytheme1=theme_bw(base_size = 12, base_family = "")

mytheme2=theme(panel.grid.major = element_line(colour = "white")) +
  theme(panel.border = 
   element_rect(linetype = "solid", colour = "white"))

#plotting dependent variables

plot(density(d3$health))  

plot(density(d3$absences)) 

plot(density(d3$G1))  

plot(density(d3$G2))  

plot(density(d3$G3)) 

#plotting dependent variables

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


 

#building linear regression model
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

#preparing  training and testing sets for the future work

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

train.glm<- glm(Dalc~ ., data=trainData,family= gaussian)

summary(train.glm)

plot(train.glm)

#predicting 

predicted.glm=predict(train.glm,testData,type="response")

head(predicted.glm)

str(predicted.glm)



#Confusion Matrix  for logistic regression

confusionMatrix=table(predicted.glm, testData$Dalc)

summary(confusionMatrix)

#inproving logistic regression


#CVM regression
library(caret)
library(e1071) 



trainModels=list()
grid=expand.grid(cost=seq(1,900,100),gamma=seq(1,51,10))

for(i in 1:nrow(grid)){ 
  trainModels[[i]]=svm(Dalc ~ sex+ age+famsize+Pstatus+ Medu+Fedu + studytime +failures+ schoolsup+ activities+ higher +romantic
                       +famrel + freetime+goout, data = trainData,type= "C", kernel="radial", cost=grid$cost[i],
                       gamma = grid$gamma[i] ,probability=TRUE) 
}

trainModels  #Will take 40 sec ,the best   cost:  801 ,gamma:  51 


train_svmBest<-svm(Walc ~ ., data = trainData,type= "C", kernel="radial", cost=801,
                   gamma = 51,probability=TRUE) 
train_p<-predict(train_svmBest,trainData,probability=TRUE) 

cm<-confusionMatrix(train_p,trainData[,27]) 

cm

train_svmBest_test=svm(Walc ~., data = testData,type= "C", kernel="radial", cost=801,
                       gamma = 51,probability=TRUE) 
train_p_test<-predict(train_svmBest_test,testData,probability=TRUE)

cm_test<-confusionMatrix(train_p_test,testData[,27]) 

cm_test

#CART
library(rpart) 
library(rpart.plot) 
library(e1071)
library(caret)
d3$G3=as.factor(d3$G3)
sample.d31 = sample.split(d3$Dalc, SplitRatio=0.8,group = NULL )
trainIdx1 = which(sample.d31 == TRUE)
trainData1 = d3[trainIdx1,]
testIdx1 = which(sample.d31 == FALSE)
testData1 = d3[testIdx1,]
treeDalc1<-rpart(Dalc~.,data=trainData1,method="poisson")
treeDalc2<-rpart(Dalc~.,data=trainData1,method="class")

treeDalc3<-rpart(Dalc~.,data=trainData1,method="anova")
prp(treeDalc1)
prp(treeDalc2)
prp(treeDalc3)



train.contr=trainControl(method="cv",number=20)
grid=expand.grid(.cp=(0:10)*0.001)

training=train(Dalc~sex+Medu+Mjob+reason+traveltime+paid+higher+freetime,
               data=trainData1,method="rpart", 
               trControl=train.contr,tuneGrid=grid)

best=training$finalModel
prp(best)
best.prediction= predict(best, data =testData1 )
sum(best.prediction - trainData1$Dalc)^2



