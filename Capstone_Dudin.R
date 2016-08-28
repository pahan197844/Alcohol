## Code for capstone project:  PREDICTING SCHOOL STUDENT DAYLY ALCOHOL CONSUMPTION AND OVERALL PERFORMANCE

#installing packages

wants <- c("mlogit","mgcv", "nnet","e1071" ,"VGAM","nnet","rpart.plot","ROCR","randomForest",
           "caret","lift","nnet","ggplot2","reshape2","caTools","mlbench","SDMTools","pROC")

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


#creating the uniqe index using mgcv, and getting the final data set d3

library(mgcv)

unique=uniquecombs(df[1:13])

uniqueIndex<-attributes(unique)

d3=df[uniqueIndex$row.names,]

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

#plotting independent variables

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

#creating logistic regression the model 

train.glm<- glm(Dalc~ ., data=d3,family= gaussian)

summary(train.glm)

plot(train.glm)

#predicting the 

d3$G3<-as.factor(d3$G3)

predicted.glm=predict(train.glm,type="response")

head(predicted.glm)

summary(predicted.glm)

tapply(predicted.glm,d3$Dalc,mean)

table(d3$Dalc, predicted.glm >2.5) #with threshold 2.5



#CVM regression

library(caret)

library(e1071) 


trainModels=list()

#forming set of 60 different values of cost and gamma 
#and applying to SVM to finding the best model

grid=expand.grid(cost=seq(1,901,100),gamma=seq(1,200,30))

for(i in 1:nrow(grid)){ 
  trainModels[[i]]=svm(Dalc ~ sex+ age+famsize+Pstatus+ Medu+Fedu + 
                         studytime +failures+ schoolsup+ activities+ higher +romantic
                       +famrel+freetime+goout, data = trainData,type= "C", kernel="radial",
                       cost=grid$cost[i], gamma = grid$gamma[i] ,probability=TRUE)  }

trainModels  #Will take 40 sec ,the best   cost:  901 ,gamma:  181 

train_svmBest<-svm(Dalc ~ sex+ age+famsize+Pstatus+ Medu+Fedu + 
                     studytime +failures+ schoolsup+ activities+ higher +romantic
                   +famrel+freetime+goout, data = trainData,type= "C", kernel="radial", cost=901,
                   gamma = 181,probability=TRUE) 


#predicting the test data

svmmodel.predict<-predict(train_svmBest,subset(testData,decision.values=TRUE))

svmmodel.class<-predict(train_svmBest,testData,type="class")

svmmodel.labels<-testData$Dalc


#analyzing result

library(SDMTools) 

svmmodel.confusion<-confusionMatrix(svmmodel.labels,svmmodel.class)

svmmodel.confusion #Accuracy : 0.8408



library(rpart) 

library(rpart.plot) 

library(e1071)

library(caret)


treeDalc1<-rpart(Dalc~.,data=trainData,method="poisson")

treeDalc2<-rpart(Dalc~.,data=trainData,method="class")

treeDalc3<-rpart(Dalc~.,data=trainData,method="anova")

prp(treeDalc1)

prp(treeDalc2)

prp(treeDalc3)



train.contr=trainControl(method="cv",number=20)

grid=expand.grid(.cp=(0:10)*0.001)

training=train(Dalc~sex+Medu+Mjob+reason+traveltime+paid+higher+freetime,
               data=trainData,method="rpart", 
               trControl=train.contr,tuneGrid=grid)

best=training$finalModel

prp(best)

best.prediction= predict(best, data =testData )

sum(best.prediction - trainData$Dalc)^2

