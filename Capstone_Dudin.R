## Code for capstone project:  PREDICTING  STUDENT DAYLY ALCOHOL CONSUMPTION

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

#creating the uniqe index using mgcv, and getting the final data set d3

library(mgcv)

unique=uniquecombs(df[1:13])

uniqueIndex<-attributes(unique)

d3=df[uniqueIndex$row.names,]

#visualizing and exploratory analysing

str(d3)

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

plot(density(d3$Dalc))  

plot(density(d3$Walc))  

plot(density(d3$G3)) 


#plotting independent variables

ggplot (aes(x = Dalc,fill=sex),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(sex~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Dalc,fill=age),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(age~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Dalc,fill=Medu),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(Medu~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Dalc,fill=Mjob),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(Mjob~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Dalc,fill=Fedu),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(Fedu~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Dalc,fill=Fjob),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(Fjob~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Dalc,fill=freetime          ),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(freetime          ~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Walc,y=Dalc),data = d3) + geom_point()+geom_jitter(alpha = 0.2)


 

#building linear regression model
linear<-lm(Dalc ~ ., d3)

summary(linear)

predict.lm<-predict(linear,d3)

summary(predict.lm)

plot(linear)



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

#creating logistic regression model 

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

#multinomial regression

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

mult.regression <-  multinom(Dalc ~ . , data = trainData)

summary(mult.regression)

z <- summary(mult.regression)$coefficients/summary(mult.regression)$standard.errors

p <- (1 - pnorm(abs(z), 0, 1))*2

p

predict.test.multinom<-predict(mult.regression,newdata = testData)

predict.test.multinom.prob<- predict(mult.regression, newdata = testData, "probs")

summary(predict.train.multinom.prob)

table(testData$Dalc,predict.test.multinom)

mean(as.character(predict.test.multinom) != as.character(testData$Dalc)) #misclassification erreor  27.3% low

ggplot(testData, aes(x=testData$Dalc, y=predict.test.multinom)) + geom_point(aes(colour=Dalc))+geom_jitter(alpha = 0.2)


#CVM regression

library(caret)

library(e1071) 


train_svmBest<-svm(as.factor(Dalc) ~ sex+ age+famsize+Pstatus+ Medu+Fedu + 
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


#SVM with cross validation in R using caret

ctrl <- trainControl(method = "repeatedcv", repeats = 10)

set.seed(1500)

mod <- train(Dalc ~ sex+ age+famsize+Pstatus+ Medu+Fedu + 
            +                  studytime +failures+ schoolsup+ activities+ higher +romantic
             +                 famrel+freetime+goout, data=trainData, method = "svmLinear", trControl = ctrl)

mod


