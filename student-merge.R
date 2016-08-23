
wants <- c("mlogit", "ROCR","nnet", "VGAM","nnet","rpart.plot","caret","lift","nnet","ggplot2","reshape2","caTools","mlbench")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

#load data
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

d3<-mutate(d3,Alc.total = (d3$Walc*2+d3$Dalc*5)/7) 
summary(d3$Alc.total)
str(d3)



table(d2$Dalc) #weekday alcohol consumption 1-5 score
table(d2$Walc) #weekend alcohol consumption 1-5 score


mytheme2=theme(panel.grid.major = element_line(colour = "white")) + theme(panel.border = 
                                                                           element_rect(linetype = "solid", colour = "white"))
mytheme1=theme_bw(base_size = 12, base_family = "")

ggplot (aes(x = Alc.total,fill=sex),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(sex~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Alc.total,fill=age),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(age~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Alc.total,fill=Pstatus),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(Pstatus~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Alc.total,fill=Fjob),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(Fjob~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Alc.total,fill=schoolsup),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(schoolsup~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Alc.total,fill=activities),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(activities~.,scale="free") +mytheme1+mytheme2

ggplot (aes(x = Alc.total,fill=goout),data = d3) + geom_histogram(binwidth = 1,na.rm = T) + 
  facet_grid(goout~.,scale="free") +mytheme1+mytheme2



plot(density(d3$Walc))  
plot(density(d3$health))  
plot(density(d3$absences))  
plot(density(d3$G1))  
plot(density(d3$G2))  
plot(density(d3$G3))  




#create multinomial prediction model weekends alcohol consumption

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

#the regression
linear<-lm(Alc.total ~ ., d3)
summary(linear)
#RMSE linear
rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- linear$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 2.312008e-15
predict.lm<-predict(linear,d3)
summary(predict.lm)
library(ggplot2)
#visualisation the most significant relationships
ggplot(d3, aes(x = sex, y = Alc.total,y ~ x)) +
  geom_jitter(alpha = 0.2)+
  geom_point()+geom_smooth(method = "lm")
ggplot(d3, aes(x = age, y = Alc.total,y ~ x)) +
  geom_jitter(alpha = 0.2)+
  geom_point()+geom_smooth(method = "lm")
ggplot(d3, aes(x = Pstatus, y = Alc.total,y ~ x)) +
  geom_jitter(alpha = 0.2)+
  geom_point()+geom_smooth(method = "lm")
ggplot(d3, aes(x = Fjob, y = Alc.total,y ~ x)) +
  geom_jitter(alpha = 0.2)+
  geom_point()+geom_smooth(method = "lm")
ggplot(d3, aes(x = schoolsup, y = Alc.total,y ~ x)) +
  geom_jitter(alpha = 0.2)+
  geom_point()+geom_smooth(method = "lm")
ggplot(d3, aes(x = activities, y = Alc.total,y ~ x)) +
  geom_jitter(alpha = 0.2)+
  geom_point()+geom_smooth(method = "lm")
ggplot(d3, aes(x = goout, y = Alc.total,y ~ x)) +
  geom_jitter(alpha = 0.2)+
  geom_point()+geom_smooth(method = "lm")




#Splitting into training and testing sets.
library(caTools) 
sample.d3 = sample.split(d3$Alc.total, SplitRatio=0.8,group = NULL )
trainIdx = which(sample.d3 == TRUE)
trainData = d3[trainIdx,]
testIdx = which(sample.d3 == FALSE)
testData = d3[testIdx,]

#Display of distributed data
dim(trainData)
dim(testData)
##Train VD model for train data
library(caret)
library(e1071) 
train_svm<-svm(G3 ~Alc.total, data = trainData,type= "C", kernel="radial", cost=900,
               gamma = 20,probability=TRUE)

#RMSE SMV
errorSVM <- trainData$G3-train_p
svrPredictionRMSE <- rmse(error)  # 3.157061
train_p<-predict(train_svm,trainData,probability=TRUE)
cm<-confusionMatrix(train_p,trainData[,34])
cm
#gamma array
Gamma = c(0.05, 1, 5, 10 ,15,20 ,25 ,30 ,35 ,40 ,45, 50, 55 ,60 ,65)
##sex+ age+famsize+Pstatus+ Medu+Fedu + studytime +failures+ schoolsup+ activities+ higher +romantic
#+famrel + freetime+goout
plot(train_p,d3[,34])


#tune up SVM model 
tuneResult <- tune(svm, G3 ~ Alc.total, data = trainData,
                   ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9))
) 
print(tuneResult)
plot(tuneResult)

#Logistic regression
set.seed(123)
#using the train function on the training set logistic regression
train.glm<- glm(Alc.total~ ., data=trainData)
summary(train.glm)


#predicting 
predicted1=predict(train.glm,testData)
head(predicted1)
str(predicted1)



#Confusion Matrix  
confusionMatrix=table(predicted1, testData$Alc.total)
confusionMatrix
#Misclassification Error
print(mean(as.character(predicted1) != as.character(testData$Alc.total)))


plot(confusionMatrix, values = 60, auto.key = list(columns = 5,
                                            lines = TRUE,
                                            points = FALSE))
#CATR

library(rpart) 
library(rpart.plot) 
d3$G3=as.factor(d3$G3)
sample.d31 = sample.split(d3$G3, SplitRatio=0.8,group = NULL )
trainIdx1 = which(sample.d31 == TRUE)
trainData1 = d3[trainIdx1,]
testIdx1 = which(sample.d31 == FALSE)
testData1 = d3[testIdx1,]
treeG3<-rpart(factor(G3)~sex+age+Pstatus+Fjob+schoolsup+activities+goout+Alc.total,data=trainData1,method="class")
prp(treeG3)
