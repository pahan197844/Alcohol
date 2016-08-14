require(lift)
require(nnet)
require(ggplot2)
require(reshape2)
wants <- c("mlogit", "nnet", "VGAM","nnet","caret")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
#load data
d2=read.table("student-por.csv",sep=";",header=TRUE)
#explore column names
names (d2)
#explore dataset
str(d2)
summary(d2)
head(d2, 10)
d2$goout=factor(d2$goout)
d2$Dalc=factor(d2$Dalc)
d2$Walc=factor(d2$Walc)
d2$health=factor(d2$health)


table(d2$Dalc) #weekday alcohol consumption 1-5 score
table(d2$Walc) #weekend alcohol consumption 1-5 score



library("nnet")
library("caTools")
#create multinomial prediction model weekends alcohol consumption
sample.d2 = sample.split(d2 [,28], SplitRatio=.8,group = NULL )
trainIdx = which(sample.d2 == TRUE)
trainData = d2[trainIdx,]
testIdx = which(sample.d2 == FALSE)
testData = d2[testIdx,]



test = multinom(Dalc ~ sex+ age+famsize+Pstatus+ Medu+Fedu + studytime +failures+ schoolsup+ activities+ higher +romantic
                 +famrel + freetime+goout, data = trainData)

summary(test)
#calculate Z score and p-Value for the variables in the model
z <- summary(test)$coefficients/summary(test)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p
exp(coef(test))

names (d2)
test1 <- multinom(Walc ~ ., data = trainData)

head(fitted(test))
#predicting 
predicted1=predict(test1,testData)
predicted2=predict(test1,testData,"probs")
head(predicted1)
head(predicted2)
#Confusion Matrix  
confusionMatrix=table(predicted1, testData$Walc)
confusionMatrix
#Misclassification Error
print(mean(as.character(predicted1) != as.character(testData$Walc)))

#probably too high. May be it can be improved by improving the model
#terms or may be the variables are not as good in explaining the 
#contraceptive method used. Either ways, I would encourage the investigator 
#to try other ML approaches as well for this problem.

plot(confusionMatrix, values = 60, auto.key = list(columns = 5,
                                            lines = TRUE,
                                            points = FALSE))
