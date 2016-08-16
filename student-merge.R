
wants <- c("mlogit", "nnet", "VGAM","nnet","caret","lift","nnet","ggplot2","reshape2","caTools","mlbench")
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
contr.treatment(2)
contr.treatment(3)
contr.treatment(4)
contr.treatment(5)
contrasts(d2$Mjob) = contr.treatment(5)
contrasts(d2$Fjob) = contr.treatment(5)
contrasts(d2$famsize) = contr.treatment(2)
contrasts(d2$Pstatus) = contr.treatment(2)

contrasts(d2$reason) = contr.treatment(4)
contrasts(d2$guardian) = contr.treatment(3)
contrasts(d2$romantic) = contr.treatment(2)


table(d2$Dalc) #weekday alcohol consumption 1-5 score
table(d2$Walc) #weekend alcohol consumption 1-5 score

plot(density(d2$Dalc))  
plot(density(d2$Walc))  
plot(density(d2$health))  
plot(density(d2$absences))  
plot(density(d2$G1))  
plot(density(d2$G2))  
plot(density(d2$G3))  


#the regression
summary(lm(Walc ~ ., d2))

#create multinomial prediction model weekends alcohol consumption
sample.d2 = sample.split(d2 [,28], SplitRatio=.8,group = NULL )
trainIdx = which(sample.d2 == TRUE)
trainData = d2[trainIdx,]
testIdx = which(sample.d2 == FALSE)
testData = d2[testIdx,]



test = svm(Dalc ~ sex+ age+famsize+Pstatus+ Medu+Fedu + studytime +failures+ schoolsup+ activities+ higher +romantic
                 +famrel + freetime+goout, data = trainData)
SVM 
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


plot(confusionMatrix, values = 60, auto.key = list(columns = 5,
                                            lines = TRUE,
                                            points = FALSE))
#probably too high?
