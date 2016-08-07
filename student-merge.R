require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
#load data
d2=read.table("student-por.csv",sep=";",header=TRUE)
#explore column names
names (d2)
#explore dataset
str(d2)
summary(d2)
head(d2, 10)
table(d2$Dalc) #weekday alcohol consumption 1-5 score
table(d2$Walc) #weekend alcohol consumption 1-5 score
#create linear prediction model weekday alcohol consumption
model.days = lm(Dalc~ sex+ age+famsize+ activities+ higher +
              +famrel + freetime, data = d2)
summary (model.days)
SSE1 = sum(model.days$residuals^2)
SSE1
#create linear prediction model weekends alcohol consumption
model.weekend = lm(Walc~ sex+ age+famsize+ activities+ higher +
              +famrel + freetime, data = d2)


model2 = lm(Dalc~ sex+ age+famsize+Pstatus+ Medu+Fedu + studytime +failures+ schoolsup+ activities+ higher +romantic
                +famrel + freetime+goout, data = d2)
summary (model1)
summary (model2)
SSE2 = sum(model1$residuals^2)
SSE2
SSE3 = sum(model2$residuals^2)
SSE4

library("nnet")
#create multinomial prediction model weekends alcohol consumption
test = multinom(Walc ~ sex+ age+famsize+Pstatus+ Medu+Fedu + studytime +failures+ schoolsup+ activities+ higher +romantic
                 +famrel + freetime+goout, data = d2)
summary(test)
#calculate Z score and p-Value for the variables in the model
z <- summary(test)$coefficients/summary(test)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p
exp(coef(test))

names (d2)
test <- multinom(Dalc ~ ., data = d2)

head(fitted(test))
#predicting 
predicted=predict(test,type="probs")
head(predicted)
bpp=cbind(d2, predicted)
names (bpp)
by(bpp[,34:38], bpp$Walc, colMeans)
#melting data for unique id variable combination
bpp2 = melt (bpp,id.vars=c(names (d2)),value.name="probablity")

head(bpp2)
 library(ggplot2)
#ggplot does not work?????
ggplot(bpp2, aes(x = write, y = probablity, colour = Walc)) + facet_grid(variable ~ ., scales="free")

#create logical prediction model weekends alcohol consumption
library(caTools)
set.seed(123)

sample.d2 = sample.split (d2 [,28], SplitRatio=.8,group = NULL )
sample.d2 #doesn`t take sample(still 649, not 80%)

