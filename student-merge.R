require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
#load data

d2=read.table("student-por.csv",sep=";",header=TRUE)

names (d2)
str(d2)
summary(d2)
head(d2, 10)
table(d2$Dalc  )
table(d2$Walc  )

model.days = lm(Dalc~ sex+ age+famsize+ activities+ higher +
              +famrel + freetime, data = d2)
summary (model.days)
SSE1 = sum(model.days$residuals^2)
SSE1
model.weekend = lm(Walc~ sex+ age+famsize+ activities+ higher +
              +famrel + freetime, data = d2)


model2 = lm(Dalc~ sex+ age+famsize+Pstatus+ Medu+Fedu + studytime +failures+ schoolsup+ activities+ higher +romantic
                +famrel + freetime+goout, data = d2)
summary (model2)
SSE2 = sum(model2$residuals^2)
SSE2

library("nnet")
test = multinom(Walc ~ sex+ age+famsize+Pstatus+ Medu+Fedu + studytime +failures+ schoolsup+ activities+ higher +romantic
                 +famrel + freetime+goout, data = d2)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p
exp(coef(test))

names (d2)
test <- multinom(Dalc ~ ., data = d2)

head(fitted(test))
predicted=predict(test,type="probs")
head(predicted)
bpp=cbind(d2, predicted)
names (bpp)
by(bpp[,34:38], bpp$Walc, colMeans)
bpp2 = melt (bpp,id.vars=c(names (d2)),value.name="probablity")

head(bpp2)
 library(ggplot2)

ggplot(bpp2, aes(x = write, y = probablity, colour = Walc)) + facet_grid(variable ~ ., scales="free")

