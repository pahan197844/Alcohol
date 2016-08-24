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

#creating the uniqe index using mgcv
library(mgcv)
byname=subset(df,select=name)
unique=uniquecombs(byname)
uniqueIndex<-attr(unique,"index")
d3=df[uniqueIndex,]



