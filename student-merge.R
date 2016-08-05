d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students
str(d3)
summary((d3))

table(d3$Dalc.y )
table(d3$Walc.y )

model1 = lm(Walc.x~ sex+ age+famsize+ activities.x+ higher.x +
              +famrel.x + freetime.x, data = d3)
summary (model1)
SSE1 = sum(model1$residuals^2)
SSE1

model2 = lm(Walc.x~ sex+ age+famsize+Pstatus+ Medu+Fedu + studytime.x +failures.x+ schoolsup.x+ activities.x+ higher.x +romantic.x
           +            +famrel.x + freetime.x+ goout.x+goout.x, data = d3)
summary (model2)
SSE2 = sum(model2$residuals^2)
SSE2
