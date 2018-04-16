library(foreign)
library(RColorBrewer)
library(ggplot2)
mydata = read.dta("E:/graduate/class/visualization/4数据集四：北大国家发展中心数据/demographic_background/demographic_background.dta",
                  convert.factors=F)
head(mydata)
dim(mydata)
var.lab <- attr(mydata,"var.labels")
# 这个可以查看对应的变量名和labels
name_label = data.frame(var.name=names(mydata),var.lab)
head(name_label)

#定性变量：rgender Gender  bb001  Birth Place  ba005
#定量： ba002_1 Birth Year 

#选择列
mydata2 = data.frame(mydata$ba005,mydata$rgender,mydata$ba002_1)
colnames(mydata2) = c("ba005","rgender","ba002_1")
head(mydata2)
mydata2 = na.omit(mydata2)


mydata2$rgender[mydata2$rgender==0] = NA
mydata2$rgender[mydata2$rgender==1] = "man"
mydata2$rgender[mydata2$rgender==2] = "female"
# mydata2$rgender = as.factor(mydata2$rgender)

mydata2$ba005[mydata2$ba005==1] = "yes"
mydata2$ba005[mydata2$ba005==2] = "no"

#ggplot2
library("grid")
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3)))
vplayout <- function(x, y) {
  viewport(layout.pos.row = x, layout.pos.col = y)
}
#条形图
p = ggplot(mydata2, mapping = aes(ba005,fill = I("steelblue"))) 
p = p+ geom_bar(width = 0.5)  + xlab("return home every week")
print(p, vp = vplayout(1,1))

#直方图

p = ggplot(mydata2,aes(ba002_1))
p = p+geom_histogram(bins = 30,aes(y = ..count..),alpha = 0.5,fill = I("steelblue"))
p = p+xlab("birth year")
print(p, vp = vplayout(1,2))

#饼图

p = ggplot(na.omit(mydata2), aes(x = factor(1),fill = rgender)) + geom_bar() 
p = p + coord_polar(theta = 'y') +ylab("")+xlab("")
print(p, vp = vplayout(1,3))


#分面
p = ggplot(na.omit(mydata2),aes(ba002_1))
p = p+geom_histogram(bins = 30,aes(y = ..count..),alpha = 0.5,fill = I("steelblue"))
p+facet_grid(ba005~rgender)+ylab("return home every week")+xlab("gender")+labs(title = "birth year distribution")


#R基础绘图
par(mfrow = c(1,3))
barplot(table(mydata2$ba005),col =  brewer.pal(5,"Greens"),space = 1,xlab = "return home every week")
hist(mydata2$ba002_1,col = "light green",xlab = "birthyear",main = "")
pie(table(mydata2$rgender),col =  brewer.pal(2,"Greens"))


#两两关系 堆积柱状图
ggplot(mydata2,aes(fill=ba005,x=rgender))+geom_bar()+discrete_scale("sss")
