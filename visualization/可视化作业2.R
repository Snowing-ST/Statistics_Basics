#data.table()
#dplyr
library(data.table)
setwd("E:/graduate/class/visualization/1数据集一：Wind数据/")
dailyprice = fread("dailyprice.txt")
head(dailyprice)
dim(dailyprice)
for(i in 1:dim(dailyprice)[2]) 
{
  print(colnames(dailyprice)[i])
  print(sum(is.na(dailyprice[,..i])))
}
#以dailyprice数据为例，自选变量，使用本节课介绍的多变量数据和高维数据的展示方法练习绘制


# 散点图矩阵、

corplotData = dailyprice[datetime == "2015-4-29",.(close,pct_chg,adjfactor,turn)] 
head(corplotData)
dim(corplotData)
library(car)
scatterplotMatrix(na.omit(corplotData),var.labels = c("收盘价","涨跌幅","复权因子","换手率"))

# 相关系数图
corplotData2 = dailyprice[datetime == "2015-4-29",.(close,chg,pct_chg,adjfactor,turn,free_turn,mkt_cap,mkt_freeshares)]
library(corrplot)
corrplot(cor(na.omit(corplotData2)),tl.col="black")

# 密度图、
library(ggplot2)
#生成几何对象
p<-ggplot(na.omit(corplotData),aes(x=pct_chg,y=turn))
#默认等高线图
p = p+geom_jitter(size = 1)+stat_density2d(h = 10,size = 1)
p+coord_cartesian(ylim=c(0,20))+xlab("涨跌幅")+ylab("换手率")
# 热图、
heatplotData = dailyprice[trade_code == "000001.SZ",.(datetime,close)]
heatplotData$datetime = as.Date(heatplotData$datetime,format='%Y-%m-%d')
heatplotData = cbind(heatplotData,year(heatplotData$datetime),quarter(heatplotData$datetime))
colnames(heatplotData)[3:4] = c("quarter","year")
head(heatplotData)

heatplot_group<-heatplotData[,.(mean_price=mean(close)),by=.(quarter,year)]
head(heatplot_group)
p<-ggplot(heatplot_group,aes(x=year,y=quarter,fill= mean_price))
p+ geom_tile(na.rm = T)

# 气泡图

p = ggplot(dailyprice[datetime == "2015-4-29"][1:100],aes(x = annualstdevr_100w,y = dividendyield2,size =close))
p+geom_point(shape=21,colour="black",fill="lightblue")+
  scale_size_area(max_size= 10)+xlab("年化波动率")+ylab("股息率")
# 平行坐标图、
library(lattice)
pData = dailyprice[datetime == "2015-4-29",.(close,chg,pct_chg,adjfactor,turn)]
upordown = data.frame(upordown = rep(0,dim(pData)[1]))
upordown[pData$chg>0,]=1
upordown[pData$chg<0,]=-1
upordown[pData$chg==0,]=0
pData = cbind(pData,upordown)
head(pData)
parallel(~pData[,1:4],pData,group = upordown,horizontal.axis= FALSE)



# 雷达图、
stars(pData[1:10,1:5],locations=c(0,0),col.lines= 2:7,radius=FALSE,key.loc=c(0,0),lwd=1.5,scale = TRUE,main="Star (Spider/Radar) Plot")
# 交互图
library(plotly)
plot_ly(pData,x=~chg,y=~pct_chg,type="scatter")
# 使用dailyprice中的定量变量，在同一张图中画出直方图和密度曲线图

p = ggplot(dailyprice[datetime == "2015-4-29"],aes(x = close))
p = p+geom_histogram(aes(y = ..density..),bins = 50,alpha = 0.5,fill = I("steelblue"))+stat_density(geom = "line",colour = I("blue"))
p+coord_cartesian(xlim=c(0,100))+xlab("收盘价")
# 并寻找数据中是否存在异常点，画出包含异常点及隐去异常点的箱线图、小提琴图。（如缺乏分类变量，可自行构造）
#有异常值
pData$upordown = as.character(pData$upordown)
pData$adjfactor[pData$adjfactor>100] = NA
p = ggplot(pData,aes(x=upordown,y=adjfactor))+xlab("涨或跌")+stat_summary(fun.y="mean",geom="point",shape=23,size=3,fill="white")
p+geom_boxplot()
p +geom_violin()


#无异常值
p = ggplot(pData,aes(x=upordown,y=adjfactor))+coord_cartesian(ylim=c(0,20))+xlab("涨或跌")+stat_summary(fun.y="mean",geom="point",shape=23,size=3,fill="white")#有异常值
p+geom_boxplot(outlier.shape= NA)
p +geom_violin(outlier.colour= NA,trim=FALSE)



# •2、选择前面ppt中除正态分布之外的两个分布，绘制其概率密度函数图及累积分布函数图，
# 其中一个用ggplot2来绘制。
set.seed(1)
x <-seq(0,5,length.out=100)
y <-df(x,10,5)
z = pf(x,10,5)
myrandom = data.frame(x = c(x,x),y = c(y,z),symbol = c(rep("pdf",100),rep("cdf",100)))
p = ggplot(myrandom)
p+geom_line(aes(x,y,colour = symbol))


set.seed(1)#用于设定随机数种子，一个特定的种子可以产生一个特定的伪随机序列，这个函数的主要目的是让模拟能够可重复出现
x <-seq(0,5,length.out=100)#（-5,5）中的100个数
y <-dchisq(x,2)#dnorm为正态分布的密度函数，均值为0，方差为1
z = pchisq(x,2)
plot(x,y,col="red",xlim=c(0,5),ylim=c(0,1),type='l',
     xaxs="i", yaxs="i",ylab='y',xlab='x')
lines(x,z,col="green")

legend("topleft",legend=c("概率密度分布",'累积分布'), lwd=1,col=c("red", "green"))
