library(RColorBrewer)


face = read.csv("E:/研究生/graduate/EDA/面膜数据.csv")
head(face)
tail(face)

#因变量
hist(log(face$月销量),col = "light blue",breaks = 20,xlab = "月销量的对数(log(件))",main = "")

summary(face$月销量)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0    51.5   199.5  1282.0   909.8 49690.0 

face[which.max(face$月销量),]
face[c(face$月销量==0),]



#自变量
#价格与评价--------------------------------------------------------------------
par(mfrow = c(1,2))
hist(log(face$价格),col = "light blue",breaks = 20,xlab = "价格的对数(log(元))",main = "")
hist(log(face$评价数),col = "light blue",breaks = 20,xlab = "评价数的对数(log(条))",main = "")
par(mfrow = c(1,1))
pairs(cbind(log(face$价格),log(face$评价数),log(face$月销量)),labels = c("log(价格)","log(评价)","log(月销量)"))

face[which.max(face$价格),]
face[which.min(face$价格),]


summary(face$价格)
summary(face$评价数)

face[which.max(face$评价),]
face[face$评价==1,]

plot(face$价格[order(face$月销量,decreasing = T)][1:100])
price_class = rep(0,dim(face)[1])
price_class[order(face$月销量,decreasing = T)][1:round(0.25*dim(face)[1])] = 1
price_class[order(face$月销量,decreasing = T)][round(0.25*dim(face)[1]):round(0.5*dim(face)[1])] = 2
price_class[order(face$月销量,decreasing = T)][round(0.5*dim(face)[1]):round(0.75*dim(face)[1])] = 3
price_class[order(face$月销量,decreasing = T)][round(0.75*dim(face)[1]):dim(face)[1]] = 4


# face2 = cbind(face,price_class)
face2$price_class = price_class
boxplot(face2$价格~face2$price_class,xlab = "月销量等级",ylab = "价格",ylim = c(1,1000))

#品牌--------------------------------------------------------------------
length(levels(face$名称))
a = table(face$名称)
a = sort(a,decreasing = T)
a[1] #韩后，41家店售卖
sum(a[11:length(a)])
par(mfrow = c(1,2))
barplot(a[1:5],col =  brewer.pal(5,"Blues")[seq(5,1,-1)],space = 0.5,xlab = "品牌",ylab = "售卖店铺数")
#前10品牌总频次占所有品牌总频次278/794 = 35.01%

sale_sum = aggregate(face$月销量,by = list(face$名称),FUN = "sum")
head(sale_sum)
head(sale_sum[order(sale_sum$x,decreasing = T),],10) #总销量前十品牌
topten = sale_sum[order(sale_sum$x,decreasing = T),][1:5,]
topten1 = array(topten$x)
names(topten1) = topten$Group.1
barplot(topten1,col = brewer.pal(5,"Blues")[seq(5,1,-1)],space = 0.5,xlab = "品牌",ylab = "总销量")

df = data.frame()
for(name in c("韩后",	"美肤宝","自然堂","温碧泉","春纪","柏氏 ","欧诗漫","精装金美芙坊"))
  {
    # print(name)
    maxi = max(face$月销量[face$名称==name])
    mini = min(face$月销量[face$名称==name])
    meani = mean(face$月销量[face$名称==name])
    top3 = sum(sort(face$月销量[face$名称==name],decreasing = T)[1:3])
    per = top3/sum(face$月销量[face$名称==name]) 
    tmp = data.frame(name,maxi,mini,meani,top3,per)
    df = rbind(df,tmp)
}
df



#店铺所在地--------------------------------------------------------------------
provs = sort(table(face$店铺所在地),decreasing = T)
barplot(provs[1:5],col = "light blue",space = 0.5,xlab = "省份",ylab = "频数")
sum(provs[6:length(provs)])
sum(provs)
aa = array(c(200,3))
names(aa) = c("其他省份","海外")
provs1 = c(provs[1:5],aa)
barplot(provs1,col = "light blue",space = 0.5,xlab = "省份",ylab = "频数")
percentage = paste(round(100*provs1/sum(provs1),2),"%")

pie(provs1,labels =paste(names(provs1),percentage),col = brewer.pal(length(names(provs1)),"Blues"))
#前5省份频次占总频次的（794-203）/794 = 74.43%

#产地
org = sort(table(face$产地),descreasing = T)
ch = org[6]
othr = org[1:5]
b = array(sum(othr))
names(b) = "其他国家"
ch1 = c(ch,b)
percentage = paste(round(100*ch1/sum(ch1),2),"%")
pie(ch1,labels =paste(names(ch1),percentage))
percentage = paste(round(100*othr/sum(othr),2),"%")
pie(othr,labels =paste(names(othr),percentage),col = brewer.pal(5,"Blues"))


#功效--------------------------------------------------------------------
func = face[8:12]
for(i in 1:dim(func)[2])
{
  bb = table(func[,i])
  if(i == 1) {bbb = bb}
  else {bbb = rbind(bbb,bb)}
}
bbb = t(bbb)
colnames(bbb) = names(func)
bbb = bbb[c(2,1),]

barplot(bbb,space = 0.5,ylim = c(0,1000),
        col = brewer.pal(2,"Blues")[c(2,1,3)],xlab = "功效",ylab = "频次",legend = c("有此功效","无此功效"))

boxplot(face$月销量~face$美白提亮,outline = F)
boxplot(face$月销量~face$控油祛痘,outline = F)
boxplot(face$月销量~face$清洁毛孔,outline = F)
boxplot(face$月销量~face$提拉紧致,outline = F)

func_class = rep(0,dim(face)[1])
func_class[rowSums(face[,8:12])==1] = 1
func_class[rowSums(face[,8:12])==2] = 2
func_class[rowSums(face[,8:12])==3] = 3
func_class[rowSums(face[,8:12])==4] = 4
func_class[rowSums(face[,8:12])==5] = 5

table(func_class)
face[func_class==0,]
par(mfrow = c(1,2))
boxplot(face$月销量~func_class,outline = T,xlab = "功效种类数目",ylab = "月销量")
boxplot(face$价格~func_class,outline = T,xlab = "功效种类数目",ylab = "价格")



#肤质--------------------------------------------------------------------
skin = matrix(0,dim(face)[1],6)
skin = as.data.frame(skin)
colnames(skin) = c("干性","油性","中性","混合型","敏感型","不针对特定肤质")
skin$干性[c(face$适合肤质 %in% c("干性肤质","中性及干性肤质"))] = 1
skin$中性[c(face$适合肤质 %in% c("中性肤质","中性及干性肤质"))] = 1
skin$油性[c(face$适合肤质 %in% c( "油性肤质" ,"油性及混合性肤质"))] = 1
skin$混合型[c(face$适合肤质 %in% c( "混合型肤质" ,"油性及混合性肤质"))] = 1
skin$敏感型[c(face$适合肤质 =="敏感性肤质")] = 1
skin$不针对特定肤质[c(face$适合肤质 =="任何肤质")] = 1
head(skin)
face1 = cbind(face,skin)
head(face1)
levels(face$适合肤质)
table(face$适合肤质)
           
for(i in 1:dim(skin)[2])
{
  bb = table(skin[,i])
  if(i == 1) {bbb = bb}
  else {bbb = rbind(bbb,bb)}
}
bbb = t(bbb)
colnames(bbb) = names(skin)
bbb = bbb[c(2,1),]
barplot(sort(bbb[1,1:5],decreasing = T),space = 0.5,col = brewer.pal(5,"Blues")[seq(5,1,-1)],ylim = c(0,60),xlab = "针对肤质",ylab = "频次",legend = c("针对此肤质的产品","不针对此肤质的产品"))

par(mfrow = c(1,1))
every = table(skin$不针对特定肤质)
special = colSums(skin[,1:5])
names(every) = c("针对特定肤质","不针对特定肤质")
percentage = paste(round(100*every/sum(every),2),"%")
pie(every,labels =paste(names(every),percentage),init.angle = -30)
percentage = paste(round(100*special/sum(special),2),"%")
pie(special,labels =paste(names(special),percentage),col = brewer.pal(5,"Blues")[seq(5,1,-1)])


boxplot(face$月销量~face$适合肤质,outline = F)

