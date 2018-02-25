setwd("/Users/zhangbeibei/Documents/Teaching/备课_2015/2015大数据统计基础/大数据-数据预处理/大数据预处理-授课2017/Data and Code 2017")
getwd()

########################################################
##########Basic R Operation with Missing Value##########
########################################################

#Missing values are represented by NA "Not Available"
x <- c(2, 4, NA, 5); x
y <- c("M", NA, "M", "F"); y
y=c(1,2,3)
mean(y)
#elementary numerical operations on objects that contain NA 
#return a single NA, until you specify the na.rm option
y=c(1,2,NA)
mean(y)
sum(y)
mean(y,na.rm=TRUE)
sum(y,na.rm=TRUE)
#(1)Source of NA, in R 
as.numeric(c("4", "six")) #inappropriate coercion
c(1:5)[7] #indexing out of range
(df<-data.frame(v1=1:3,v2=4:6))
df[4,]
df["4th",] #indexing with non-existing name
NA+8  #Operations with NAs
var(55) # Variance of a single number
##NAs indicate a problem that we must or should be address!!
#Importing missing values with read.table() function
#na.strings option to specify what characters are to be converted to NA
#default setting is na.strings="NA"
#blank fields are also considered to be missing values
mydat<-read.table("miss1.csv", 
                  na.strings = c(99999, 88888, "."),sep=",")
mydat
#(2)Indexing NA, replacing NA, and recoding NA
#generate logical vector to identify which positions 
#contain or do not contain NAs
x <- c(10, NA, 33, NA, 57)
y <- c(NA, 24, NA, 47, NA)
is.na(x)  #generate logical vector
x[!is.na(x)] #remove missing value
which(is.na(x)) #which positions are NA 
x[which(!is.na(x))]
y[is.na(x)]  #index other vector
x[is.na(x)] <- 999 #replacement
x
#recoding missing values to NA is accomplished using replacement
x <- c(1, -99, 3, -88, 5)
x[x==-99|x==-88] <- NA
x
m=matrix (c(1, -99, 3, 4, -88, 5), 2, 3)
m
m[m==-99|m==-88]<-NA  
#index like a vector, global replacement
#matrix is vector
m
m=matrix (c(1, -99, 3, 4, -88, 5), 2, 3)
m
m[m[,3]==-88, 3] <- NA #replace a column at a time
m
m[m[,1]==-99, 1] <- NA
m
fname <- c("Tom", "Unknown", "Jerry")
age <- c(56, 34, -999)
(z1=z2=data.frame(fname, age))
#global replacement
z2[z2=="Unknown"|z2==-999] <- NA  
z2
# Replacement one column at a time
z1$fname[z1$fname=="Unknown"] <- NA
z1$age[z1$age==-999] <- NA
z1
#(3)Working with NA values in data frames and factor
#(3.1) data frames
name <- c("Jose", "Ana", "Roberto", "Isabel", "Jen")
gender <- c("M", "F", "M", NA, "F")
age <- c(34, NA, 22, 18, 34)
df <- data.frame(name, gender, age)
df
na.fail(df) 
#na.fail() returns the object if it does not contain any missing values, returns an error otherwise
na.fail(df[c(1,3,5),]) 
na.omit(df)
complete.cases(df)
df[complete.cases(df),]
df
df[df$age<25,] #Indexing data frames, containing NAs
df[df$age<25&!is.na(df$age), ]
#(3.2)NA values in factors: 
#by default, factor levels do not include NA
#To include NA as a factor level,use the factor function, 
#setting the exclude option to NULL. 
df$gender
xtabs(~gender, data = df)
df$gender.na <- factor(df$gender, exclude = NULL) #exclude:缺失值不排除
df  #looks nothing different
xtabs(~gender.na, data = df) 
#Setting default NA behaviors in statistical models
#eg, glm function for generalized linear models
#glm has default NA behaviors that can be reset locally using the na.action option
#or reset globally using the na.action option setting in the options function.
options("na.action")  # display global setting
options(na.action="na.fail") # reset global setting
data(airquality)
airquality
any(is.na(airquality))  #Check missing value existed or not
lm(Ozone ~ Wind, data = airquality)
options(na.action="na.omit") #设置成忽略缺失值
fit=lm(Ozone ~ Wind, data = airquality)
summary(fit)
fit=lm(Ozone ~ Wind, data = airquality, na.action="na.omit") #这是局部设置
summary(fit)
#lm(,na.action=?), na.action setting by options() 
#Change the factory-fresh setting of option
#na.omit is the factory-fresh setting

#############################################################
############Exploration of  Missing Pattern##################
#############################################################

library(VIM)
?sleep
data(sleep,package="VIM")
head(sleep)
#list the complete observation and missing observation
sleep[complete.cases(sleep),]
nrow(sleep[complete.cases(sleep),])
sleep[!complete.cases(sleep),]
nrow(sleep[!complete.cases(sleep),])
mean(!complete.cases(sleep))  #percentage of observation with missing value
sum(is.na(sleep$Dream)) #number of missing, variable Dream
mean(is.na(sleep$Dream)) #percentage of missing,variable Dream
##When the data is big, complete.cases() may not work well
library(mice)
data(sleep,package="VIM")
md.pattern(sleep)#matirx, display the missing pattern
aggr(sleep, prop=FALSE, numbers=TRUE) #plot, more intuitional to display
matrixplot(sleep) #all cells of a data matrix are visualized by rectangles
marginplot(sleep[c("Gest","Dream")], pch=c(20),
           col=c("darkgray","red","blue"))

#Reference: <<R in action: Data analysis and graphics with R>>

#############################################################
################Imputation Method############################
#############################################################

#(1)listwise deletion or complete case analysis
data(airquality)
any(is.na(airquality))  #检查数据中是否存在缺失值
airquality[complete.cases(airquality),]  
#去掉有缺失值的观测，只保留完整观测
fit=lm(Ozone ~ Wind, data = airquality, na.action="na.omit")
deleted = na.action(fit)
naprint(deleted) #number of deleted cases
na.omit(airquality)

#(2)Pairwise deletion
data(airquality)
apply(airquality,2,mean,na.rm=TRUE)
cor(airquality,use="pair")
cov(airquality,use="pair")

#(3)Mean imputation
data(airquality)
air.mean=apply(airquality,2,mean,na.rm=TRUE)
air.mean
air=airquality
air$Ozone
air$Ozone <- 
  with(airquality,ifelse(is.na(Ozone),air.mean["Ozone"],Ozone))
air$Ozone
air$Solar.R
air$Solar.R <- 
  with(airquality,ifelse(is.na(Solar.R),air.mean["Solar.R"],Solar.R))
air$Solar.R
any(is.na(air))   #检查插补后是否还存在缺失值
head(air)
library(ggplot2)
#绘制插补后Ozone的直方图
air$colOzone=
  ifelse(complete.cases(airquality$Ozone),"notNA","Mean_imputation")
air
ggplot(air, aes(Ozone, fill = colOzone)) +
  geom_histogram(alpha = 0.5, position = 'identity')
#绘制插补后Solar.R 和Ozone的散点图
air$col=ifelse(complete.cases(airquality[,1:2]),"notNA","Mean_imputation")
ggplot(air,aes(x=Solar.R,y=Ozone,colour=col))+
  geom_point(size=4)
mean(airquality$Ozone,na.rm=TRUE)
mean(air$Ozone)
sd(airquality$Ozone,na.rm=TRUE) 
sd(air$Ozone)  
cor(airquality$Ozone,airquality$Solar.R,use="complete.obs")  
cor(air$Ozone,air$Solar.R)
#install.packages("mice") #for the first time
library(mice)
data(airquality)
air=airquality
imp=mice(air, method="mean",m=1,maxit=1)
#m: requests a single imputed datasets
#maxit: set the number of iterations to 1(no iteration)
names(imp)
air=complete(imp)
air
#unbiased estimate of mean
mean(airquality$Ozone,na.rm=TRUE)
mean(air$Ozone)
#standard deviation comparision
sd(airquality$Ozone,na.rm=TRUE)
sd(air$Ozone) #much smaller
#Causes bias, for any estimate other than the mean 
#Correlation
cor(airquality$Ozone,airquality$Solar,use="pair")
cor(air$Ozone,air$Solar.R)
#drop from 0.35 to 0.3

#(4)Regression Imputation
#imputate Ozone from the regression line
library(mice)
data(airquality)
air=airquality
fit = lm(Ozone ~ Solar.R, data = air)
air$Ozone[is.na(air$Ozone)] = 
  predict(fit, newdata = air[is.na(air$Ozone),]) 
air$Ozone
air$col=ifelse(complete.cases(airquality$Ozone),"notNA","imputation")
ggplot(air, aes(Ozone, fill = col)) +
  geom_histogram(alpha = 0.5, position = 'identity')
ggplot(air,aes(x=Solar.R,y=Ozone,colour=col))+
  geom_point(size=4)
sd(airquality$Ozone,na.rm=TRUE)
sd(air$Ozone,na.rm=TRUE)
cor(airquality$Ozone,airquality$Solar.R,use="complete.obs")
cor(air$Ozone,air$Solar.R,use="complete.obs")
#(5)Stochastic regression imputation
library(mice)
library(ggplot2)
imp=mice(airquality[,1:2],method="norm.nob",m=1,maxit=1,seed=1)
air=complete(imp)
air$col=ifelse(complete.cases(airquality[,1:2]),"notNA","imputation")
ggplot(air, aes(Ozone, fill = col)) +
   geom_histogram(alpha = 0.5, position = 'identity')
ggplot(air,aes(x=Solar.R,y=Ozone,colour=col))+
   geom_point(size=4)

#(6)Multiple imputation
imp=mice(airquality,seed=1,print=FALSE)
fit=with(imp,lm(Ozone~Wind+Temp+Solar.R))
pooled=pool(fit)
summary(pooled)
round(summary(pooled),3)[,c(1:3,5)]
fit.r=lm(Ozone ~ Wind+Temp+Solar.R, data = airquality)
round(coef(summary(fit.r)),3)
imp$imp  #可以观察到实际的插补值
imp$imp$Ozone 
air=complete(imp) #可观察到m次插补值的任意一次
head(air)
air$col=ifelse(complete.cases(airquality[,1:2]),"notNA","imputation")
ggplot(air, aes(Ozone, fill = col)) +
  geom_histogram(alpha = 0.5, position = 'identity')
ggplot(air,aes(x=Solar.R,y=Ozone,colour=col))+
  geom_point(size=4)


##############################################################
#################CASE STUDY###################################
##############################################################
##Reference:##################################################
##<<Data Mining with R: Learning with case studies>>##########
##install package:DMwR
library(DMwR)
data(algae)
head(algae)
dim(algae)
#(1)Delete the missing sample: when the proportation of missing value is small
algae[!complete.cases(algae),] #missing obs
nrow(algae[!complete.cases(algae),]) 
dim(na.omit(algae))
#compute how many NAs in each row(observation)
apply(algae,1,function(x)sum(is.na(x))) #统计每行缺失值个数
#Find rows(observations) numbers with too many NA values and delete those rows
#because they are useless sample with too many NA
#even with complicated imputation method, results big bias
manyNAs(algae,0.2) #哪几行的缺失值超过了20%
algae[manyNAs(algae,0.2),]
algae[-manyNAs(algae,0.2),]

#(2)Impute with highly frequent value
algae[!complete.cases(algae),]
algae[48,"mxPH"]=mean(algae$mxPH,na.rm=TRUE)
#usually interpolate by variable, not by observation
algae[,"Chla"]
algae[is.na(algae$Chla),"Chla"]=median(algae$Chla,na.rm=TRUE)
algae[,"Chla"]
data(algae) #reload data
algae=algae[-manyNAs(algae),]
#centralImputation() use “medium” for numeric data
#“mode” for catagorical data
algae=centralImputation(algae) #fill in any NA value in all columns
any(is.na(algae)) 

#(3)Impute missing values by variable correlation 
data(algae)
cor(algae[,4:18],use="complete.obs")
symnum(cor(algae[,4:18],use="complete.obs")) #相关系数可视化
#encode a given numeric vector, for visualizaiton of big matrics
#cutpoints = c(0.3, 0.6, 0.8, 0.9, 0.95)
#high correlation between NH4 and NO3, oPO4 and PO4
which(!complete.cases(algae[,c("NH4","NO3")]))
algae=algae[-manyNAs(algae),] #NH4,NO3 is not missing without obs 62 and 199
any(is.na(algae$oPO4))
any(is.na(algae$PO4))
which(is.na(algae$PO4))
lm(PO4~oPO4,data=algae)
algae[28,"PO4"]=42.897+1.293*algae[28,"oPO4"]
##If there exist many missing values
##Write a R function to impute multiple values
data(algae)
algae=algae[-manyNAs(algae),]
fillPO4=function(oP) 
  ifelse(is.na(oP),NA,42.897+1.293*oP) #in case oP is missing 
algae[is.na(algae$PO4),"PO4"]<-
  sapply(algae[is.na(algae$PO4),"oPO4"],fillPO4)
any(is.na(algae$PO4)) 
#(4)Impute missing values by similar observations  K近邻插补
data(algae)
algae=algae[-manyNAs(algae),]
algae=knnImputation(algae,k=10)
algae=knnImputation(algae,k=10,meth="median")
any(is.na(algae))
########################################################################

