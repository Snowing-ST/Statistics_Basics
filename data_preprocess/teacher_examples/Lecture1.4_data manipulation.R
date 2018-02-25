setwd("/Users/zhangbeibei/Documents/Teaching/备课_2015/2015大数据统计基础/大数据-数据预处理/大数据预处理-授课2017/Data and Code 2017")
#Commonly used R packages for data processing:
#plyr,reshape2,lubridate, stringr
#install.packages(c("plyr","reshape2","lubridate", "stringr","foreign"))
library(MASS)
library(foreign)
library(stringr)
library(plyr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(nycflights13)
library(lubridate)  

######################################################################
##############Categorical variable Manipulation#########################
######################################################################
#Value of categorical variables is stored in factor variables: levels
f=factor(c("small","large","large","small","medium"))
f
levels(f) #alphabetic order
#(1)Change the order of levels
sizes=factor(f,levels=c("small","medium","large"))
sizes
sizes=factor(sizes,levels=rev(levels(sizes))) #reverse the order
sizes
#change the order of levels based on one numerical variable 
#Changing order of levels are very important to control the order of labels in boxplot
iss=InsectSprays
iss
par(mfrow=c(1,1))
boxplot(count~spray,data=iss) 
iss$spray
xtemp=aggregate(count~spray,data=iss,median)
iss$spray=factor(iss$spray,
       levels=with(xtemp,spray[order(count,decreasing=TRUE)]))
iss$spray
iss
#Moreover, in multivariate analyses fix one level as reference level
#eg:lm(),glm() use the first level as reference level 
relevel(sizes,ref="large") #relevel determine which level comes first
#reorder(): reorders the levels based on the values of a numeric variable
with(iss,rev(reorder(spray,count,median)))
iss$spray
boxplot(count~spray,data=iss) 

#(2)Changing the names of levels in factor
levels(f)[levels(f)=="large"]="L"
levels(f)[levels(f)=="small"]="S"
levels(f)[levels(f)=="medium"]="M"
f
f=factor(c("small","large","large","small","medium"))
f
levels(f)=list(S="small",L="large",M="medium")
#all the levels should be put in "list" class
f
library(plyr)
#revalue() : replace the named levels of factor with the new values
f=factor(c("small","large","large","small","medium"))
f
(f=revalue(f,replace=c(small="S",large="L",medium="M")))
#replace=named character vector, with new values as values, and old values as names.

(pg=PlantGrowth)
str(pg)
pg$treatment[pg$group=="ctrl"]="no"
pg$treatment[pg$group=="trt1"]="yes"
pg$treatment[pg$group=="trt2"]="yes"
pg
str(pg)
pg$treatment=factor(pg$treatment)
str(pg)

######################################################################
################Character Manipulation##################################
######################################################################

nchar("South Pole")
paste("North","Pole")
paste("North","Pole",sep="")
paste("North","Pole",sep=".")
paste("North","and","South","Poles")
i=8
x=sprintf("the square of %d is %d",i,i^2)#打印到字符串里，而不是屏幕上
x
substr("Equator",3,5) #返回字符串指定位置范围的子字符串
strsplit("6-16-2011",split="-") 
#按split给的字符串把字符串拆分成子字符串
#返回的子字符串构成R列表
regexpr("uat","Equator")
#返回与pattern匹配的第一个子字符串的起始字符位置
#regular expression: search text
#Help: ?regex or books by Fitzgerald or Friedl
#If you frequently have to deal with "messy" text variables,learn it!!
#正则表达式：是一种通配符，用来描述一系列字符串的简略表达式
grep("[au]",c("Equator","North Pole","South Pole"))
#"[au]":表示含有字母a或u的字符串
grep("o.e",c("Equator","North Pole","South Pole"))
#"o.e": .表示任意一个字符
grep("N..t",c("Equator","North Pole","South Pole"))
#..表示任意两个字符
#.是一个元字符，就是不按照字面意思理解的字符
#那么，想要寻找包含.的字符怎么办呢?
grep(".",c("Equator.","North.Pole","South Pole"))
grep("\\.",c("Equator.","North.Pole","South Pole"))
#加入反斜杠让句点脱离元字符属性
#为什么用两个反斜杠？因为反斜杠本身也得用反斜杠必须脱离元字符的属性

#string normalization: transform a varity strings to a set of standard strings 
#We expect it to be more easily processed later
library(stringr)
str_trim(" Hello world")
str_trim(" Hello world",side="left")
str_trim("Hello world ",side="right")
str_pad(112,width=10,side="left",pad=0)
toupper("Hello world")
tolower("Hello world")
#Approximate string matching: 
#whether a substring occurs in another string
#Two methods: pattern matching and string distance
#Pattern Matching
gender=c("M","male","Female","fem.")
grepl("m",gender)    #case sensitive
grep("m",gender)
grepl("m",gender,ignore.case=TRUE) #case sensitive
grepl("m",tolower(gender))
#search for strings that start with an m or M
grepl("^m",gender,ignore.case=TRUE)
gender[grepl("^m",gender,ignore.case=TRUE)]="m"
gender[!grepl("^m",gender,ignore.case=TRUE)]="f"
gender

#String distances
adist("abc","bac")
codes=c("male","female")
gender=c("M","male","Female","fem.")
disMatrix=adist(gender,codes)
disMatrix
colnames(disMatrix)=codes  #for readability
rownames(disMatrix)=gender
disMatrix
i=apply(disMatrix,1,which.min)
i 
data.frame(rawtext=gender,coded.gender=codes[i]) 
#stringdist() compute better distance than adist
#allow character transpositions, which is a common typographical error
#But adist() does not allow
library(stringdist)
stringdist("abc","bac")
#amath() return an index to the closest match(codes) within a maximum distance
i=amatch(gender,codes,maxDist=4)
i
data.frame(rawtext=gender,code=codes[i])

#CASE STUDY:
#检测某文件夹中的文件名是否带有指定的后缀，比如查找出所有的html文件
fn=dir()
fn=fn[grep("\\.",fn)]
parts=strsplit(fn,".",fixed=TRUE)
whichtxt=sapply(parts,
                function(x)
                  x[length(x)]=="txt")
fn[whichtxt]

######################################################################
###################Converting Date##################################
######################################################################
current_time=Sys.time()
current_time
class(current_time)
as.numeric(current_time)
date1=as.Date(current_time)
date1
as.numeric(date1)
end_time=Sys.time()
end_time-current_time #Running time of some program

library(lubridate)  
today()
now()

#three ways you’re likely to create a date/time: 
#(1)From a string.
#Automatically work out the format once you specify the order of the component.
#contain functions facilitating conversion of text to POSIXct date
ymd(20170131)
dates=c("15/02/2013","15 Feb 2013","It happened on 15 02 13")
dmy(dates)
#dmy() assumes that dates denoted in the order day-month-year
#and try to extract valid dates
#Similar functions: dmy(),myd(),ydm(),mdy(),dym(),ymd()
dym("15 Febr. 2013") #only converting certain standard notations
ymd_hms("2017-01-31 20:11:59")
ymd_hm("2017-01-31 20:11")
#(2)From individual date-time components.
library(nycflights13)
?flights
flights %>%
  dplyr::select(year, month, day, hour, minute)

flights %>%
  dplyr::select(year, month, day, hour, minute) %>% 
  dplyr::mutate(
    departure = make_datetime(year,month, day, hour, minute))

head(flights$dep_time,100)

make_datetime_100 <- function(year, month, day, time) 
  {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- 
  flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
  dep_time = make_datetime_100(year, month, day, dep_time), 
  arr_time = make_datetime_100(year, month, day, arr_time), 
  sched_dep_time = make_datetime_100(year, month, day, sched_dep_time ),
  sched_arr_time = make_datetime_100( year, month, day, sched_arr_time)) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))

#visualize the distribution of departure times across the year
flights_dt %>%
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 86400) 

# 86400 seconds = 60*60*24 = 1 day

#
flights_dt %>%
  filter(dep_time < ymd(20130102)) %>%
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 60*10) # 600 s = 10 minutes

#(3)From an existing date/time object.
as_datetime(60 * 60 * 10)
as_date(365 * 10 + 2)

datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime) 
month(datetime) 
mday(datetime) #一个月中第几天。几号？
yday(datetime) #一年第几天？
wday(datetime) #星期几？
month(datetime, label = TRUE)
wday(datetime, label = TRUE, abbr = FALSE)

flights_dt %>%
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar()
 # more flights depart during the week than on the weekend:

#average departure delay ~ minute 
flights_dt %>%
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>%
  dplyr::summarize(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>%
  ggplot(aes(minute, avg_delay)) +
  geom_line()

#It looks like flights leaving in minutes 20–30 and 50–60
#have much lower delays than the rest of the hour

#Interestingly, if we look at the scheduled departure time we don’t see such a strong pattern:
sched_dep <- flights_dt %>%
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>%
  summarize(
    avg_delay = mean(arr_delay, na.rm = TRUE), n = n())

ggplot(sched_dep, aes(minute, avg_delay)) + geom_line()

#ggplot(sched_dep, aes(minute, n)) + geom_line()

#Reference: 
#Hadley Wickham and Garrett Grolemund
#<R for Data Science - Import, Tidy, Transform, Visualize and Model Data>

#####################################################################
##########Data aggregation: Split-Apply-Combine Strategy#############
#####################################################################

#(1)apply(),lapply(),sapply(),mapply()
(ma=matrix(1:100,nrow=20))
apply(ma,1,sum)
apply(ma,2,sum)
#similar to R function: rowSums() and colSums()
#add optional arguments to your function
ma[2,3]=NA
apply(ma,1,sum)
apply(ma,2,sum)
apply(ma,1,sum,na.rm=TRUE)
apply(ma,2,sum,na.rm=TRUE)
Thelist=list(A=matrix(1:9,nrow=3),B=1:5,C=matrix(1:4,nrow=2),D=c(2))
Thelist
lapply(Thelist,sum)
sapply(Thelist,sum)
#lapply() works by applying a function to each elment of a list
#and returning results as a list;
#sapply() return results as a vector instead.

#(2)aggregate()
#Splits the data into subsets,
#computes summary statistics for each,
#and returns the result in a convenient form.
require(ggplot2)
data(diamonds)
head(diamonds)
#Usage: aggregate(formula, data, FUN, ...,)
#a formula, such as y ~ x or cbind(y1, y2) ~ x1 + x2
#where the y variables are numeric data to be split into groups 
#according to the grouping variables ”x“ (usually factors).
aggregate(price~cut,data=diamonds,mean)
aggregate(price~cut+color,data=diamonds,mean)
aggregate((price+carat)~cut+color,data=diamonds,mean)

#(3)plyr Package
#ddply(),dlply()
#matrix
a=matrix(1:21,nrow=3,ncol=7)
a
aaply(.data=a,.margins=1,.fun=mean) 
aaply(a,1,mean)
aaply(a,2,mean) #same as apply() in simple problem
#data.frame
names=c("John","Mary","Alice","Peter","Roger","Phyillis") 
age=c(13,15,14,13,14,13) 
sex=c("Male","Female","Female","Male","Male","Female") 
data=data.frame(names,age,sex)
data
aver=function(data)
  c(average.age=mean(data$age))
dlply(data,"sex",aver)
ddply(data,"sex",aver)
daply(data,"sex",aver)


#####################################################################
##########Case study: data(baseball)#################################
#####################################################################
#baseball数据集包括了1887-2007年间1228位美国职业棒球运动员15年以上的击球记录
#baseball data set contains the batting records 
#for all professional US players with 15 or more years of data
library(plyr)
data(baseball)
head(baseball)
?baseball
baseball[baseball$id=="yosted01",]
baseball$stint
#generate another variable: OBP(On-Base Percentage)
#OBP=(h+bb+hbp)/(ab+bb+hbp+sf)
baseball$sf[baseball$year<1954]
baseball$sf[baseball$year<1954]=0 
#set missing data to 0
baseball$hbp[is.na(baseball$hbp)]=0
any(is.na(baseball$sf))
any(is.na(baseball$hbp))
#check if any missing values existed
#OBP in each year, for each player
baseball$OBP=with(baseball,(h+bb+hbp)/(ab+bb+hbp+sf))
tail(baseball)
#How about OBP for each play in their whole career?
#OBP=sum(h+bb+hbp)/sum(ab+bb+hbp+sf)
obp=function(data) 
  c(OBP=with(data,sum(h+bb+hbp)/sum(ab+bb+hbp+sf)))
obp(baseball[baseball$id=="aaronha01",])
careerOBP=ddply(baseball,"id",obp)
careerOBP
head(careerOBP)
arrange(careerOBP,OBP)

baberuth <- subset(baseball, id == "ruthba01")
# calculate “career year”, 
# i.e. the number of years since the player started playing
baberuth <- transform(baberuth, cyear = year - min(year) + 1) 
#do this for all players, apply transform() to each piece
baseball <- ddply(baseball, .(id), transform, cyear = year - min(year) + 1)
head(baseball)
library(ggplot2)
ra <- with(baberuth,rbi/ab)
a <- data.frame(cyear=baberuth$cyear,ra)
p <- ggplot(a,aes(cyear,ra))
p + geom_line()
#summarize the pattern across all players, we first need to figure out what the common patterns are
#time series plot of rbi/ab
baseball <- subset(baseball, ab >= 25)
model <- function(df) lm(rbi / ab ~ cyear, data = df)
model(baberuth)
bmodels <- dlply(baseball, .(id), model)
head(bmodels)
rsq <- function(x) summary(x)$r.squared
bcoefs <- ldply(bmodels, function(x) c(coef(x), rsquare = rsq(x)))
head(bcoefs)
names(bcoefs)[2:3] <- c("intercept", "slope")
head(bcoefs)
baseballcoef <- merge(baseball, bcoefs, by = "id")
head(baseballcoef)
subset(baseballcoef, rsquare > 0.999)$id
#selecting records with an R^2 of 1
qplot(rsquare, data=bcoefs, geom="histogram", binwidth=0.01)
head(bcoefs)
p <- ggplot(bcoefs, aes(slope, intercept))
p + geom_point(aes(size = rsquare),alpha=0.3)
#show a negative correlation between slope and intercept
#particularly bad models have estimates for both values close to 0

#Reference:
#Wickham H. The Split-Apply-Combine Strategy for Data Analysis[J]. Journal of Statistical Software, 2011, 40(01):1-29.

#####################################################################
###################Join data, Combine data ##########################
#####################################################################

#(1) cbind(), rbind(): same number of rows or identical columns
(dat=data.frame(x=c(1:5),y=c(2:6)))
c(112,289)
(newdata=rbind(dat,c(112,289)))
(z=seq(223,228))
z
(cbind(newdata,z))

###########Merge datasets########
#(2) merge(),join()
#美国国际开发署开发政府公开的原始数据
download.file(url="http://jaredlander.com/data/US_Foreign_Aid.zip",
              destfile="ForeignAid.zip")
unzip("ForeignAid.zip")
library(stringr)
theFiles=dir(pattern = "^US_Foreign_Aid")
theFiles
for (a in theFiles)
{
  #build a simple name to assign data
  nameToUse=str_sub(string=a,start=12,end=18)
  temp=read.table(a,header=TRUE,sep=",",stringsAsFactors=FALSE)
  #assign them into workspace
  assign(x=nameToUse,value=temp)
}
head(Aid_00s)
sum(Aid_00s$Country.Name!=Aid_90s$Country.Name)
#by.x: names of left data
#by.y: names of right data
Aid_90s00s=merge(x=Aid_90s,y=Aid_00s,
                 by.x=c("Country.Name","Program.Name"),
                 by.y=c("Country.Name","Program.Name"))
head(Aid_90s00s)
#join() in Package plyr, which is similar with merge()
#faster than merge()
library(plyr)
names(Aid_90s)
names(Aid_00s)
Aid90s00sJoin=join(x=Aid_90s,y=Aid_00s,
                   by=c("Country.Name","Program.Name"))
names(Aid90s00sJoin)
##########Join multiple dataframe into list
frameNames=str_sub(string=theFiles, start=12, end=18)
frameNames
#build an empty list
frameList=vector("list",length(frameNames))
frameList
names(frameList)=frameNames
frameList
#add each data.frame into list
for (a in frameNames) frameList[[a]]=get(a)
head(frameList[["Aid_00s"]])
head(frameList[[2]])
head(frameList[[3]])

allAid=frameList[[1]]
for (a in frameNames[-1])
  allAid=join(allAid,frameList[[a]],by=c("Country.Name","Program.Name"),)
dim(allAid)
require(useful)
corner(allAid,c=15)
bottomleft(allAid,c=15)
#Exercise: melting data
#Variables: Country.Name, Program.Name, Year, Vale.

##
#data()
library(nycflights13)
airlines 
airports #faa airport code
planes
weather
flights

#keys: The variables used to connect each pair of tables are called keys

#create a narrow dataset
flights2 <- 
  flights %>%
  dplyr::select(year:day, hour, origin, dest, tailnum, carrier)

flights2

#add the full airline name to the flights2 data
flights2 %>%
  dplyr::select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")

