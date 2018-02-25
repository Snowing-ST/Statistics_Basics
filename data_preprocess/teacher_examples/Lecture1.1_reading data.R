#read plain-text rectangular files into R
setwd("/Users/zhangbeibei/Documents/Teaching/备课_2015/2015大数据统计基础/大数据-数据预处理/大数据预处理-授课2017/Data and Code 2017")
getwd()

#Commonly used R packages for data processing:
#plyr,reshape2,lubridate, stringr
#install.packages(c("plyr","reshape2","lubridate", "stringr","foreign"))

library(MASS)
library(foreign)
library(stringr)
library(plyr)
library(reshape2)
library(ggplot2)
######################################################################
###################Importing Data##############################
###################################################################### 
#R包自带数据
data(diamonds)
diamonds
head(diamonds)
tail(diamonds)
str(diamonds)

#R工具包，可以帮助从一些开放源直接下载金融数据，包括雅虎财经、谷歌财经、等
library(quantmod)
getSymbols("AAPL",from="2017-09-01") 
#download daily prices of Apple stock from yahoo
dim(AAPL)
head(AAPL)
tail(AAPL)
chartSeries(AAPL,theme=chartTheme('white'))
getFX("USD/CNY",from="2017-01-01") 
#download exchange Rates
head(USDCNY)
tail(USDCNY)
chartSeries(USDCNY,theme=chartTheme('white'))

#read.table(): reading text data into R data.frame
x=read.table("data1.1.txt",sep=",")
x
y=read.table("http://www.jaredlander.com/data/Tomato%20First.csv",
             header=TRUE,sep=",")
#data.frame should be inspected with head(),str(),summary()
head(y)
str(y)
summary(y)
#cousins: read.table(), read.csv(), read.csv2(), read.delim(), read.delim2(), read.fwf()
#Except for read.table and read.fwf, each functions assumes first line contains columns headers
person=read.csv("data1.1.txt",sep=",")
person
person=read.csv("data1.1.txt",
                header=FALSE,
                col.names=c("age","height"))
person
str(person) 
#by default text variable are converted to factor
person=read.csv("data1.1.txt",
                header=FALSE,
                col.names=c("age","height"),
                colClasses=c("numeric","numeric"))
#Error is desirable if you need to be strict about how data is offered to R
#If you are ready to trycatch
person=read.csv("data1.1.txt",
                header=FALSE,
                col.names=c("age","height"),
                stringsAsFactors=FALSE)
person
str(person)
person$height=as.numeric(person$height)
person
str(person)

#############download data from website, unzip data########
#############read data from mutiple separate files
#美国国际开发署开发政府公开的原始数据
download.file(url="http://jaredlander.com/data/US_Foreign_Aid.zip",
              destfile="ForeignAid.zip")
unzip("ForeignAid.zip")
library(stringr)
dir()
theFiles=dir(pattern = "^US_Foreign_Aid")
theFiles
Aid_60s=...
#loop through those files
for (a in theFiles)
{
  #build a good name to assign to data
  nameToUse=str_sub(string=a,start=12,end=18)
  temp=read.table(a,header=TRUE,sep=",",stringsAsFactors=FALSE)
  #assign them into workspace
  assign(x=nameToUse,value=temp)
}
head(Aid_00s)

#readLines(): when the rows in a data files are not uniformly formatted
#step(1): Reading data
txt=readLines("data1.2.txt")
txt
#step(2)':Selecting lines containing data
I=grepl("^%",txt)
I
dat=txt[!I]
dat
#step(3):Split lines into separate fields
(fieldList=strsplit(dat,split=","))
#step(4):Standardize rows
assignFields=function(x){
  out=character(3)
  i=grepl("[[:alpha:]]",x)
  out[1]=x[i]
  i=which(as.numeric(x)<1890)
  out[2]=ifelse(length(i)>0,x[i],NA)
  i=which(as.numeric(x)>1890)
  out[3]=ifelse(length(i)>0,x[i],NA)
  out
}
standardFields=lapply(fieldList,assignFields) #apply a function over a list
standardFields
#step(5): transform a  list to data.frame
#copy into a matrix which is then coerced into a data.frame
M=matrix(unlist(standardFields),
         nrow=length(standardFields),
         byrow=TRUE)  
#unlist() produce a vector which contains all the atomic components which occur in x
colnames(M)=c("name","birth","death")
M
deltons=as.data.frame(M,stringsAsFactors=FALSE)
deltons
str(deltons)
#step(6):Normalize and coerce to correct types
deltons$birth=as.numeric(deltons$birth)
deltons$death=as.numeric(deltons$death)
deltons
str(deltons)
