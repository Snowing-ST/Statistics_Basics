setwd("/Users/zhangbeibei/Documents/Teaching/备课_2015/2015大数据统计基础/大数据-数据预处理/大数据预处理-授课2017/Data and Code 2017")
library(reshape2)
library(plyr) 
library(stringr) #for common string operations
######################################################################
########################Tidy data(Hadley Wickham)###############
######################################################################
#(1)Column headers are values, not variable names
pew <- read.delim(file = "pew.txt",
                  header = TRUE,
                  stringsAsFactors = FALSE,
                  check.names = F)
pew
#What are variables in the data?How many?
pew_tidy <- melt(data = pew,
                 id.vars = "religion",
                 variable.name="income",
                 value.name="frequency")
head(pew_tidy,15)


#(2)Multiple Variable in one column
tb <- read.csv(file = "tb.csv",
               header = TRUE, 
               stringsAsFactors = FALSE)
head(tb)
names(tb)
tb$new_sp = NULL  #clean up column names
names(tb)
names(tb) <- gsub("new_sp_", "", names(tb))
names(tb)
head(tb)
# Use na.rm = TRUE to remove missing observations
tb_tidy <- melt(
  data = tb,
  id = c("iso2", "year"),
  variable.name = "gender_age",
  value.name = "cases",
  na.rm = TRUE)
head(tb_tidy)
# na.rm = TRUE is useful if the missings don't have any meaning
# Often a good idea to ensure the rows are ordered
# by the variables
tidy = arrange(tb_tidy, iso2, gender_age, year)
head(tidy)
str_sub(tidy$gender_age, 1, 1) 
#str_sub() extracts substrings from a character vector
#str_sub(string=,start=,end=)
str_sub(tidy$gender_age, 2)
ageraw=str_sub(tidy$gender_age, 2)
head(ageraw)
agemap= c("04" = "0-4", "514" = "5-14",
          "014" = "0-14", "1524" = "15-24", "2534" = "25-34",
          "3544" = "35-44", "4554" = "45-54", "5564" = "55-64",
          "65"= "65+", "u" = NA)
age=agemap[ageraw]
head(age)
age=unname(age)
age
#age=revalue(ageraw,agemap)
#Given a map relation, replace specified values with new values in a factor or character vector
tidy$sex <- str_sub(tidy$gender_age, 1, 1)
tidy$age <- factor(age)
head(tidy)
tidy <- tidy[c("iso2", "year", "sex", "age", "cases")]
head(tidy,5)
#(3)Variables in rows and columns
weather <- read.delim(
  file = "weather.txt",
  stringsAsFactors = FALSE)
head(weather)
raw1=melt(weather,
          id.vars=c("id","year","month","element"),
          na.rm = TRUE, 
          variable.name="day",
          value.name = "temperature")
head(raw1)
raw1$day <- as.integer(str_replace(raw1$day, "d", ""))
raw1$element <- tolower(raw1$element)
names(raw1)
raw1 <- raw1[c("id", "year", "month", "day",
               "element", "temperature")]
raw1 <- arrange(raw1, year, month, day, element)
head(raw1)
dcast(raw1,id+year+month+day~element,
      value.var="temperature")

