setwd("/Users/zhangbeibei/Documents/Teaching/备课_2015/2015大数据统计基础/大数据-数据预处理/Data and Code for Course (2016)")
library(dplyr) 
packageVersion("dplyr")

# RStudio's CRAN download log from July 8, 2014,
# which contains information on roughly
# 225,000 R package downloads (http://cran-logs.rstudio.com/)

mydf <- read.csv("cran2014-07-08.csv",stringsAsFactors = FALSE)
cran<-tbl_df(mydf)
rm("mydf") 
#to avoid confusion, remove original data frame from your space
?tbl_df
#"The main advantage to using a tbl_df() over a
# regular data frame is the printing.
cran #more informative and compact than original data frame(mydf)

#####################################################################
###################Data Manipulation with "dplyr"##########################
#####################################################################

#(1)select(): select a subset of columns
?select
select(cran,ip_id,package,country) 
#keep only variables you mention, in order we specified
select(cran, r_arch:country)
select(cran, country:r_arch)
select(cran,-time)
-5:20
-(5:20)
select(cran,-(X:size))

#(2)filter(): select a subset of rows
?filter
filter(cran, package == "swirl") 
#without having to explicitly specify cran$package
filter(cran, r_version == "3.1.1", country == "US")
#you can specify as many conditions as you want
#return all rows of cran corresponding to downloads from users in the US running R version 3.1.1
filter(cran, r_version <= "3.0.2", country == "IN")
filter(cran, country == "US" | country == "IN")
filter(cran, size > 100500, r_os =="linux-gnu")
filter(cran, !is.na(r_version))

#(3)arrange():order the rows of a dataset
cran2 <- select(cran, size:ip_id)
cran2
arrange(cran2, ip_id) #smallest ip_id to largest ip_id
arrange(cran2, desc(ip_id))
arrange(cran2, package, ip_id)
cran3<-select(cran,ip_id,package,size)
cran3

#(4)mutate(): create a new variable based on the value of one or more variable
#add a column named "size_mb" = size/2^20
#1KB=1024B, 1MB=1024KB, 1GB=1024MB, 1TB=1024GB
mutate(cran3, size_mb = size / 2^20)
mutate(cran3, size_mb = size / 2^20, size_gb = size_mb /2^10)
mutate(cran3, correct_size =size +1000)

#(5)summarize()
summarize(cran, avg_bytes=mean(size))

#####################################################################
###############Grouping and Chaining with "dplyr"########################
#####################################################################

#group_by()
by_package <- group_by(cran, package) 
by_package
# Everything else looks the same, except "Groups: package"
# any operation we apply to grouped data will take place on 
# a per package basis
summarize(by_package, mean(size)) #return mean size for each package 

pack_sum <- summarize(by_package,
                      count = n(),
                      unique = n_distinct(ip_id),
                      countries = n_distinct(country),
                      avg_bytes = mean(size))
# we'd like to know which packages were most popular
# on the day these data were collected (July 8, 2014)
quantile(pack_sum$count, probs = 0.99)
top_counts <- filter(pack_sum, count>679)
top_counts #only show first 10 rows
View(top_counts)
top_counts_sorted <- arrange(top_counts, desc(count))
View(top_counts_sorted)
#| Perhaps we're more interested in the number of *unique*
#| downloads on this particular day. In other words, if a package
#| is downloaded ten times in one day from the same computer, we
#| may wish to count that as only one download. That's what the
#| 'unique' column will tell us.
quantile(pack_sum$unique, probs = 0.99)
top_unique<-filter(pack_sum,unique>465)
View(top_unique)
top_unique_sorted<-arrange(top_unique,desc(unique))
View(top_unique_sorted)

#Popularity: number of distinct countries from which each package was downloaded.
#每个包被多少个不同的国家下载
# 'chaining' ('piping'): string together multiple function calls
#in a compact and readable way

by_package <- group_by(cran, package)
pack_sum <- summarize(by_package,
                      count = n(),
                      unique = n_distinct(ip_id),
                      countries = n_distinct(country),
                      avg_bytes = mean(size))

top_countries <- filter(pack_sum, countries > 60)
result1 <- arrange(top_countries, desc(countries), avg_bytes)
print(result1)

result2 <-
  arrange(
    filter(
      summarize(
        group_by(cran,
                 package
        ),
        count = n(),
        unique = n_distinct(ip_id),
        countries = n_distinct(country),
        avg_bytes = mean(size)
      ),
      countries > 60
    ),
    desc(countries),
    avg_bytes
  )

print(result2)
#much less readable

result3 <-
  cran %>%
  group_by(package) %>%
  summarize(count = n(),
            unique = n_distinct(ip_id),
            countries = n_distinct(country),
            avg_bytes = mean(size)
  ) %>%
  filter(countries > 60) %>%
  arrange(desc(countries), avg_bytes)

# Print result to console
print(result3)
View(result3)

cran %>%
  select(ip_id,country,package,size) %>%
  print

cran %>%
  select(ip_id, country, package, size) %>%
  mutate(size_mb=size/2^20)

cran %>%
  select(ip_id, country, package, size) %>%
  mutate(size_mb = size / 2^20) %>%
  filter(size_mb <= 0.5) %>%
  print

