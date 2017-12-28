setwd('F:/yrd')
file_name <- list.files('F:/yrd',pattern = ".xlsx")

library(xlsx)
library(stringr)

#读入文件
a <-read.xlsx(file_name[1],1)  

  #截取日期
date <- substr(file_name[1], nchar(file_name[1])-19, nchar(file_name[1])-12)
date1 <- paste(substr(date,1,4),'/',substr(date,5,6),'/',substr(date,7,8),sep = "")

#加入日期
a$date <- as.Date(date1)
a <- a[,c('CLIENT_ID','OVERDUE_PERIOD_NUM','date')]

##===累计文件===##
for(i in c(2:length(file_name))){
  print(i)
  
  #读入文件
  b <-read.xlsx(file_name[i],1)  
  
  #截取日期
  date <- substr(file_name[i], nchar(file_name[i])-19, nchar(file_name[i])-12)
  date1 <- paste(substr(date,1,4),'/',substr(date,5,6),'/',substr(date,7,8),sep = "")
  

  #加入日期
  b$date <- as.Date(date1)
  b <- b[,c('CLIENT_ID','OVERDUE_PERIOD_NUM','date')]
  colnames(a) <- colnames(b)
  a <<- rbind(a,b)
}

job <- read.delim('clipboard')
job$contractno <- as.character(job$contractno)
str(job)
job$date <- as.Date(as.character(job$date))

library(sqldf)

result <- 
sqldf('select a.*,
              b.OVERDUE_PERIOD_NUM
      from job as a
      left join a as b on b.CLIENT_ID = a.contractno and 
      a.date = b.date')

result_all <- 
  sqldf('select a.*,
        b.OVERDUE_PERIOD_NUM
        from job as a
        left join a as b on b.CLIENT_ID = a.contractno
        ')