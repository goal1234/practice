setwd("E:/报表/杂七杂八/减免分析/data")

derate <- read.csv('减免比例.csv', stringsAsFactors = T)
str(derate)
derate[is.na(derate)]<-0

year1 <- sapply(derate$derate_date, function(x)strsplit(as.character(x), "-")[[1]][1])
month1 <- sapply(derate$derate_date, function(x)strsplit(as.character(x), "-")[[1]][2])
day1 <- sapply(derate$derate_date, function(x)strsplit(as.character(x), "-")[[1]][3])
derate$ym <- paste(year1,month1)
derate$y <- year1
derate$m <- month1
derate$d <- day1
rm(year1, month1, day1)

derate <- derate[!duplicated(derate),]
derate <- derate[derate$derate_rate < 3,]

summary(derate$derate_rate)
library(fBasics); options(scipen = 10)
basicStats(derate$derate_rate)

level <- c(0, 0.5, 0.8, 1, 1.2, 80)
count <- vector()
len <- length(level)-1;print(len)
derate$type <- NULL
for(i in 1:len){
  cond <- derate$derate_rate > level[i] & derate$derate_rate <= level[i+1]
  a <- sum(cond)
  print(paste('[',level[i], level[i+1], ']','----->',a))
  count <- c(count,a)
  derate$type[cond] <- i
}


derate_copy <- derate
derate <- subset(derate,is_clear == "0")
derate <- subset(derate, outsource == "0")


city <- derate
#所有总计
aa0 <- city %>%　group_by(y,m) %>% 
  summarise(len = length(type), derate = sum(derate_all)) %>% mutate(ym = paste(y, m))

#所有的还款
aa1 <- city %>% group_by(y, m,type) %>%
  summarise(len = length(type), all_derate = sum(derate_all)) %>%
  mutate(ym = paste(y, m))

aa2 <- left_join(aa1[,c(6:1)],aa0[,c(5:3)], by = 'ym') %>% mutate(derate_rate = all_derate/derate, len = len.x/len.y)

c1 <- subset(aa2, type == 1)
c2 <- subset(aa2, type == 2)
c3 <- subset(aa2, type == 3)
c4 <- subset(aa2, type == 4)  
c5 <- subset(aa2, type == 5)

c <- left_join(c4[,c(1,9,10)], c3[,c(1,9,10)], by = 'ym')
c <- left_join(c, c1[,c(1, 9,10)], by = 'ym')
c <- left_join(c, c2[,c(1, 9,10)], by = 'ym')
c <- left_join(c, c5[,c(1, 9,10)], by = 'ym')
colnames(c) <- c("ym","derate_rate_4", "len_4","derate_rate_3", "len_3","derate_rate_1",
                 "len_1", "derate_rate_2", "len_2", "derate_rate_5", "len_5")

c <- c[, c(1,7,9,5,3,11,6,8,4,2,10)]
c[is.na(c)] <- 0
write.csv(c, 'outsource_no.csv')



