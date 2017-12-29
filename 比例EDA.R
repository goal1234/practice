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
  derate_copy <- derate
  derate <- subset(derate,is_clear == "0")
  
  
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
  
  library(dplyr)
  #城市个人信用贷款每天减免的变化#
  city <- subset(derate, prd == "城市个人信用贷款")
  
  #所有总计
  aa0 <- city %>%　group_by(y,m,d) %>% 
    summarise(len = length(type)) %>% mutate(ym = paste(y, m, d))
  
  #所有的还款
  aa1 <- city %>% group_by(y, m, d, type) %>%
    summarise(all_derate = sum(derate_all)) %>%
    mutate(ym = paste(y, m, d))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #------分组------#
  library(dplyr)
  by_type_count <- derate  %>% group_by(type) %>%
    summarise(lnsamt = sum(sum_lnsamt != 0),lnint = sum(sum_lnsint != 0), lnsperfee = sum(sum_lnsperfee !=0),
              lnsdefint = sum(sum_lnsdefint != 0), lnslatmt = sum(sum_lnslatamt != 0))
  
  by_type_value <- derate  %>% group_by(type) %>%
    summarise(lnsamt = sum(sum_lnsamt),lnint = sum(sum_lnsint), lnsperfee = sum(sum_lnsperfee),
              lnsdefint = sum(sum_lnsdefint), lnslatmt = sum(sum_lnslatamt), all_derate = sum(derate_all))
  
  by_tyva <- derate  %>% group_by(prd, type) %>% 
    summarise(lnsamt = sum(sum_lnsamt),lnint = sum(sum_lnsint), lnsperfee = sum(sum_lnsperfee),
              lnsdefint = sum(sum_lnsdefint), lnslatmt = sum(sum_lnslatamt), all_derate = sum(derate_all))
  
  by_month <- derate  %>% group_by(y, m, type) %>%
    summarise(lnsamt_sum = sum(sum_lnsamt), lnint = sum(sum_lnsint),lnsperfee = sum(sum_lnsperfee), 
              lnsdefint = sum(sum_lnsdefint), lnslatmt = sum(sum_lnslatamt),len = length(type),
              all_derate = sum(derate_all), derate_rate = sum(derate_rate)) %>%
    mutate(ym = paste(y,m))
  
  by_month_1 <- derate  %>% group_by(y, m, type) %>%
    summarise(lnsamt_sum = sum(sum_lnsamt), lnint = sum(sum_lnsint),lnsperfee = sum(sum_lnsperfee), 
              lnsdefint = sum(sum_lnsdefint), lnslatmt = sum(sum_lnslatamt),len = length(type),
              all_derate = sum(derate_all), derate_rate = sum(derate_rate)) %>%
    mutate(ym = paste(y,m), avg = derate_rate/len)
  
  by_month_2 <- derate  %>% group_by(y, m) %>%
    summarise(lnsamt_sum = sum(sum_lnsamt), lnint = sum(sum_lnsint),lnsperfee = sum(sum_lnsperfee), 
              lnsdefint = sum(sum_lnsdefint), lnslatmt = sum(sum_lnslatamt),len = length(type),
              all_derate = sum(derate_all), derate_rate = sum(derate_rate)) %>%
    mutate(ym = paste(y,m), avg = derate_rate/len)
  
  by_clear <- derate_copy %>% group_by(y,m) %>% summarise(clear = sum(is_clear == 1)/length(is_clear))
  
  
  wm <- left_join(by_month[,c(12,1:11)], by_month_2[,c(11,8,9)], by = "ym")
  wm$weight_len <-  wm$len.x/wm$len.y
  #wm$weightmean <- wm$weight*wm$derate_rate
  wm$weight_derate <- wm$all_derate.x/wm$all_derate.y
  wm$derate_rate1 <- wm$derate_rate/wm$len.x
  
  by_month_2 <- wm %>% group_by(ym) %>% summarise(wm = sum(weightmean))
  
  a1 <- subset(wm, type == 1)[,c(1, 15, 16, 17)]
  colnames(a1) <- c("ym", "len1", 'derate1', 'rate1')
  a2 <- subset(wm, type == 2)[,c(1, 15, 16, 17)]
  colnames(a2) <- c("ym", "len2", 'derate2', 'rate2')
  a3 <- subset(wm, type == 3)[,c(1, 15, 16, 17)]
  colnames(a3) <- c("ym", "len3", 'derate3', 'rate3')
  a4 <- subset(wm, type == 4)[,c(1, 15, 16, 17)]
  colnames(a4) <- c("ym", "len4", 'derate4', 'rate4')
  a5 <- subset(wm, type == 5)[,c(1, 15, 16, 17)]
  colnames(a5) <- c("ym", "len5", 'derate5', 'rate5')
  
  b1 <- left_join(a3, a1, by = "ym")
  b1 <- left_join(b1,a2,by = "ym")
  b1 <- left_join(b1,a4,by = "ym")
  b1 <- left_join(b1,a5,by = "ym")
  b1[is.na(b1)] <- 0
  write.csv(b1, "clear.csv")
  aa1 <- wm[wm$ym == "2016 09", ]
  
  library(ggplot2)
  
  with(by_type_value, plot(type,lnsdefint,ylim = c(1,100000)))
  with(by_type_value, plot(type,lnslatmt,ylim = c(1,10000)))
  
  write.csv(by_month_1, "by_month.csv")
  write.csv(by_type_count, "by_type_count.csv")
  write.csv(by_type_value,  "by_type_value.csv")
  write.csv(by_tyva, "by_tyva.csv")
  write.csv(wm,"wm.csv")
  
  ggplot(wm,aes(x = 1:length(derate_rate), y = derate_rate))+geom_line()+facet_grid(.~type)
  ggplot(wm,aes(x = 1:length(len.x), y = len.x))+geom_line()+facet_grid(.~type)
  
  wm %>% group_by(ym) %>%　ggplot(.,aes(x =ym, y = weight_len,fill = as.factor(type))) + geom_area()
  