  setwd("E:/±¨±í/ÔÓÆßÔÓ°Ë/¼õÃâ·ÖÎö/data")
  
  derate <- read.csv('¼õÃâ±ÈÀı.csv', stringsAsFactors = T)
  str(derate)
  derate[is.na(derate)]<-0
  
  year1 <- sapply(derate$derate_date, function(x)strsplit(as.character(x), "-")[[1]][1])
  month1 <- sapply(derate$derate_date, function(x)strsplit(as.character(x), "-")[[1]][2])
  derate$ym <- paste(year1,month1)
  derate$y <- year1
  derate$m <- month1
  rm(year1,month1)
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
  
  #------·Ö×é------#
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
              lnsdefint = sum(sum_lnsdefint), lnslatmt = sum(sum_lnslatamt),len = length(sum_lnslatamt),
              all_derate = sum(derate_all), derate_rate = sum(derate_rate)) %>%
    mutate(ym = paste(y,m))
  
  by_month_1 <- derate  %>% group_by(y, m, type) %>%
    summarise(lnsamt_sum = sum(sum_lnsamt), lnint = sum(sum_lnsint),lnsperfee = sum(sum_lnsperfee), 
              lnsdefint = sum(sum_lnsdefint), lnslatmt = sum(sum_lnslatamt),len = length(sum_lnslatamt),
              all_derate = sum(derate_all), derate_rate = sum(derate_rate)) %>%
    mutate(ym = paste(y,m), avg = derate_rate/len)
  
  by_month_2 <- derate  %>% group_by(y, m) %>%
    summarise(lnsamt_sum = sum(sum_lnsamt), lnint = sum(sum_lnsint),lnsperfee = sum(sum_lnsperfee), 
              lnsdefint = sum(sum_lnsdefint), lnslatmt = sum(sum_lnslatamt),len = length(sum_lnslatamt),
              all_derate = sum(derate_all), derate_rate = sum(derate_rate)) %>%
    mutate(ym = paste(y,m), avg = derate_rate/len)
  
  
  
  
  wm <- left_join(by_month[,c(12,1:11)], by_month_2[,c(11,8,9)], by = "ym")
  wm$weight_len <-  wm$len.x/wm$len.y
  #wm$weightmean <- wm$weight*wm$derate_rate
  wm$weight_derate <- wm$all_derate.x/wm$all_derate.y
  by_month_2 <- wm %>% group_by(ym) %>% summarise(wm = sum(weightmean))
  
  
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
  
  wm %>% group_by(ym) %>%¡¡ggplot(.,aes(x =ym, y = weight_len,fill = as.factor(type))) + geom_area()
  