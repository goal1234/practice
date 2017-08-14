  setwd("E:/±¨±í/ÔÓÆßÔÓ°Ë/¼õÃâ·ÖÎö/data")
  derate <- read.csv('¼õÃâ±ÈÀı.csv', stringsAsFactors = T)
  str(derate)
  derate$TRANDATE <- as.Date(derate$TRANDATE)
  derate <- na.omit(derate)
  
  
  summary(derate$derate_rate)
  library(fBasics)
  basicStats(derate$derate_rate)
  
  level <- c(0,0.03,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.8,1,1.2,1.5,2,20)
  count <- vector()
  len <- length(level)-1;print(len)
  derate$type <- NULL
  for(i in 1:len){
    cond <- derate$derate_rate > level[i] & derate$derate_rate < level[i+1]
    a <- sum(cond)
    print(paste('[',level[i], level[i+1], ']','----->',a))
    count <- c(count,a)
    derate$type[cond] <- i
  }
  
  library(dplyr)
  options(scipen = 10)
  by_type_count <- derate  %>% group_by(type) %>%
    summarise(lnsamt = sum(sum_lnsamt != 0),lnint = sum(sum_lnsint != 0), lnsperfee = sum(sum_lnsperfee !=0),
              lnsdefint = sum(sum_lnsdefint != 0), lnslatmt = sum(sum_lnslatamt != 0))
  
  by_type_value <- derate  %>% group_by(type) %>%
    summarise(lnsamt = mean(sum_lnsamt),lnint = mean(sum_lnsint), lnsperfee = mean(sum_lnsperfee),
              lnsdefint = mean(sum_lnsdefint), lnslatmt = mean(sum_lnslatamt))
  
  by_tyva <- derate  %>% group_by(prd, type) %>% 
    summarise(lnsamt = mean(sum_lnsamt),lnint = mean(sum_lnsint), lnsperfee = mean(sum_lnsperfee),
              lnsdefint = mean(sum_lnsdefint), lnslatmt = mean(sum_lnslatamt))
  by_tyva_max <- by_tyva[by_tyva$type == 14,]
  
  derate$month <- months(derate$TRANDATE)
  by_month <- derate  %>% group_by(month) %>%
    summarise(lnsamt = mean(sum_lnsamt),lnint = mean(sum_lnsint), lnsperfee = mean(sum_lnsperfee),
              lnsdefint = mean(sum_lnsdefint), lnslatmt = mean(sum_lnslatamt))
  
  library(ggplot2)
  ggplot(data = derate,aes(x = derate_rate, y = derate_all)) + geom_line()+geom_smooth()
  with(by_type_value, plot(type,lnsamt))
  with(by_type_value, plot(type,lnint))
  with(by_type_value, plot(type,lnsperfee, type = 'l'))
  
  with(by_type_value, plot(type,lnsdefint,ylim = c(1,100000)))
  with(by_type_value, plot(type,lnslatmt,ylim = c(1,10000)))
  
  write.csv(by_month, "by_month.csv")
  write.csv(by_type_count, "by_type_count.csv")
  write.csv(by_type_value,  "by_type_value.csv")
  write.csv(by_tyva, "by_tyva.csv")
  
  