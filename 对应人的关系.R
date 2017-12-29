  setwd('E:/报表/杂七杂八/减免分析/data')
  filename <- "a_test_10day.csv"
  
  datasource <- read.csv(filename, stringsAsFactors = T)
  datasource <- datasource[!is.na(datasource$derate_rate),]
  
  clear <- subset(datasource, norm == 0) 
  
  result <- with(clear, cut(derate_rate, breaks =  c(0,0.5,1,1.05,1.2,Inf)))  
  result_prin <- with(clear, cut(derate_prin, breaks = c(0,0.5,1,1.05,1.2,Inf)))
  table(result)
  prop.table(table(result))
  table(result_prin)
  prop.table(table(result_prin))
  
  #给dpd进行这个几张表

get <- function(x){
  period <- subset(clear, dpd %in% x)
  result <- with(period, cut(derate_rate, 
                             breaks =  c(0,0.5,1,1.05,1.2,Inf)))  
  result_prin <- with(period, cut(derate_prin, 
                                  breaks = c(0,0.5,1,1.05,1.2,Inf)))
  
  original <- table(result)
  change <- table(result_prin)
  
  original_p <- prop.table(original)
  change_p <- prop.table(change)
  len <- nrow(period)
  
  result <- list(
    len = len,
    ori =original,
    change = change,
    ori.p = original_p,
    change_p = change_p
  )
  
  return(result)
  
}

  range <- list(c(1),c(2:3),c(4:6),c(7:200))
  len <- length(range)
  for(i in len){
    result <- get(range[i])
  }
  
  #简单点
  result <- lapply(range,get)
  for(i in 1:4){
    print(result[[i]][2])
  }
  
  
  #----------------------------------------------#
  clear <- clear[order(clear$derate_prin),]
  clear$derate_prin[3]  #0.5
  clear$derate_prin[1474]  #1
  clear$derate_prin[2358]  #1.3
  clear$derate_prin[3340]  #1.5
  clear$derate_prin[5374]
  
  result_prin <- with(clear, cut(derate_prin, breaks = c(0, 0.5, 1, 1.3, 1.5, Inf)))
  table(result_prin)
  prop.table(table(result_prin))
  
  
  get_prin <- function(x){
    period <- subset(clear, dpd %in% x)
    result <- with(period, cut(derate_rate, 
                               breaks =  c(0, 0.5, 1, 1.05, 1.2, Inf)))  
    result_prin <- with(period, cut(derate_prin, 
                                    breaks = c(0, 0.5, 1, 1.3, 1.5, Inf)))
    
    original <- table(result)
    change <- table(result_prin)
    
    original_p <- prop.table(original)
    change_p <- prop.table(change)
    len <- nrow(period)
    
    result <- list(
      len = len,
      ori =original,
      change = change,
      ori.p = original_p,
      change_p = change_p
    )
    
    return(result)
    
  }
  
  result <- lapply(range, get_prin)
  result[[1]]
  result[[2]]
  result[[3]]
  result[[4]]
  
  
  
  
  
  period1 <- subset(clear, dpd %in% c(7))
  period2 <- subset(clear, dpd %in% c(2,3))
  period3 <- subset(clear, dpd %in% c(4:6))
  period4 <- subset(clear, dpd %in% c(7:Inf))
  
  
  ##################################################################
  ################################################################
  
  #---对所有的进行切割---#
  setwd('E:/报表/杂七杂八/减免分析/data')
  filename <- "a_test_10day.csv"
  
  datasource <- read.csv(filename, stringsAsFactors = T)
  datasource <- datasource[!is.na(datasource$derate_rate),]
  
  clear <- subset(datasource, norm == 0) 
  
  
  clear$result_prin <- with(clear, cut(derate_rate, 
                                       breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2,
                                                  1.3, 1.4, 1.5, 1.6, Inf)))
  
  library(dplyr)
  
  clear$ddpd <-   
    ifelse(clear$dpd == 1, 'M1',
           ifelse(clear$dpd %in% c(2:3),'M2-M3',
                  ifelse(clear$dpd %in% c(4:6), 'M4-M6',
                         'M7+')))
  
  
  mig_clear <- aggregate(clear$CONTRACTNO,by = list(dpd = clear$ddpd, prin = clear$result_prin), 
                         FUN = length)
  all <- nrow(clear)
  mig_clear$count <- mig_clear$x/all
  derate <- mig_clear
  
  write.csv(mig_clear, '原有口径.csv')
  
  #---变为本金口径之后的情况---#
  setwd('E:/报表/杂七杂八/减免分析/data')
  filename <- "a_test_10day.csv"
  
  datasource <- read.csv(filename, stringsAsFactors = T)
  datasource <- datasource[!is.na(datasource$derate_rate),]
  
  clear <- subset(datasource, norm == 0) 
  
  
  clear$result_prin <- with(clear, cut(derate_prin, 
                                 breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2,
                                            1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, Inf)))
  
  library(dplyr)
  
  clear$ddpd <-   
    ifelse(clear$dpd == 1, 'M1',
           ifelse(clear$dpd %in% c(2:3),'M2-M3',
                  ifelse(clear$dpd %in% c(4:6), 'M4-M6',
                          'M7+')))
  
    
  mig_clear <- aggregate(clear$CONTRACTNO,by = list(dpd = clear$ddpd, prin = clear$result_prin), 
                         FUN = length)
  all <- nrow(clear)
  mig_clear$count <- mig_clear$x/all
  
  derate_prin <- mig_clear
  
  derate$prin_change <- derate_prin$prin
  derate$x_change <- derate_prin$x
  derate$count_change <- derate_prin$count*100
  derate$count <- derate$count*100
  
  derate$dif <- derate$count - derate$count_change


  barplot(derate$dif,col = 'lightgreen',border = 'black')
  View(derate)
  
  write.csv(derate_prin, '本金口径.csv')
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  