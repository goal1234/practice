  #用的是原始数据这一页
  a <- read.delim("clipborad",stringsAsFactors = T)
  
  plot(a$当日剩余案件量, type = 'l')
  plot(a$联络客户数, type = 'l')
  plot(a$当日人均案件量, type = 'l')
  plot(a$尝试呼叫次数, type = 'l')
  plot(a$接通次数, type = 'l')
  plot(a$可联客户数, type = 'l')
  plot(a$有效联络客户数, type = 'l')
  plot(a$队列人数, type = 'l')
  
  connect_rate <- as.numeric(a$有效联络客户数)/as.numeric(a$联络客户数)
  plot(connect_rate, type = 'l')
  plot(density(connect_rate))
  t.test(connect_rate)
  
  get_rate <- as.numeric(a$接通次数)/as.numeric(a$尝试呼叫次数)
  plot(get_rate, type ='l')
  
  a$connect_rate <- connect_rate
  a$get_rate <- get_rate
  
  lm(a$connect_rate~.-get_rate,data = a[, -1])
  
  after <-  a[227:length(a$date),]
  before <- a[109:226,]
  
  library(corrplot)
  library(smbinning)
  corrplot(cor(a[,-1]))
  smbinning.eda(a[,-1])  
  
  par(mfrow = c(2,1))
    hist(before$联络客户数, col = 'lightblue', lwd = 1, breaks = c(100))
    hist(after$联络客户数, col = 2, lwd = 1, breaks = c(100))
    plot(density(before$联络客户数));plot(density(before$联络客户数))
  par(mfrow = c(1,1))
  
  t.test(before$联络客户数, after$联络客户数)  #<<<联络客户并无不同
  
  par(mfrow = c(2,1))
  hist(before$connect_rate, col = 'lightblue', lwd = 1, breaks = c(100))
  hist(after$connect_rate, col = 2, lwd = 1, breaks = c(100))
  plot(density(before$connect_rate));plot(density(before$connect_rate))
  par(mfrow = c(1,1))
  
  t.test(before$connect_rate, after$connect_rate)  #<<<connect_rate并无不同
  
  #####<<<<<<<<<<<<<前后下降是一致的>>>>>>>>>>>>>>>#####
  
  md_before <- lm(before$connect_rate~.-get_rate, data = before[, -1])
  md_after <- lm(after$connect_rate~.-get_rate, data = after[, -1])
  
  md_before$coefficients - md_after$coefficients
  
  barplot((md_before$coefficients - md_after$coefficients)[2:8])
  #<<<可联客户减少了
  
  lm(before$联络客户数~before$可联客户数, data = before)
  lm(after$联络客户数~after$可联客户数, data = after)
  
  boxplot(before$可联客户数/before$联络客户数)
  boxplot(after$可联客户数/after$联络客户数)
  #<<<之后连线的客户中可联的少了  
  
  
  plot(a$可联客户数, type = 'l') 
  flask <-   diff(a$可联客户数)/a$可联客户数[1:(length(a$可联客户数)-1)]    
  summary(flask)
  plot(flask, ylim = c(0,1), type = 'l')
  
  #===关注联络客户数和可联库户数===*
  ####<<<<可联客户的敏感性更高>>>>####
  
  plot(a$联络客户数, type = 'l')
  plot(a$可联客户数, type = 'l')
  
  t.test(a$联络客户数, a$可联客户数)
  
  plot(a$联络客户数/a$可联客户数)
  #<<<1个可联客户需要的联络次数出现上升，6-8
  
  library(ggplot2)
  ggplot(data = a, aes(x = a$联络客户数, y = a$可联客户数)) +geom_point() + stat_smooth(method = 'lm')  
  ggplot(data = a, aes(x = a$联络客户数, y = a$可联客户数)) +geom_point() + stat_smooth(method = 'loess')  
  
  ggplot(data = before, aes(x = before$联络客户数, y = before$可联客户数)) +geom_point()
  ggplot(data = after, aes(x = after$联络客户数, y = after$可联客户数)) +geom_point()
  
  ggplot(data = a, aes(x = a$联络客户数, y = a$可联客户数)) +geom_point(size = a$get_rate*10, alpha = 0.7)
  
  #弹性
  p <- a$connect_rate
  q <- a$联络客户数
  
  (q[1] - q[311])/(p[1] - p[311])*(p[311]/q[311])  #2.158086
  (q[311] - q[1])/q[1]    #0.6672489
  diff(q)/q[2:length(q)]
  
  
  
  p <- a$connect_rate
  q <- a$可联客户数
  diff(q)/q[2:length(q)]
  (q[1] - q[311])/(p[1] - p[311])*(p[311]/q[311])  #5.163335
  #<<<对于connect_rate可联客户的弹性为5.16，联络客户为2.15
  (q[311] - q[1])/q[1]    #0.8275168
  
  
  q <- after$联络客户数
  (q[85] - q[1])/q[1]
  q <- after$可联客户数
  (q[85] - q[1])/q[1]
  
  q <- before$联络客户数
  (q[83] - q[1])/q[1]
  q <- before$可联客户数
  (q[83] - q[1])/q[1]
  
  
  q <- a$联络客户数
  pert_connect <- diff(q)/q[2:length(q)]
  
  q <- a$可联客户数
  pert_avail <- diff(q)/q[2:length(q)]
  
  what <- data.frame(pert_connect = pert_connect, pert_avail = pert_avail)
  matplot(what, ylim = c(0,1))  
  
  a$date <- as.Date(a$date)
  
  a4 <- a[a$date < "2016-05-1",]
  a5 <- a[a$date <"2016-06-1" & a$date > "2016-05-01",]
  a6 <- a[a$date <"2016-07-1" & a$date > "2016-06-01",]
  a7 <- a[a$date <"2016-08-1" & a$date > "2016-07-01",]
  a8 <- a[a$date <"2016-09-1" & a$date > "2016-08-01",]
  a9 <- a[a$date <"2016-10-1" & a$date > "2016-09-01",]
  a10 <- a[a$date <"2016-11-1" & a$date > "2016-10-01",]
  a11 <- a[a$date <"2016-12-1" & a$date > "2016-11-01",]
  a12 <- a[a$date <"2017-01-1" & a$date > "2016-12-01",]
  
  a4_17 <- a[a$date <"2017-05-1" & a$date > "2017-04-01",]
  a5_17 <- a[a$date <"2017-06-1" & a$date > "2017-05-01",]
  a6_17 <- a[a$date <"2017-07-1" & a$date > "2017-06-01",]
  
  quick <- function(x){
    i <- nrow(x)
    q <- x$联络客户数
    print((q[i] - q[1])/q[1])
    
    q <- x$可联客户数
    print((q[i] - q[1])/q[1])
    print(rep("i",10))
  
  }
  
  quick(a4)
  quick(a5)
  quick(a6)
  quick(a7)
  quick(a8)
  quick(a9)
  quick(a10)
  quick(a11)
  quick(a12)
  
  quick(a4_17)
  quick(a5_17)
  quick(a6_17)
  
        