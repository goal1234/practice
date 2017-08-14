  #�õ���ԭʼ������һҳ
  a <- read.delim("clipborad",stringsAsFactors = T)
  
  plot(a$����ʣ�స����, type = 'l')
  plot(a$����ͻ���, type = 'l')
  plot(a$�����˾�������, type = 'l')
  plot(a$���Ժ��д���, type = 'l')
  plot(a$��ͨ����, type = 'l')
  plot(a$�����ͻ���, type = 'l')
  plot(a$��Ч����ͻ���, type = 'l')
  plot(a$��������, type = 'l')
  
  connect_rate <- as.numeric(a$��Ч����ͻ���)/as.numeric(a$����ͻ���)
  plot(connect_rate, type = 'l')
  plot(density(connect_rate))
  t.test(connect_rate)
  
  get_rate <- as.numeric(a$��ͨ����)/as.numeric(a$���Ժ��д���)
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
    hist(before$����ͻ���, col = 'lightblue', lwd = 1, breaks = c(100))
    hist(after$����ͻ���, col = 2, lwd = 1, breaks = c(100))
    plot(density(before$����ͻ���));plot(density(before$����ͻ���))
  par(mfrow = c(1,1))
  
  t.test(before$����ͻ���, after$����ͻ���)  #<<<����ͻ����޲�ͬ
  
  par(mfrow = c(2,1))
  hist(before$connect_rate, col = 'lightblue', lwd = 1, breaks = c(100))
  hist(after$connect_rate, col = 2, lwd = 1, breaks = c(100))
  plot(density(before$connect_rate));plot(density(before$connect_rate))
  par(mfrow = c(1,1))
  
  t.test(before$connect_rate, after$connect_rate)  #<<<connect_rate���޲�ͬ
  
  #####<<<<<<<<<<<<<ǰ���½���һ�µ�>>>>>>>>>>>>>>>#####
  
  md_before <- lm(before$connect_rate~.-get_rate, data = before[, -1])
  md_after <- lm(after$connect_rate~.-get_rate, data = after[, -1])
  
  md_before$coefficients - md_after$coefficients
  
  barplot((md_before$coefficients - md_after$coefficients)[2:8])
  #<<<�����ͻ�������
  
  lm(before$����ͻ���~before$�����ͻ���, data = before)
  lm(after$����ͻ���~after$�����ͻ���, data = after)
  
  boxplot(before$�����ͻ���/before$����ͻ���)
  boxplot(after$�����ͻ���/after$����ͻ���)
  #<<<֮�����ߵĿͻ��п���������  
  
  
  plot(a$�����ͻ���, type = 'l') 
  flask <-   diff(a$�����ͻ���)/a$�����ͻ���[1:(length(a$�����ͻ���)-1)]    
  summary(flask)
  plot(flask, ylim = c(0,1), type = 'l')
  
  #===��ע����ͻ����Ϳ����⻧��===*
  ####<<<<�����ͻ��������Ը���>>>>####
  
  plot(a$����ͻ���, type = 'l')
  plot(a$�����ͻ���, type = 'l')
  
  t.test(a$����ͻ���, a$�����ͻ���)
  
  plot(a$����ͻ���/a$�����ͻ���)
  #<<<1�������ͻ���Ҫ�������������������6-8
  
  library(ggplot2)
  ggplot(data = a, aes(x = a$����ͻ���, y = a$�����ͻ���)) +geom_point() + stat_smooth(method = 'lm')  
  ggplot(data = a, aes(x = a$����ͻ���, y = a$�����ͻ���)) +geom_point() + stat_smooth(method = 'loess')  
  
  ggplot(data = before, aes(x = before$����ͻ���, y = before$�����ͻ���)) +geom_point()
  ggplot(data = after, aes(x = after$����ͻ���, y = after$�����ͻ���)) +geom_point()
  
  ggplot(data = a, aes(x = a$����ͻ���, y = a$�����ͻ���)) +geom_point(size = a$get_rate*10, alpha = 0.7)
  
  #����
  p <- a$connect_rate
  q <- a$����ͻ���
  
  (q[1] - q[311])/(p[1] - p[311])*(p[311]/q[311])  #2.158086
  (q[311] - q[1])/q[1]    #0.6672489
  diff(q)/q[2:length(q)]
  
  
  
  p <- a$connect_rate
  q <- a$�����ͻ���
  diff(q)/q[2:length(q)]
  (q[1] - q[311])/(p[1] - p[311])*(p[311]/q[311])  #5.163335
  #<<<����connect_rate�����ͻ��ĵ���Ϊ5.16������ͻ�Ϊ2.15
  (q[311] - q[1])/q[1]    #0.8275168
  
  
  q <- after$����ͻ���
  (q[85] - q[1])/q[1]
  q <- after$�����ͻ���
  (q[85] - q[1])/q[1]
  
  q <- before$����ͻ���
  (q[83] - q[1])/q[1]
  q <- before$�����ͻ���
  (q[83] - q[1])/q[1]
  
  
  q <- a$����ͻ���
  pert_connect <- diff(q)/q[2:length(q)]
  
  q <- a$�����ͻ���
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
    q <- x$����ͻ���
    print((q[i] - q[1])/q[1])
    
    q <- x$�����ͻ���
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
  
        