  setwd('E:/报表/杂七杂八/致命QQ/data')
  roll <- read.csv('roll_status.csv',stringsAsFactors = T)
  roll[is.na(roll)] <- 0
  
  M2_qqemail <- read.csv('M2_qqemail.csv',stringsAsFactors = T)
  
  roll <- roll[roll$APPLY_ID %in% M2_qqemail$APPLY_ID,]
  
  product <- read.csv('product.csv',stringsAsFactors = T)
  
  roll <- merge(roll,product,by='APPLY_ID')
 
  roll$qq.y <- NULL
  roll$online.y <- NULL
  roll$i_user_id <-NULL
  colnames(roll)[2] <-c('qq')
  
  roll <- roll[roll$product_model=='极速模式',]
  #
  qq <- roll[roll$qq == 1,] 
  not_qq <- roll[roll$qq == 0,]
  
  nrow(qq);nrow(not_qq)
  table(qq$city_desc)
  table(not_qq$city_desc)
  
  summary(qq$rtp);barplot(qq$rtp)
  summary(not_qq$rtp);barplot(not_qq$rtp)
  t.test(qq$rtp,not_qq$rtp)
  par(mfrow = c(2,2))
  hist(na.omit(qq$rtp));hist(na.omit(not_qq$rtp))
  plot(density(na.omit(qq$rtp)));plot(density(na.omit(qq$rtp)))
  
  
  for(i in c(6:18)){
  print(summary(qq[,i]));
  print(summary(not_qq[,i]));
  t.test(qq[,i],not_qq[,i])
  par(mfrow = c(2,2))
  hist(na.omit(qq[,i]),main = colnames(qq)[i]);hist(na.omit(not_qq[,i]),main = colnames(not_qq)[i])
  plot(density(na.omit(qq[,i])),main = colnames(qq)[i]);plot(density(na.omit(not_qq[,i])),main = colnames(not_qq)[i])
  }
  
  money_qq <- qq$balance_prin0517+qq$balance_prin0417+qq$balance_prin0317+qq$balance_prin0217+qq$balance_prin0117+qq$balance_prin1216
  money_not_qq <- not_qq$balance_prin0517 + not_qq$balance_prin0417+not_qq$balance_prin0317+not_qq$balance_prin0217+not_qq$balance_prin0117+not_qq$balance_prin1216
  hist(na.omit(money_qq),main = 'qq金额直方图',xlab='金额',ylab='频数',col=heat.colors(1));
  hist(na.omit(money_not_qq),main = '非qq金额直方图',xlab='金额',ylab='频数',col='lightblue')
  plot(density(na.omit(money_qq)),main = 'qq金额概率密度图');
  plot(density(na.omit(money_not_qq)),main = '非qq金额概率密度图')
  summary(money_qq);summary(money_not_qq)
  library(fBasics)
  basicStats(money_qq);basicStats(money_not_qq)
  
  prin00 <- sum(qq$balance_prin1216,na.rm = T)
  prin01 <- sum(qq$balance_prin0117,na.rm = T)
  prin02 <- sum(qq$balance_prin0217,na.rm = T)
  prin03 <- sum(qq$balance_prin0317,na.rm = T)
  prin04 <- sum(qq$balance_prin0417,na.rm = T)
  prin05 <- sum(qq$balance_prin0517,na.rm = T)
  
  qq_prin <- c(prin00,prin01,prin02,prin03,prin04,prin05)
  
  prin00 <- sum(not_qq$balance_prin1216,na.rm = T)
  prin01 <- sum(not_qq$balance_prin0117,na.rm = T)
  prin02 <- sum(not_qq$balance_prin0217,na.rm = T)
  prin03 <- sum(not_qq$balance_prin0317,na.rm = T)
  prin04 <- sum(not_qq$balance_prin0417,na.rm = T)
  prin05 <- sum(not_qq$balance_prin0517,na.rm = T)
  
  not_qq_prin <- c(prin00,prin01,prin02,prin03,prin04,prin05)
  qq_prin;not_qq_prin
  barplot(qq_prin);barplot(not_qq_prin)
  #额度变化的曲线
  par(mfrow=c(2,1))
  plot(abs(diff(qq_prin)),type='l',lwd = 2.5,main = '提供qq的金额变化',xlab = '月份',ylab = '金额',col=heat.colors(1));
  plot(abs(diff(not_qq_prin)),type='l',lwd = 2.5,main = '未提供qq的金额变化',xlab = '月份',ylab = '金额',col='lightblue')  
  
  ###########################################################################################
  ############################################################################################
  
  #迁跃
  roll00 <- aggregate(qq$balance_prin0616,list(qq$cur_overdue_periods0616),sum)
  roll01 <- aggregate(qq$balance_prin0716,list(qq$cur_overdue_periods0716),sum)
  roll02 <- aggregate(qq$balance_prin0816,list(qq$cur_overdue_periods0816),sum)
  roll03 <- aggregate(qq$balance_prin0916,list(qq$cur_overdue_periods0916),sum)
  roll04 <- aggregate(qq$balance_prin1016,list(qq$cur_overdue_periods1016),sum)
  roll05 <- aggregate(qq$balance_prin1116,list(qq$cur_overdue_periods1116),sum)
  roll06 <- aggregate(qq$balance_prin1216,list(qq$cur_overdue_periods1216),sum)
  roll07 <- aggregate(qq$balance_prin0117,list(qq$cur_overdue_periods0117),sum)
  roll08 <- aggregate(qq$balance_prin0217,list(qq$cur_overdue_periods0217),sum)
  roll09 <- aggregate(qq$balance_prin0317,list(qq$cur_overdue_periods0317),sum)
  roll10 <- aggregate(qq$balance_prin0417,list(qq$cur_overdue_periods0417),sum)
  roll11 <- aggregate(qq$balance_prin0517,list(qq$cur_overdue_periods0517),sum)  
  
  roll00;roll01;roll02;roll03;roll04;roll05;roll06;roll07;roll08;roll09;roll10;roll11
  
  a <- matrix(data=NA,nrow = 9,ncol = 1)    
  a[,1] <- c(0:8)
  a<-as.data.frame(a)
  colnames(a)[1] <- 'Group.1'
  a <- left_join(a,roll00,by = 'Group.1')
  a <- left_join(a,roll01,by = 'Group.1')
  a <- left_join(a,roll02,by = 'Group.1')
  a <- left_join(a,roll03,by = 'Group.1')
  a <- left_join(a,roll04,by = 'Group.1')
  a <- left_join(a,roll05,by = 'Group.1')
  a <- left_join(a,roll06,by = 'Group.1')
  a <- left_join(a,roll07,by = 'Group.1')
  a <- left_join(a,roll08,by = 'Group.1')
  a <- left_join(a,roll09,by = 'Group.1')
  a <- left_join(a,roll10,by = 'Group.1')
  a <- left_join(a,roll11,by = 'Group.1')
  a
  colnames(a) <- c('period','1606','1607','1608','1609','1610','1611','1612','1701','1702','1703','1704','1705')
  qq_matrix <- a
  
  #非qq部分
  roll00 <- aggregate(not_qq$balance_prin0616,list(not_qq$cur_overdue_periods0616),sum)
  roll01 <- aggregate(not_qq$balance_prin0716,list(not_qq$cur_overdue_periods0716),sum)
  roll02 <- aggregate(not_qq$balance_prin0816,list(not_qq$cur_overdue_periods0816),sum)
  roll03 <- aggregate(not_qq$balance_prin0916,list(not_qq$cur_overdue_periods0916),sum)
  roll04 <- aggregate(not_qq$balance_prin1016,list(not_qq$cur_overdue_periods1016),sum)
  roll05 <- aggregate(not_qq$balance_prin1116,list(not_qq$cur_overdue_periods1116),sum)
  roll06 <- aggregate(not_qq$balance_prin1216,list(not_qq$cur_overdue_periods1216),sum)
  roll07 <- aggregate(not_qq$balance_prin0117,list(not_qq$cur_overdue_periods0117),sum)
  roll08 <- aggregate(not_qq$balance_prin0217,list(not_qq$cur_overdue_periods0217),sum)
  roll09 <- aggregate(not_qq$balance_prin0317,list(not_qq$cur_overdue_periods0317),sum)
  roll10 <- aggregate(not_qq$balance_prin0417,list(not_qq$cur_overdue_periods0417),sum)
  roll11 <- aggregate(not_qq$balance_prin0517,list(not_qq$cur_overdue_periods0517),sum) 
  
  roll00;roll01;roll02;roll03;roll04;roll05;roll06;roll07;roll08;roll09;roll10;roll11
  
  a <- matrix(data=NA,nrow = 9,ncol = 1)    
  a[,1] <- c(0:8)
  a<-as.data.frame(a)
  colnames(a)[1] <- 'Group.1'
  a <- left_join(a,roll00,by = 'Group.1')
  a <- left_join(a,roll01,by = 'Group.1')
  a <- left_join(a,roll02,by = 'Group.1')
  a <- left_join(a,roll03,by = 'Group.1')
  a <- left_join(a,roll04,by = 'Group.1')
  a <- left_join(a,roll05,by = 'Group.1')
  a <- left_join(a,roll06,by = 'Group.1')
  a <- left_join(a,roll07,by = 'Group.1')
  a <- left_join(a,roll08,by = 'Group.1')
  a <- left_join(a,roll09,by = 'Group.1')
  a <- left_join(a,roll10,by = 'Group.1')
  a <- left_join(a,roll11,by = 'Group.1')
  a
  colnames(a) <- c('period','1606','1607','1608','1609','1610','1611','1612','1701','1702','1703','1704','1705')
  not_qq_matrix <- a
  
  
  qq_matrix
  a <- NULL
  migration_qq <- vector()
  for(i in 2:12){
    j <- i+1
    a <- qq_matrix[,j][2:9]/qq_matrix[,i][1:8]
    migration_qq <- cbind(migration_qq,a)
  }
  
  
  not_qq_matrix
  a <- NULL
  migration_not_qq <- vector()
  for(i in 2:12){
    j <- i+1
    a <- not_qq_matrix[,j][2:9]/not_qq_matrix[,i][1:8]
    migration_not_qq <- cbind(migration_not_qq,a)
  }
  
  
  migration_qq;migration_not_qq
  
  #qq有大的金额，更低的回收
  #-------------------------------------------笔数
  roll00 <- aggregate(qq$balance_prin0616,list(qq$cur_overdue_periods0616),length)
  roll01 <- aggregate(qq$balance_prin0716,list(qq$cur_overdue_periods0716),length)
  roll02 <- aggregate(qq$balance_prin0816,list(qq$cur_overdue_periods0816),length)
  roll03 <- aggregate(qq$balance_prin0916,list(qq$cur_overdue_periods0916),length)
  roll04 <- aggregate(qq$balance_prin1016,list(qq$cur_overdue_periods1016),length)
  roll05 <- aggregate(qq$balance_prin1116,list(qq$cur_overdue_periods1116),length)
  roll06 <- aggregate(qq$balance_prin1216,list(qq$cur_overdue_periods1216),length)
  roll07 <- aggregate(qq$balance_prin0117,list(qq$cur_overdue_periods0117),length)
  roll08 <- aggregate(qq$balance_prin0217,list(qq$cur_overdue_periods0217),length)
  roll09 <- aggregate(qq$balance_prin0317,list(qq$cur_overdue_periods0317),length)
  roll10 <- aggregate(qq$balance_prin0417,list(qq$cur_overdue_periods0417),length)
  roll11 <- aggregate(qq$balance_prin0517,list(qq$cur_overdue_periods0517),length)  
  
  roll00;roll01;roll02;roll03;roll04;roll05;roll06;roll07;roll08;roll09;roll10;roll11
  
  a <- matrix(data=NA,nrow = 9,ncol = 1)    
  a[,1] <- c(0:8)
  a<-as.data.frame(a)
  colnames(a)[1] <- 'Group.1'
  a <- left_join(a,roll00,by = 'Group.1')
  a <- left_join(a,roll01,by = 'Group.1')
  a <- left_join(a,roll02,by = 'Group.1')
  a <- left_join(a,roll03,by = 'Group.1')
  a <- left_join(a,roll04,by = 'Group.1')
  a <- left_join(a,roll05,by = 'Group.1')
  a <- left_join(a,roll06,by = 'Group.1')
  a <- left_join(a,roll07,by = 'Group.1')
  a <- left_join(a,roll08,by = 'Group.1')
  a <- left_join(a,roll09,by = 'Group.1')
  a <- left_join(a,roll10,by = 'Group.1')
  a <- left_join(a,roll11,by = 'Group.1')
  a
  colnames(a) <- c('period','1606','1607','1608','1609','1610','1611','1612','1701','1702','1703','1704','1705')
  qq_matrix <- a
  
  #非qq部分
  roll00 <- aggregate(not_qq$balance_prin0616,list(not_qq$cur_overdue_periods0616),length)
  roll01 <- aggregate(not_qq$balance_prin0716,list(not_qq$cur_overdue_periods0716),length)
  roll02 <- aggregate(not_qq$balance_prin0816,list(not_qq$cur_overdue_periods0816),length)
  roll03 <- aggregate(not_qq$balance_prin0916,list(not_qq$cur_overdue_periods0916),length)
  roll04 <- aggregate(not_qq$balance_prin1016,list(not_qq$cur_overdue_periods1016),length)
  roll05 <- aggregate(not_qq$balance_prin1116,list(not_qq$cur_overdue_periods1116),length)
  roll06 <- aggregate(not_qq$balance_prin1216,list(not_qq$cur_overdue_periods1216),length)
  roll07 <- aggregate(not_qq$balance_prin0117,list(not_qq$cur_overdue_periods0117),length)
  roll08 <- aggregate(not_qq$balance_prin0217,list(not_qq$cur_overdue_periods0217),length)
  roll09 <- aggregate(not_qq$balance_prin0317,list(not_qq$cur_overdue_periods0317),length)
  roll10 <- aggregate(not_qq$balance_prin0417,list(not_qq$cur_overdue_periods0417),length)
  roll11 <- aggregate(not_qq$balance_prin0517,list(not_qq$cur_overdue_periods0517),length)  
  
  roll00;roll01;roll02;roll03;roll04;roll05;roll06;roll07;roll08;roll09;roll10;roll11
  
  a <- matrix(data=NA,nrow = 9,ncol = 1)    
  a[,1] <- c(0:8)
  a<-as.data.frame(a)
  colnames(a)[1] <- 'Group.1'
  a <- left_join(a,roll00,by = 'Group.1')
  a <- left_join(a,roll01,by = 'Group.1')
  a <- left_join(a,roll02,by = 'Group.1')
  a <- left_join(a,roll03,by = 'Group.1')
  a <- left_join(a,roll04,by = 'Group.1')
  a <- left_join(a,roll05,by = 'Group.1')
  a <- left_join(a,roll06,by = 'Group.1')
  a <- left_join(a,roll07,by = 'Group.1')
  a <- left_join(a,roll08,by = 'Group.1')
  a <- left_join(a,roll09,by = 'Group.1')
  a <- left_join(a,roll10,by = 'Group.1')
  a <- left_join(a,roll11,by = 'Group.1')
  a
  colnames(a) <- c('period','1216','1701','1702','1703','1704','1705')
  not_qq_matrix <- a
  qq_matrix <- as.data.frame(qq_matrix)
  not_qq_matrix <- as.data.frame(not_qq_matrix)
  
  qq_matrix
  a <- NULL
  migration_qq <- vector()
  for(i in 2:6){
    j <- i+1
    a <- qq_matrix[,j][2:9]/qq_matrix[,i][1:8]
    migration_qq <- cbind(migration_qq,a)
  }
  
  not_qq_matrix
  a <- NULL
  migration_not_qq <- vector()
  for(i in 2:6){
    j <- i+1
    a <- not_qq_matrix[,j][2:9]/not_qq_matrix[,i][1:8]
    migration_not_qq <- cbind(migration_not_qq,a)
  }
  
  
  migration_qq;migration_not_qq
  
  
  #-----------------------------------------------
  eff <- read.csv('eff.csv',stringsAsFactors = F)
  eff$OPERATION_DATE <- NULL
  eff$time <- NULL
  str(eff)
  eff <- eff[eff$APPLY_ID %in% M2_qqemail$APPLY_ID,]
  
  eff <- merge(eff,product,by='APPLY_ID')
  eff$qq.y <- NULL
  eff$online.y <- NULL
  colnames(eff)[2] <- 'qq'
  eff <- eff[eff$product_model=='极速模式',]
  
  eff <- na.omit(eff)
  eff1 <- eff
  
  eff <- eff1[eff1$qq==1,]
  
  eff_rate <- NULL
  count_n <- NULL
  count_y <- NULL
  for(i in c(12,1,2,3,4,5)){
     data1 <- eff[eff$month1==i,]
     b <- sum(data1$IS_EFFECTIVE == 'N')
     c <- sum(data1$IS_EFFECTIVE != 'N')
     rate = c/(b+c)
     count_n <- c(count_n,b)
     count_y <- c(count_y,c)
     eff_rate <- c(eff_rate,rate)
  }
  eff_rate;count_n;count_y
  
  
  #--------------amount
  amount <- read.csv('amount.csv',stringsAsFactors = T)
  qq_amount <- amount[amount$qq==1,]
  not_qq_amount <- amount[amount$qq==0,] 
  
  library(fBasics)
  basicStats(qq_amount$audit_amount);basicStats(not_qq_amount$audit_amount)
  par(mfrow=c(2,2))
  hist(na.omit(qq_amount$audit_amount),main = 'qq金额直方图',xlab='金额',ylab='频数',col=heat.colors(1));
  hist(na.omit(not_qq_amount$audit_amount),main = '非qq金额直方图',xlab='金额',ylab='频数',col='lightblue')
  plot(density(na.omit(qq_amount$audit_amount)),main = 'qq金额概率密度图');
  plot(density(na.omit(not_qq_amount$audit_amount)),main = '非qq金额概率密度图')
  
  
  
  