  setwd('F:/guocheng/old')
  file_name <- list.files('F:/guocheng/old',pattern = ".xls")
  
  library(xlsx)
  library(stringr)
 
    
  #读入文件
  a <-read.xlsx2(file_name[1],sheetIndex = 1)  
  
  #截取日期
  date <- substr(file_name[1], nchar(file_name[1])-11, nchar(file_name[1])-4)
  date1 <- paste(substr(date,1,4),'/',substr(date,5,6),'/',substr(date,7,8),sep = "")
  
  #加入日期
  a$date <- as.Date(date1)
  
  ##===累计文件===##
  for(i in c(2:length(file_name))){
    print(i)
    
    #读入文件
    b <-read.xlsx2(file_name[i],sheetIndex = 1)  
  
    #截取日期
    date <- substr(file_name[i], nchar(file_name[i])-11, nchar(file_name[i])-4)
    date1 <- paste(substr(date,1,4),'/',substr(date,5,6),'/',substr(date,7,8),sep = "")
  
    b <- b[,1:30]
    #加入日期
    b$date <- as.Date(date1)
  
    colnames(a) <- colnames(b)
    a <<- rbind(a,b)
  }
  
  old <- a
  #------------------------------------------------#
  setwd('F:/guocheng/new')
  file_name <- list.files('F:/guocheng/new',pattern = ".xls")
  
  a <-read.xlsx2(file_name[1],sheetIndex = 1)  
  
  #截取日期
  date <- substr(file_name[1], nchar(file_name[1])-11, nchar(file_name[1])-4)
  date1 <- paste(substr(date,1,4),'/',substr(date,5,6),'/',substr(date,7,8),sep = "")
  
  #加入日期
  a$date <- as.Date(date1)
  
  ##===累计文件===##
  for(i in c(2:length(file_name))){
    print(i)
    
    #读入文件
    b <-read.xlsx2(file_name[i],sheetIndex = 1)  
    b <- b[1:39]
    #截取日期
    date <- substr(file_name[i], nchar(file_name[i])-11, nchar(file_name[i])-4)
    date1 <- paste(substr(date,1,4),'/',substr(date,5,6),'/',substr(date,7,8),sep = "")
    
    #加入日期
    b$date <- as.Date(date1)
    
    colnames(a) <- colnames(b)
    a <<- rbind(a,b)
  }
  
  new <- a
  
  ####----------------------------M2------------------------------####
  ##<<<<<<<<<<<<<<<<<<<<<<<<<<在这段中进行变化>>>>>>>>>>>>>>>>>>>>>>>>>>>##
  new_m2 <- subset(new,new$periods == 'M2')
  match_city <- as.data.frame(str_match(new_m2$账期,"城市信贷") == '城市信贷')                 
  new_m2_city <- new_m2[!is.na(match_city$V1),]
  
  que <- c('城市信贷-M2-手工电催队列', '城市信贷_M2_授薪类手工电催队列', '城市信贷_M2_自营类手工电催队列',
           '城市信贷_M2_授薪类非测试队列','城市信贷_M2_授薪类测试队列')
  old_m2_city <- old[old$账期 %in% que, ]
  old_m2_city <- old_m2_city[old_m2_city$date > '2016-10-31',]
  
  #数据时间
  range(old_m2_city$date)
  range(new_m2_city$date)
  
  new_var <- c("date","USER_NAME","有效联络客户占比","当日有效联络客户数","当日处理客户数", "剩余客户数", "当日处理量","当日有效联络量","当日总呼出次数","当日总呼出时长",
    "当日有效呼出次数", "当日平均呼出时长.s.", "PTP","当日覆盖率")
  old_var <- c("date","用户名", "有效联络客户占比","有效联络客户数","处理客户数","分案剩余量", "处理量","有效联络量","当日总呼出次数","当日总呼出时长",
    "当日有效呼出次数","当日平均呼出时长.s.","PTP","当日覆盖率")

  data_new <- new_m2_city[new_var]
  data_old <- old_m2_city[old_var]
  colnames(data_old) <- colnames(data_new)
  
  data_all <- rbind(data_new, data_old)
  data_all <- data_all[order(data_all$date),]
  for(i in c(3:ncol(data_all))){
    data_all[,i] <- as.numeric(as.character(data_all[,i]))
  }
  
  apple <- data_all[data_all$date > '2017-03-31' & data_all$当日处理客户数 != 0 & data_all$剩余客户数 > 90,]
  cream <- data_all[data_all$date < '2017-04-01' & data_all$当日处理客户数 != 0 & data_all$剩余客户数 > 90,]
  cream <- cream[cream$date > '2017-02-28' | cream$date <'2017-02-01',]
  
  apple <- unique(apple)
  cream <- unique(cream)
  #时间
  range(apple$date)
  range(cream$date)
  library(corrplot)
  corrplot(cor(apple[, 3:15]))
  corrplot(cor(cream[, 3:15]))
  
  #-----------------------------------------------
  summary(lm(有效联络客户占比~.,data = apple[,-c(1:2)]))
  summary(lm(有效联络客户占比~.,data = cream[,-c(1:2)]))
  
  md1 <- lm(有效联络客户占比~.,data = apple[,-c(1:2)])
  md2 <- lm(有效联络客户占比~.,data = cream[,-c(1:2)])
  
  md1$coefficients - md2$coefficients
  
  #前后数据保持了一致性
  for(i in c(3:14)){
    print(t.test(apple[,i], cream[, i],alternative = c("two.sided")))
    print(i)
    par(mfrow = c(1,2))
     boxplot(apple[,i])
     boxplot(cream[,i])
     plot(density(apple[,i]),main = colnames(apple)[i])
     plot(density(cream[,i]),main = colnames(cream)[i])
  }
  
  #相关系数出现了变化？？
  apple_cor <- as.data.frame(cor(apple[, 3:14]))
  cream_cor <- as.data.frame(cor(cream[, 3:14]))
  
  #协方差结构，可惜我不会
  
  
  #####################-------------M3-------------------########################
  new_m3 <- subset(new,new$periods == 'M3')
  match_city <- as.data.frame(str_match(new_m3$账期,"城市信贷") == '城市信贷')                 
  new_m3_city <- new_m2[!is.na(match_city$V1),]
  
  que <- c('城市信贷-M3-手工电催队列', '城市信贷_M3_授薪类手工电催队列', '城市信贷_M3_自营类手工电催队列',
           '城市信贷_M3_授薪类非测试队列','城市信贷_M3_授薪类测试队列')
  old_m3_city <- old[old$账期 %in% que, ]
  old_m3_city <- old_m3_city[old_m3_city$date > '2016-10-31',]
  
  #数据时间
  range(old_m3_city$date)
  range(new_m3_city$date)
  
  new_var <- c("date","USER_NAME","有效联络客户占比","当日有效联络客户数","当日处理客户数", "剩余客户数", "当日处理量","当日有效联络量","当日总呼出次数","当日总呼出时长",
               "当日有效呼出次数", "当日平均呼出时长.s.", "PTP","当日覆盖率")
  old_var <- c("date","用户名", "有效联络客户占比","有效联络量","处理客户数","分案剩余量", "处理量","有效联络客户数","当日总呼出次数","当日总呼出时长",
               "当日有效呼出次数","当日平均呼出时长.s.","PTP","当日覆盖率")
  
  data_new <- new_m3_city[new_var]
  data_old <- old_m3_city[old_var]
  colnames(data_old) <- colnames(data_new)
  
  data_all <- rbind(data_new, data_old)
  data_all <- data_all[order(data_all$date),]
  for(i in c(3:ncol(data_all))){
    data_all[,i] <- as.numeric(as.character(data_all[,i]))
  }
  
  apple_m3 <- data_all[data_all$date > '2017-03-31' & data_all$当日处理客户数 != 0 & data_all$剩余客户数 >90,]
  cream_m3 <- data_all[data_all$date < '2017-04-01' & data_all$当日处理客户数 != 0 & data_all$剩余客户数 >90,]
  cream_m3 <- cream_m3[cream_m3$date > '2017-02-28' | cream_m3$date <'2017-02-01',]
  
  apple_m3 <- unique(apple_m3)
  cream_m3 <- unique(cream_m3)
  
  #时间
  range(apple_m3$date)
  range(cream_m3$date)
  
  
  library(corrplot)
  corrplot(cor(apple_m3[, 3:14]))
  corrplot(cor(cream_m3[, 3:14]))
  
  #-----------------------------------------------#
  summary(lm(有效联络客户占比~.,data = apple_m3[,-c(1:2)]))
  summary(lm(有效联络客户占比~.,data = cream_m3[,-c(1:2)]))
  
  md1 <- lm(有效联络客户占比~.,data = apple_m3[,-c(1:2)])
  md2 <- lm(有效联络客户占比~.,data = cream_m3[,-c(1:2)])
  
  md1$coefficients - md2$coefficients
  
  #前后数据保持了一致性
  for(i in c(3:14)){
    print(t.test(apple_m3[,i], cream_m3[, i],alternative = c("two.sided")))
    print(i)
    par(mfrow = c(1,2))
    boxplot(apple_m3[,i])
    boxplot(cream_m3[,i])
    plot(density(apple_m3[,i]),main = colnames(apple_m3)[i])
    plot(density(cream_m3[,i]),main = colnames(cream_m3)[i])
  }
  
  #相关系数出现了变化？？
  apple_m3_cor <- as.data.frame(cor(apple_m3[, 3:14]))
  cream_m3_cor <- as.data.frame(cor(cream_m3[, 3:14]))
  
  
  ###################################################分子，分母的分析#######################################
  #---------------------当日有效联络客户数-----------------#
  #-person-#
  #当日总呼出次数 当日平均呼出时长.s 当日有效呼出次数
  for(i in c(1:nrow(apple_cor))){
    a <- paste(colnames(apple_cor)[i],"==>",apple_cor[,3][i])
    print(a)
  }
  
  #当日总呼出次数 当日有效呼出次数 当日平均呼出时长.s
  library(randomForest)
  data1 <- scale(apple[,-c(1,2)], scale = F)
  data1 <- as.data.frame(data1)
  x <- data1[,-3]
  y <- data1[,3]
  
  md_rf <- randomForest(x,y,ntree = 3000)
  rf_importance <- as.data.frame(md_rf$importance)
  
  for(i in c(1:nrow(rf_importance))){
    print(paste(rownames(rf_importance)[i],"==>",rf_importance[i,1]))
  }
  
  
  #----------------------"当日处理客户数"----------------------#
  #当日总呼出次数， 当日有效呼出次数， 当日呼出时长
  for(i in c(1:nrow(apple_cor))){
    a <- paste(colnames(apple_cor)[i],"==>",apple_cor[,2][i])
    print(a)
  }
  
  #当日总呼出次数 当日覆盖率 当日有效呼出次数 当日总呼出时长
  library(randomForest)
  data1 <- scale(apple[,-c(1,2)], scale = F)
  data1 <- as.data.frame(data1)
  
  x <- data1[,-2]
  y <- data1[,2]
  
  md_rf <- randomForest(x,y,ntree = 3000)
  rf_importance <- as.data.frame(md_rf$importance)
  
  for(i in c(1:nrow(rf_importance))){
    print(paste(rownames(rf_importance)[i],"==>",rf_importance[i,1]))
  }
  
  ################cream###############之前的
  #---------------------当日有效联络客户数-----------------#
  #-person-#
  #当日覆盖率、当日处理量、当日呼出次数
  for(i in c(1:nrow(cream_cor))){
    a <- paste(colnames(cream_cor)[i],"==>",cream_cor[,3][i])
    print(a)
  }
  
  #-当日覆盖率，当日处理量，有效联络客户占比，剩余客户数
  library(randomForest)
  data1 <- scale(cream[,-c(1,2)], scale = F)
  data1 <- as.data.frame(data1)
  x <- data1[,-3]
  y <- data1[,3]
  
  md_rf <- randomForest(x,y,ntree = 3000)
  rf_importance <- as.data.frame(md_rf$importance)
  
  for(i in c(1:nrow(rf_importance))){
    print(paste(rownames(rf_importance)[i],"==>",rf_importance[i,1]))
  }
  
  
  #----------------------"当日处理客户数"----------------------#
  #当日有效联络客户数，当日呼出时长，当日处理量
  for(i in c(1:nrow(cream_cor))){
    a <- paste(colnames(cream_cor)[i],"==>",cream_cor[,2][i])
    print(a)
  }
  
  #当日有效联络客户数，当日处理量，当日有效呼出次数
  library(randomForest)
  data1 <- scale(cream[,-c(1,2)], scale = F)
  data1 <- as.data.frame(data1)
  
  x <- data1[,-2]
  y <- data1[,2]
  
  md_rf <- randomForest(x,y,ntree = 3000)
  rf_importance <- as.data.frame(md_rf$importance)
  
  for(i in c(1:nrow(rf_importance))){
    print(paste(rownames(rf_importance)[i],"==>",rf_importance[i,1]))
  }
  
  
  #####################################################M3的分子分母分析######################################
  #---------------------当日有效联络客户数-----------------#
  #-person-#
  #-当日总呼出次数，当日有效呼出次数，当日平均呼出时长
  for(i in c(1:nrow(apple_m3_cor))){
    a <- paste(colnames(apple_m3_cor)[i],"==>",apple_m3_cor[,3][i])
    print(a)
  }
  
  #-当日总呼出次数，当日总呼出时长，当日平均呼出时长.s
  library(randomForest)
  data1 <- scale(apple_m3[,-c(1,2)], scale = F)
  data1 <- as.data.frame(data1)
  x <- data1[,-3]
  y <- data1[,3]
  
  md_rf <- randomForest(x,y,ntree = 3000)
  rf_importance <- as.data.frame(md_rf$importance)
  
  for(i in c(1:nrow(rf_importance))){
    print(paste(rownames(rf_importance)[i],"==>",rf_importance[i,1]))
  }
  
  
  #----------------------"当日处理客户数"----------------------#
  #当日总呼出次数，当日有效呼出次数，当日总呼出时长
  for(i in c(1:nrow(apple_m3_cor))){
    a <- paste(colnames(apple_m3_cor)[i],"==>",apple_m3_cor[,2][i])
    print(a)
  }
  
  #当日总呼出次数，剩余客户量，当日覆盖率
  library(randomForest)
  data1 <- scale(apple_m3[,-c(1,2)], scale = F)
  data1 <- as.data.frame(data1)
  
  x <- data1[,-2]
  y <- data1[,2]
  
  md_rf <- randomForest(x,y,ntree = 3000)
  rf_importance <- as.data.frame(md_rf$importance)
  
  for(i in c(1:nrow(rf_importance))){
    print(paste(rownames(rf_importance)[i],"==>",rf_importance[i,1]))
  }
  
  ################cream###############之前的
  #---------------------当日有效联络客户数-----------------#
  #-person-#
  #当日覆盖率，当日处理量，当日总呼出次数
  for(i in c(1:nrow(cream_m3_cor))){
    a <- paste(colnames(cream_m3_cor)[i],"==>",cream_m3_cor[,3][i])
    print(a)
  }
  
  #当日覆盖率，当日处理量，剩余客户数
  library(randomForest)
  data1 <- scale(cream_m3[,-c(1,2)], scale = F)
  data1 <- as.data.frame(data1)
  x <- data1[,-3]
  y <- data1[,3]
  
  md_rf <- randomForest(x,y,ntree = 3000)
  rf_importance <- as.data.frame(md_rf$importance)
  
  for(i in c(1:nrow(rf_importance))){
    print(paste(rownames(rf_importance)[i],"==>",rf_importance[i,1]))
  }
  
  
  #----------------------"当日处理客户数"----------------------#
  #当日有效联络客户数，当日总呼出时长，当日处理量
  for(i in c(1:nrow(cream_m3_cor))){
    a <- paste(colnames(cream_m3_cor)[i],"==>",cream_m3_cor[,2][i])
    print(a)
  }
  
  
  #当日有效联络客户数，当日处理量，当日覆盖率
  library(randomForest)
  data1 <- scale(cream_m3[,-c(1,2)], scale = F)
  data1 <- as.data.frame(data1)
  
  x <- data1[,-2]
  y <- data1[,2]
  
  md_rf <- randomForest(x,y,ntree = 3000)
  rf_importance <- as.data.frame(md_rf$importance)
  
  for(i in c(1:nrow(rf_importance))){
    print(paste(rownames(rf_importance)[i],"==>",rf_importance[i,1]))
  }
  
  ###################################################################################
  ###################################################################################
  ##对新的进行相关性
  
  #分母-分子顺序
  
  new_var <- c("date","USER_NAME","有效联络客户占比","当日处理客户数","当日有效联络客户数", "剩余客户数", "当日处理量","当日有效联络量","当日总呼出次数","当日总呼出时长",
               "当日有效呼出次数", "当日平均呼出时长.s.", "PTP","当日覆盖率",
               "当日处理量_老案件","当日处理量_新增案件","旧有效联络量","当日有效联络量_新增案件","查找处理量","查找客户数","他人转告处理量","他人转告客户数" )
  new_cor <- new_m2_city[new_var]
  
  for(i in c(3:ncol(new_cor))){
    new_cor[,i] <- as.numeric(new_cor[,i])
  }
 
  cor(new_cor[,-c(1,2)])
  
  data_05 <- new_cor[new_cor$date > '2017-04-30' & new_cor$date < '2017-5-30',]
  range(data_05$date)
  data_06 <- new_cor[new_cor$date > '2017-06-01' & new_cor$date < '2017-7-10',]
  range(data_06$date)
  
  cor(data_05[,-c(1,2)])
  cor(data_06[,-c(1,2)])  
  
  for(i in c(3:ncol(apple))){
    plot(apple[,i], type = 'l',main = colnames(apple)[i])
  }
  plot(apple$当日处理量/apple$剩余客户数, type = 'l') #<== '覆盖率的下降'
  plot(cream$当日处理量/cream$剩余客户数, type = 'l')  
  
  #-------------------------
  for(i in c(3:ncol(data_new))){
    data_new[,i] <- as.numeric(data_new[,i])
  }
  
  for(i in c(3:ncol(data_old))){
    data_old[,i] <- as.numeric(data_old[,i])
  }
  
  data_old <- data_old[data_old$date < '2017-01-01',]
  cor(data_new[,-c(1,2)])
  cor(data_old[,-c(1,2)])
  
  library(ggplot2)
  ggplot(data = apple,aes(x = 剩余客户数, y = 当日处理量)) +geom_smooth()+geom_point()
  
  ggplot(data = cream,aes(x = 剩余客户数, y = 当日处理量)) +geom_smooth()+geom_point()
  
  apple_rate <- apple$当日处理量/apple$当日有效联络量
  cream_rate <- cream$当日处理量/cream$当日有效联络量
  
  apple_rate <- apple_rate[apple_rate<10000]
  #cream_rate <- cream_rate[cream_rate<2]
  
  cream_rate1<- cream$有效联络客户占比
  apple_rate <- data.frame(id = seq(1:length(apple_rate)), apple_rate = apple_rate)
  cream_rate <- data.frame(id = seq(1:length(cream_rate)), cream_rate = cream_rate)
  
  ggplot(data = apple_rate, aes(x = id, y = apple_rate))+geom_point()+geom_smooth()  
  ggplot(data = cream_rate, aes(x = id, y = cream_rate))+geom_point()+geom_smooth()  
  
  mean(apple_rate$apple_rate)
  mean(cream_rate$cream_rate)
  
  #-------------------gaga--------------------#
  data_all <- unique(data_all)
  
  #分析当日处理客户数变高的原因
  index_deal <- data_all$当日处理量/data_all$当日处理客户数
  index_cover <- data_all$当日覆盖率/data_all$当日处理客户数
  index_callout <- data_all$当日总呼出次数/data_all$当日处理客户数
  index_calltime <- data_all$当日总呼出时长/data_all$当日处理客户数
  index_customer <- data_all$剩余客户数/data_all$当日处理客户数
  
  
  index <- data.frame(deal = index_deal, cover=index_cover, callout=index_callout, calltime = index_calltime,
                      customer= index_customer)
  setwd("E:/报表/杂七杂八/有效联系/Processing data")
  write.csv(index,'index.csv')
  
  