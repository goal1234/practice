  setwd('F:/guocheng/old')
  file_name <- list.files('F:/guocheng/old',pattern = ".xls")
  
  library(xlsx)
  library(stringr)
 
    
  #�����ļ�
  a <-read.xlsx2(file_name[1],sheetIndex = 1)  
  
  #��ȡ����
  date <- substr(file_name[1], nchar(file_name[1])-11, nchar(file_name[1])-4)
  date1 <- paste(substr(date,1,4),'/',substr(date,5,6),'/',substr(date,7,8),sep = "")
  
  #��������
  a$date <- as.Date(date1)
  
  ##===�ۼ��ļ�===##
  for(i in c(2:length(file_name))){
    print(i)
    
    #�����ļ�
    b <-read.xlsx2(file_name[i],sheetIndex = 1)  
  
    #��ȡ����
    date <- substr(file_name[i], nchar(file_name[i])-11, nchar(file_name[i])-4)
    date1 <- paste(substr(date,1,4),'/',substr(date,5,6),'/',substr(date,7,8),sep = "")
  
    b <- b[,1:30]
    #��������
    b$date <- as.Date(date1)
  
    colnames(a) <- colnames(b)
    a <<- rbind(a,b)
  }
  
  old <- a
  #------------------------------------------------#
  setwd('F:/guocheng/new')
  file_name <- list.files('F:/guocheng/new',pattern = ".xls")
  
  a <-read.xlsx2(file_name[1],sheetIndex = 1)  
  
  #��ȡ����
  date <- substr(file_name[1], nchar(file_name[1])-11, nchar(file_name[1])-4)
  date1 <- paste(substr(date,1,4),'/',substr(date,5,6),'/',substr(date,7,8),sep = "")
  
  #��������
  a$date <- as.Date(date1)
  
  ##===�ۼ��ļ�===##
  for(i in c(2:length(file_name))){
    print(i)
    
    #�����ļ�
    b <-read.xlsx2(file_name[i],sheetIndex = 1)  
    b <- b[1:39]
    #��ȡ����
    date <- substr(file_name[i], nchar(file_name[i])-11, nchar(file_name[i])-4)
    date1 <- paste(substr(date,1,4),'/',substr(date,5,6),'/',substr(date,7,8),sep = "")
    
    #��������
    b$date <- as.Date(date1)
    
    colnames(a) <- colnames(b)
    a <<- rbind(a,b)
  }
  
  new <- a
  
  ####----------------------------M2------------------------------####
  ##<<<<<<<<<<<<<<<<<<<<<<<<<<������н��б仯>>>>>>>>>>>>>>>>>>>>>>>>>>>##
  new_m2 <- subset(new,new$periods == 'M2')
  match_city <- as.data.frame(str_match(new_m2$����,"�����Ŵ�") == '�����Ŵ�')                 
  new_m2_city <- new_m2[!is.na(match_city$V1),]
  
  que <- c('�����Ŵ�-M2-�ֹ���߶���', '�����Ŵ�_M2_��н���ֹ���߶���', '�����Ŵ�_M2_��Ӫ���ֹ���߶���',
           '�����Ŵ�_M2_��н��ǲ��Զ���','�����Ŵ�_M2_��н����Զ���')
  old_m2_city <- old[old$���� %in% que, ]
  old_m2_city <- old_m2_city[old_m2_city$date > '2016-10-31',]
  
  #����ʱ��
  range(old_m2_city$date)
  range(new_m2_city$date)
  
  new_var <- c("date","USER_NAME","��Ч����ͻ�ռ��","������Ч����ͻ���","���մ����ͻ���", "ʣ��ͻ���", "���մ�����","������Ч������","�����ܺ�������","�����ܺ���ʱ��",
    "������Ч��������", "����ƽ������ʱ��.s.", "PTP","���ո�����")
  old_var <- c("date","�û���", "��Ч����ͻ�ռ��","��Ч����ͻ���","�����ͻ���","�ְ�ʣ����", "������","��Ч������","�����ܺ�������","�����ܺ���ʱ��",
    "������Ч��������","����ƽ������ʱ��.s.","PTP","���ո�����")

  data_new <- new_m2_city[new_var]
  data_old <- old_m2_city[old_var]
  colnames(data_old) <- colnames(data_new)
  
  data_all <- rbind(data_new, data_old)
  data_all <- data_all[order(data_all$date),]
  for(i in c(3:ncol(data_all))){
    data_all[,i] <- as.numeric(as.character(data_all[,i]))
  }
  
  apple <- data_all[data_all$date > '2017-03-31' & data_all$���մ����ͻ��� != 0 & data_all$ʣ��ͻ��� > 90,]
  cream <- data_all[data_all$date < '2017-04-01' & data_all$���մ����ͻ��� != 0 & data_all$ʣ��ͻ��� > 90,]
  cream <- cream[cream$date > '2017-02-28' | cream$date <'2017-02-01',]
  
  apple <- unique(apple)
  cream <- unique(cream)
  #ʱ��
  range(apple$date)
  range(cream$date)
  library(corrplot)
  corrplot(cor(apple[, 3:15]))
  corrplot(cor(cream[, 3:15]))
  
  #-----------------------------------------------
  summary(lm(��Ч����ͻ�ռ��~.,data = apple[,-c(1:2)]))
  summary(lm(��Ч����ͻ�ռ��~.,data = cream[,-c(1:2)]))
  
  md1 <- lm(��Ч����ͻ�ռ��~.,data = apple[,-c(1:2)])
  md2 <- lm(��Ч����ͻ�ռ��~.,data = cream[,-c(1:2)])
  
  md1$coefficients - md2$coefficients
  
  #ǰ�����ݱ�����һ����
  for(i in c(3:14)){
    print(t.test(apple[,i], cream[, i],alternative = c("two.sided")))
    print(i)
    par(mfrow = c(1,2))
     boxplot(apple[,i])
     boxplot(cream[,i])
     plot(density(apple[,i]),main = colnames(apple)[i])
     plot(density(cream[,i]),main = colnames(cream)[i])
  }
  
  #���ϵ�������˱仯����
  apple_cor <- as.data.frame(cor(apple[, 3:14]))
  cream_cor <- as.data.frame(cor(cream[, 3:14]))
  
  #Э����ṹ����ϧ�Ҳ���
  
  
  #####################-------------M3-------------------########################
  new_m3 <- subset(new,new$periods == 'M3')
  match_city <- as.data.frame(str_match(new_m3$����,"�����Ŵ�") == '�����Ŵ�')                 
  new_m3_city <- new_m2[!is.na(match_city$V1),]
  
  que <- c('�����Ŵ�-M3-�ֹ���߶���', '�����Ŵ�_M3_��н���ֹ���߶���', '�����Ŵ�_M3_��Ӫ���ֹ���߶���',
           '�����Ŵ�_M3_��н��ǲ��Զ���','�����Ŵ�_M3_��н����Զ���')
  old_m3_city <- old[old$���� %in% que, ]
  old_m3_city <- old_m3_city[old_m3_city$date > '2016-10-31',]
  
  #����ʱ��
  range(old_m3_city$date)
  range(new_m3_city$date)
  
  new_var <- c("date","USER_NAME","��Ч����ͻ�ռ��","������Ч����ͻ���","���մ����ͻ���", "ʣ��ͻ���", "���մ�����","������Ч������","�����ܺ�������","�����ܺ���ʱ��",
               "������Ч��������", "����ƽ������ʱ��.s.", "PTP","���ո�����")
  old_var <- c("date","�û���", "��Ч����ͻ�ռ��","��Ч������","�����ͻ���","�ְ�ʣ����", "������","��Ч����ͻ���","�����ܺ�������","�����ܺ���ʱ��",
               "������Ч��������","����ƽ������ʱ��.s.","PTP","���ո�����")
  
  data_new <- new_m3_city[new_var]
  data_old <- old_m3_city[old_var]
  colnames(data_old) <- colnames(data_new)
  
  data_all <- rbind(data_new, data_old)
  data_all <- data_all[order(data_all$date),]
  for(i in c(3:ncol(data_all))){
    data_all[,i] <- as.numeric(as.character(data_all[,i]))
  }
  
  apple_m3 <- data_all[data_all$date > '2017-03-31' & data_all$���մ����ͻ��� != 0 & data_all$ʣ��ͻ��� >90,]
  cream_m3 <- data_all[data_all$date < '2017-04-01' & data_all$���մ����ͻ��� != 0 & data_all$ʣ��ͻ��� >90,]
  cream_m3 <- cream_m3[cream_m3$date > '2017-02-28' | cream_m3$date <'2017-02-01',]
  
  apple_m3 <- unique(apple_m3)
  cream_m3 <- unique(cream_m3)
  
  #ʱ��
  range(apple_m3$date)
  range(cream_m3$date)
  
  
  library(corrplot)
  corrplot(cor(apple_m3[, 3:14]))
  corrplot(cor(cream_m3[, 3:14]))
  
  #-----------------------------------------------#
  summary(lm(��Ч����ͻ�ռ��~.,data = apple_m3[,-c(1:2)]))
  summary(lm(��Ч����ͻ�ռ��~.,data = cream_m3[,-c(1:2)]))
  
  md1 <- lm(��Ч����ͻ�ռ��~.,data = apple_m3[,-c(1:2)])
  md2 <- lm(��Ч����ͻ�ռ��~.,data = cream_m3[,-c(1:2)])
  
  md1$coefficients - md2$coefficients
  
  #ǰ�����ݱ�����һ����
  for(i in c(3:14)){
    print(t.test(apple_m3[,i], cream_m3[, i],alternative = c("two.sided")))
    print(i)
    par(mfrow = c(1,2))
    boxplot(apple_m3[,i])
    boxplot(cream_m3[,i])
    plot(density(apple_m3[,i]),main = colnames(apple_m3)[i])
    plot(density(cream_m3[,i]),main = colnames(cream_m3)[i])
  }
  
  #���ϵ�������˱仯����
  apple_m3_cor <- as.data.frame(cor(apple_m3[, 3:14]))
  cream_m3_cor <- as.data.frame(cor(cream_m3[, 3:14]))
  
  
  ###################################################���ӣ���ĸ�ķ���#######################################
  #---------------------������Ч����ͻ���-----------------#
  #-person-#
  #�����ܺ������� ����ƽ������ʱ��.s ������Ч��������
  for(i in c(1:nrow(apple_cor))){
    a <- paste(colnames(apple_cor)[i],"==>",apple_cor[,3][i])
    print(a)
  }
  
  #�����ܺ������� ������Ч�������� ����ƽ������ʱ��.s
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
  
  
  #----------------------"���մ����ͻ���"----------------------#
  #�����ܺ��������� ������Ч���������� ���պ���ʱ��
  for(i in c(1:nrow(apple_cor))){
    a <- paste(colnames(apple_cor)[i],"==>",apple_cor[,2][i])
    print(a)
  }
  
  #�����ܺ������� ���ո����� ������Ч�������� �����ܺ���ʱ��
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
  
  ################cream###############֮ǰ��
  #---------------------������Ч����ͻ���-----------------#
  #-person-#
  #���ո����ʡ����մ����������պ�������
  for(i in c(1:nrow(cream_cor))){
    a <- paste(colnames(cream_cor)[i],"==>",cream_cor[,3][i])
    print(a)
  }
  
  #-���ո����ʣ����մ���������Ч����ͻ�ռ�ȣ�ʣ��ͻ���
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
  
  
  #----------------------"���մ����ͻ���"----------------------#
  #������Ч����ͻ��������պ���ʱ�������մ�����
  for(i in c(1:nrow(cream_cor))){
    a <- paste(colnames(cream_cor)[i],"==>",cream_cor[,2][i])
    print(a)
  }
  
  #������Ч����ͻ��������մ�������������Ч��������
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
  
  
  #####################################################M3�ķ��ӷ�ĸ����######################################
  #---------------------������Ч����ͻ���-----------------#
  #-person-#
  #-�����ܺ���������������Ч��������������ƽ������ʱ��
  for(i in c(1:nrow(apple_m3_cor))){
    a <- paste(colnames(apple_m3_cor)[i],"==>",apple_m3_cor[,3][i])
    print(a)
  }
  
  #-�����ܺ��������������ܺ���ʱ��������ƽ������ʱ��.s
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
  
  
  #----------------------"���մ����ͻ���"----------------------#
  #�����ܺ���������������Ч���������������ܺ���ʱ��
  for(i in c(1:nrow(apple_m3_cor))){
    a <- paste(colnames(apple_m3_cor)[i],"==>",apple_m3_cor[,2][i])
    print(a)
  }
  
  #�����ܺ���������ʣ��ͻ��������ո�����
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
  
  ################cream###############֮ǰ��
  #---------------------������Ч����ͻ���-----------------#
  #-person-#
  #���ո����ʣ����մ������������ܺ�������
  for(i in c(1:nrow(cream_m3_cor))){
    a <- paste(colnames(cream_m3_cor)[i],"==>",cream_m3_cor[,3][i])
    print(a)
  }
  
  #���ո����ʣ����մ�������ʣ��ͻ���
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
  
  
  #----------------------"���մ����ͻ���"----------------------#
  #������Ч����ͻ����������ܺ���ʱ�������մ�����
  for(i in c(1:nrow(cream_m3_cor))){
    a <- paste(colnames(cream_m3_cor)[i],"==>",cream_m3_cor[,2][i])
    print(a)
  }
  
  
  #������Ч����ͻ��������մ����������ո�����
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
  ##���µĽ��������
  
  #��ĸ-����˳��
  
  new_var <- c("date","USER_NAME","��Ч����ͻ�ռ��","���մ����ͻ���","������Ч����ͻ���", "ʣ��ͻ���", "���մ�����","������Ч������","�����ܺ�������","�����ܺ���ʱ��",
               "������Ч��������", "����ƽ������ʱ��.s.", "PTP","���ո�����",
               "���մ�����_�ϰ���","���մ�����_��������","����Ч������","������Ч������_��������","���Ҵ�����","���ҿͻ���","����ת�洦����","����ת��ͻ���" )
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
  plot(apple$���մ�����/apple$ʣ��ͻ���, type = 'l') #<== '�����ʵ��½�'
  plot(cream$���մ�����/cream$ʣ��ͻ���, type = 'l')  
  
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
  ggplot(data = apple,aes(x = ʣ��ͻ���, y = ���մ�����)) +geom_smooth()+geom_point()
  
  ggplot(data = cream,aes(x = ʣ��ͻ���, y = ���մ�����)) +geom_smooth()+geom_point()
  
  apple_rate <- apple$���մ�����/apple$������Ч������
  cream_rate <- cream$���մ�����/cream$������Ч������
  
  apple_rate <- apple_rate[apple_rate<10000]
  #cream_rate <- cream_rate[cream_rate<2]
  
  cream_rate1<- cream$��Ч����ͻ�ռ��
  apple_rate <- data.frame(id = seq(1:length(apple_rate)), apple_rate = apple_rate)
  cream_rate <- data.frame(id = seq(1:length(cream_rate)), cream_rate = cream_rate)
  
  ggplot(data = apple_rate, aes(x = id, y = apple_rate))+geom_point()+geom_smooth()  
  ggplot(data = cream_rate, aes(x = id, y = cream_rate))+geom_point()+geom_smooth()  
  
  mean(apple_rate$apple_rate)
  mean(cream_rate$cream_rate)
  
  #-------------------gaga--------------------#
  data_all <- unique(data_all)
  
  #�������մ����ͻ�����ߵ�ԭ��
  index_deal <- data_all$���մ�����/data_all$���մ����ͻ���
  index_cover <- data_all$���ո�����/data_all$���մ����ͻ���
  index_callout <- data_all$�����ܺ�������/data_all$���մ����ͻ���
  index_calltime <- data_all$�����ܺ���ʱ��/data_all$���մ����ͻ���
  index_customer <- data_all$ʣ��ͻ���/data_all$���մ����ͻ���
  
  
  index <- data.frame(deal = index_deal, cover=index_cover, callout=index_callout, calltime = index_calltime,
                      customer= index_customer)
  setwd("E:/����/�����Ӱ�/��Ч��ϵ/Processing data")
  write.csv(index,'index.csv')
  
  