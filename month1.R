  #读入数据
  month <- read.csv("E:/报表/杂七杂八/有效联系/Processing data/月报数据.csv", stringsAsFactors = T)
 
  #--EDA--#
  summary(month)
  
  for(i in c(2:ncol(month))){
    par(mfrow = c(2,2))
      plot(month[,i], type ='l', lwd = 3,main = colnames(month)[i])
      qqnorm(month[,i]);qqline(month[,i])
      boxplot(month[,i])
      hist(month[,i])
    par(mfrow = c(1,1))
    plot(density(month[,i]), main = colnames(month)[i])
    acf(month[, i])
  }
  
  
  cor(month[, -1])
  library(corrplot)
  corrplot(cor(month[,-1]))
  
  for(i in c(2:20)){
    month[,i] <- as.numeric(month[, i])
  }
  
  data1 <- scale(month[,-1])
  data1 <- as.data.frame(data1)
  
  
  #person coef
  for(i in c(2:20)){
    md1 <- lm(data1[,1] ~ data1[,i],data = data1)
      coef <- paste(colnames(data1)[i],"==>",md1$coefficients[[2]],sep = " ")
    print(coef)
  }
  
  #randomforest
  library(randomForest)
  x <- data1[,-1]
  y <- data1[,1]
  md_rf <- randomForest(x,y,ntree = 500)
  
  rf_importance <- as.data.frame(md_rf$importance)
  
  for(i in c(1:nrow(rf_importance))){
    print(paste(rownames(rf_importance)[i],"==>",rf_importance[i,1]))
  }
  
  #--对于connect进行--#
  
  #-person coef-#
  #allot get try_call_out 
  for(i in c(2:18)){
    md1 <- lm(data1[,8] ~ data1[,i],data = data1)
    coef <- paste(colnames(data1)[i],"==>",md1$coefficients[[2]],sep = " ")
    print(coef)
  }
  
  #-randomforest-#
  #allot num_person get cm2
  x <- data1[,-c(1,8)]
  y <- data1[,8]
  md_rf <- randomForest(x,y,ntree = 500)
  
  rf_importance <- as.data.frame(md_rf$importance)
  
  for(i in c(1:nrow(rf_importance))){
    print(paste(rownames(rf_importance)[i],"==>",rf_importance[i,1]))
  }
  
  
  