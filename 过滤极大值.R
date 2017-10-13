  setwd('E:/报表/杂七杂八/审批流时效/data')
  filename <- "derate.csv"
  time <- read.csv(filename, stringsAsFactors = F)
  in_data <- read.csv('in.csv', stringsAsFactors = F)
  out_data <- read.csv('outsource.csv', stringsAsFactors = F)
  str(time)
  
  #---去掉周六日的时效---#
  #-当日期是空的时候就不知怎么办了,前面应该定义下na.strings
  in_data$dcreat <- 
    weekdays(as.Date(substr(as.character(in_data$create_date),1,8)))
  in_data$ddeal1 <- 
    weekdays(as.Date(substr(as.character(in_data$deal_time1),1,8)))
  in_data$ddeal2 <- 
    weekdays(as.Date(substr(as.character(in_data$deal_time2),1,8)))
  in_data$ddeal3 <- 
    weekdays(as.Date(substr(as.character(in_data$deal_time3),1,8)))
  in_data$ddeal4 <- 
    weekdays(as.Date(substr(as.character(in_data$deal_time4),1,8)))
  in_data$ddeal5 <- 
    weekdays(as.Date(substr(as.character(in_data$deal_time5),1,8)))
  in_data$ddeal_sep <- 
    weekdays(as.Date(substr(as.character(in_data$deal_time_sep),1,8)))
  in_data$ddearate <- 
    weekdays(as.Date(substr(as.character(in_data$deratetime),1,8)))
  
  
  
  in_data$create_date[in_data$dcreat %in% c('星期六','星期日')] <- NA
  in_data$deal_time1[in_data$ddeal1 %in% c('星期六','星期日')]<- NA
  in_data$deal_time2[in_data$ddeal2 %in% c('星期六','星期日')]<- NA
  in_data$deal_time3[in_data$ddeal3 %in% c('星期六','星期日')]<- NA
  in_data$deal_time4[in_data$ddeal4 %in% c('星期六','星期日')]<- NA
  in_data$deal_time5[in_data$ddeal5 %in% c('星期六','星期日')]<- NA
  in_data$deal_time_sep[in_data$ddeal_sep %in% c('星期六','星期日')]<- NA
  in_data$deratetime[in_data$ddearate %in% c('星期六','星期日')]<- NA
  
  in_data1 <- in_data[1:42]
  

  in_data <- out_data
  in_data$dcreat <- 
    weekdays(as.Date(substr(as.character(in_data$create_date),1,8)))
  in_data$ddeal1 <- 
    weekdays(as.Date(substr(as.character(in_data$deal_time1),1,8)))
  in_data$ddeal2 <- 
    weekdays(as.Date(substr(as.character(in_data$deal_time2),1,8)))
  in_data$ddeal3 <- 
    weekdays(as.Date(substr(as.character(in_data$deal_time3),1,8)))
  in_data$ddeal5 <- 
    weekdays(as.Date(substr(as.character(in_data$deal_time5),1,8)))
  in_data$ddeal_sep <- 
    weekdays(as.Date(substr(as.character(in_data$deal_time_sep),1,8)))
  in_data$ddearate <- 
    weekdays(as.Date(substr(as.character(in_data$deratetime),1,8)))
  
  in_data$create_date[in_data$dcreat %in% c('星期六','星期日')] <- NA
  in_data$deal_time1[in_data$ddeal1 %in% c('星期六','星期日')]<- NA
  in_data$deal_time2[in_data$ddeal2 %in% c('星期六','星期日')]<- NA
  in_data$deal_time3[in_data$ddeal3 %in% c('星期六','星期日')]<- NA
  in_data$deal_time4[in_data$ddeal4 %in% c('星期六','星期日')]<- NA
  in_data$deal_time5[in_data$ddeal5 %in% c('星期六','星期日')]<- NA
  in_data$deal_time_sep[in_data$ddeal_sep %in% c('星期六','星期日')]<- NA
  in_data$deratetime[in_data$ddearate %in% c('星期六','星期日')]<- NA
  
    out_data <- in_data[1:42]
   
    in_data <- in_data1
    rm(in_data1)
    
    #---变为分---#
    in_data[,32:42] <- in_data[,32:42]/60
    out_data[,32:42] <- out_data[,32:42]/60
  
  

  
  binning_one <- function(x, y, decs = T){
    if(is.null(x)) break
    var <- na.omit(x)
    var <- sort(var,decreasing = decs)  
    len <- length(var)
    var <- data.frame(var = var, sort = (var-min(var))/(max(var) - min(var)))
  
    point <- c(0, y, 1)
    var$gtype <- as.numeric(cut(var$sort, point))
    b <- as.data.frame(table(var$gtype))
    a <- as.data.frame(prop.table(table(var$gtype)))
    a$nobs <- b$Freq
    
    interval <- vector()
    for(i in 1:(length(point)-1)){
      interval <- c(interval, paste(point[i],'-',point[i+1]))
    }
    a$interval <- interval
    colnames(a) <- c('id', 'proportion','nobs','interval')
    print(a)
    Cpoints <<- a
    Cpoints_detail <<- var
    barplot(Cpoints$proportion,names.arg = Cpoints$interval,
          col = heat.colors(nrow(Cpoints)),beside = T)
  }
  
  binning_one(time$time_f_1,c(0.02,0.05,0.2,0.3,0.4))
  
  #repay实际还款比率 走的流程
 
  
  
    #去掉前5%后计算均值，中位数，方差，四分位数
    
    only_95 <- function(x){
      if(all(is.na(x))){
        break
      }
      var <- na.omit(x)
      var <- sort(var,decreasing = T)  
      len <- length(var)
      var <- data.frame(var = var, sort = (1:len)/len)
      var <- subset(var, sort >= 0.05)    
      avg <- mean(var$var);med <- median(var$var);q1 <- quantile(var$var,0.25)
      q3 <- quantile(var$var,0.75);sd = sd(var$var);cv = sd(var$var)/mean(var$var)
    
      stat <<- data.frame(len = len, q1, med = med, avg = avg, q3, sd, cv)
      print(stat)
    }
    
  only_95(time$time_1_2)
  
  
  
 #----------------------------gogoing--------------------#
  rm(result)  
  r1 <- subset(in_data, flow == 4)
  
  #---这个进行了每个环节的统计量出来---#
  a <- try(only_95(r1$time_all),silent = T)
  if("try-error" %in% class(a)){
    rm(stat)
  }else{
      rownames(stat) <- "time_all"
      time_all <<- stat
  } 
  rm(stat)
  
  a <- try(only_95(r1$time_support),silent = T)
  if("try-error" %in% class(a)){
    rm(stat)
  }else{
    rownames(stat) <- "time_support"
      time_support <<-stat
  }
  
  rm(stat)
  a <- try(only_95(r1$time_surplus),silent = T)
  if("try-error" %in% class(a)){
    rm(stat)
  }else{
    rownames(stat) <- "time_surplus"
      time_surplus <<- stat
  }
  
  rm(stat)
  a <- try(only_95(r1$time_f_1),silent = T)
  if("try-error" %in% class(a)){
    rm(stat)
  }else{
    rownames(stat) <- "time_f_1"
      time_f_1 <<- stat
  }
  
  rm(stat)
  a <- try(only_95(r1$time_f_2),silent = T)
  if("try-error" %in% class(a)){
    rm(stat)
  }else{
    rownames(stat) <- "time_f_2"
    time_f_2 <<- stat
  }
  
  rm(stat)
  a <- try(only_95(r1$time_f_3),silent = T)
  if("try-error" %in% class(a)){
    rm(stat)
  }else{
    rownames(stat) <- "time_f_3"
      time_f_3 <<- stat
  }
  
  rm(stat)
  a <- try(only_95(r1$time_f_4),silent = T)
  if("try-error" %in% class(a)){
    rm(stat)
  }else{
      rownames(stat) <- "time_f_4"
     time_f_4 <<- stat
  }
  
  rm(stat)
  a <- try(only_95(r1$time_1_2),silent = T);
  if("try-error" %in% class(a)){
    rm(stat)
  }else{
    rownames(stat) <- "time_1_2"
      time_1_2 <<- stat
      
  }
  
  rm(stat)
  a <- try(only_95(r1$time_2_3),silent = T)
  if("try-error" %in% class(a)){
    rm(stat)
  }else{
    rownames(stat) <- "time_2_3"
      time_2_3 <- stat
  }
  rm(stat)
  a <- try(only_95(r1$time_3_4),silent = T)
  if("try-error" %in% class(a)){
    rm(stat)
  }else{
    rownames(stat) <- "time_3_4"
      time_3_4 <<- stat
  }
  
  
  #---忽略错误进行合并---#
  try(if(is.data.frame(time_all)) print('get'),silent = T)
  try(if(is.data.frame(time_support)) result <- rbind(time_all,time_support), silent = T)
  try(if(is.data.frame(time_surplus)) result <- rbind(result,time_surplus), silent = T)
  try(if(is.data.frame(time_f_1)) result <- rbind(result,time_f_1), silent = T)
  try(if(is.data.frame(time_f_2)) result <- rbind(result,time_f_2), silent = T)
  try(if(is.data.frame(time_f_3)) result <- rbind(result,time_f_3), silent = T)
  try(if(is.data.frame(time_f_4)) result <- rbind(result,time_f_4), silent = T)
  try(if(is.data.frame(time_1_2)) result <- rbind(result,time_1_2), silent = T)
  try(if(is.data.frame(time_2_3)) result <- rbind(result,time_2_3), silent = T)
  try(if(is.data.frame(time_3_4)) result <- rbind(result,time_3_4), silent = T)
  
  
  #-删除数据集-#
  rm(time_all, time_support, time_surplus, time_f_1, time_f_2, time_f_3, time_f_4,
     time_1_2, time_2_3, time_3_4)
  View(result)
  only_95((r1$time_f_4 - r1$time_f_2))
  
  
  
  
  result$id <- rownames(result)
  result$flow <- paste(1,1)
  flowresult <<- result
  
  flow(3,3)
  
  
  flow(1,1)
  paste('flow-',1,'-',2)
  flow_1_1 <- rbind(time_all, time_support, time_surplus, time_f_1, time_f_2,
         time_1_2)
  flow_1_1$id <- rownames(flow_1_1)
  
  rm(time_all, time_support, time_surplus, time_f_1, time_f_2,
        time_f_3, time_f_4, time_1_2, time_2_3, time_3_4)
  
  
  rm(result)
  
  
  
    