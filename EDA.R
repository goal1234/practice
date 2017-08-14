  setwd('E:/报表/杂七杂八/减免分析/data')
  de_money <- read.csv('减免金额.csv',stringsAsFactors = T)
  
  #1按分期/金额减免 2按时间减免 3线下还款确认自动减免 9 减免结清
  unique(de_money$DERATETYPE)
  table(de_money$DERATETYPE)
  
  #
  len <- nrow(de_money)
  #---------------------------------------占比
  sum(de_money$sum_lnsamt !=0)/len    #本金       
  sum(de_money$sum_lnsint !=0)/len    #利息
  sum(de_money$sum_lnsperfee !=0)/len    #分期服务费
  sum(de_money$sum_lnsdefint !=0)/len    #罚息
  sum(de_money$sum_lnslatamt !=0)/len    #滞纳金
  
  #罚息情况
  options(scipen=10) 
  library(fBasics)
  
  f_filter <- function(x,high=0.95,low=0.05){
    var <- x
    var <- var[var>0]
    var_high <- quantile(var,high)
    var_low <- quantile(var,low)
    var <- var[var>var_low &var<var_high]
    print(basicStats(var))
  }
  f_filter(de_money$sum_lnsdefint,0.975,0.025)
  f_filter(de_money$sum_lnslatamt,0.975,0.025)
  f_filter(de_money$sum_lnsamt,0.975,0.025)
  f_filter(de_money$sum_lnsint,0.975,0.025)
  f_filter(de_money$sum_lnsperfee, 0.975, 0.025)
  
  #相同合同的差值是不是少
  de_money <- de_money[order(de_money$CONTRACTNO),]
  result <- vector()
  for(i in 2:len){
    if(de_money[i-1,1] == de_money[i,1]){
      a <- sum(de_money[i,c(4:8)]) - sum(de_money[i-1,c(4:8)])
      result <<- c(result, a)
    }
    print(paste('----->',i/len))
  }
  f_filter(result)
  
  #相关性矩阵
  cor(de_money[,c(4:8)])
  
  #按照组进行分
  de_money %>% group_by(DERATETYPE) %>% 
    summarise(mean = mean(sum_lnsamt),sd=sd(sum_lnsamt),n = sum(sum_lnsamt !=0))
  
  de_money$all <- de_money$sum_lnsamt +de_money$sum_lnsdefint + de_money$sum_lnsint +
                  de_money$sum_lnsperfee + de_money$sum_lnslatamt
  de_money <- de_money[order(de_money$TRANDATE),]
  ggplot(data = de_money)+geom_line(aes(x = TRANDATE,y=all)) + 
                scale_y_continuous(limits=c(0, 10000))
  
  
  