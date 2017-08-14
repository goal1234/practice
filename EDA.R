  setwd('E:/����/�����Ӱ�/�������/data')
  de_money <- read.csv('������.csv',stringsAsFactors = T)
  
  #1������/������ 2��ʱ����� 3���»���ȷ���Զ����� 9 �������
  unique(de_money$DERATETYPE)
  table(de_money$DERATETYPE)
  
  #
  len <- nrow(de_money)
  #---------------------------------------ռ��
  sum(de_money$sum_lnsamt !=0)/len    #����       
  sum(de_money$sum_lnsint !=0)/len    #��Ϣ
  sum(de_money$sum_lnsperfee !=0)/len    #���ڷ����
  sum(de_money$sum_lnsdefint !=0)/len    #��Ϣ
  sum(de_money$sum_lnslatamt !=0)/len    #���ɽ�
  
  #��Ϣ���
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
  
  #��ͬ��ͬ�Ĳ�ֵ�ǲ�����
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
  
  #����Ծ���
  cor(de_money[,c(4:8)])
  
  #��������з�
  de_money %>% group_by(DERATETYPE) %>% 
    summarise(mean = mean(sum_lnsamt),sd=sd(sum_lnsamt),n = sum(sum_lnsamt !=0))
  
  de_money$all <- de_money$sum_lnsamt +de_money$sum_lnsdefint + de_money$sum_lnsint +
                  de_money$sum_lnsperfee + de_money$sum_lnslatamt
  de_money <- de_money[order(de_money$TRANDATE),]
  ggplot(data = de_money)+geom_line(aes(x = TRANDATE,y=all)) + 
                scale_y_continuous(limits=c(0, 10000))
  
  
  