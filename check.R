#---------------------找出数据的不同-----------------------------
  setwd('E:/报表/杂七杂八/check/新贷后')
  real <-  read.csv('新贷后测试用20170527.csv',stringsAsFactors = T)
  mirror <- read.csv('新贷后测试用_test20170527.csv',stringsAsFactors = T)
  
  #按照唯一值进行排序
  real <- real[order(real$contractno,decreasing = T),]
  mirror <- mirror[order(mirror$contractno,decreasing = T), ]
  
  len <- ncol(real)
  same <- vector()
  for(i in 1:len){
    a <-identical(real[,i],mirror[,i])
    same <- c(same,a)
  }
  
  what <- which(same == F)
  ##只是找出不同的并没有定位在何处
  result <- NULL
  for(i in what){
    a <-setdiff(real[,i],mirror[,i])
    print(a)
  }
  
  ###找到不同的行
  long <- nrow(real)
  where <- vector()
  for(j in what){
    for(i in 1:long){
    a <- identical(real[i,j],mirror[i, j])
      if (a == F){
        where <- c(where,i)
      }
    }
  }
    