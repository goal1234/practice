# --------------------------------------------------- #
# 输出了不同月份的dpd情况
# J的账期,占比每个月的情况
  
  min(m2_j$sort_t); min(m2_n$sort_t)  
  max(m2_j$sort_t); max(m2_n$sort_t)

  cut_dpd <- function(m2_j){
    cut_point <- c(1, 2, 3, 4, 5, 6, 7, 8, Inf)
    
    #9个切割点8个标签
    labs <- c(2, 3, 4, 5, 6, 7, 8, 9)
    a <- cut(m2_j$dpd, breaks = 
               cut_point, right = T,labels = labs)
    a <- as.numeric(as.character(a))
    return(a)
  }
  
  m2_j$cdpd <- cut_dpd(m2_j)
  m2_n$cdpd <- cut_dpd(m2_n)
  
  
  # 同期未区分账期上的不同
  min_t <- min(m2_j$sort_t) + 1
  max_t <- max(m2_j$sort_t)
  
  out <- data.frame(dpd = c(2, 3, 4, 5,
                            6, 7, 8, 9))
# 未扣除成本减免到本金能够覆盖 # 
  for(i in min_t:max_t){
    
    period1 <- subset(m2_j, sort_t == i)  
    period2 <- subset(m2_n, sort_t == i)
  
    # -合同数量- #
    length1 <- length(period1$CONTRACTNO)
    length2 <- length(period2$CONTRACTNO)
    
    #- 两次以上再搞个公式 -#
    dpd1 <- as.data.frame(table(period1$cdpd))
    dpd1$Var1 <- as.numeric(as.character(dpd1$Var1))
    dpd1$count <- dpd1$Freq/sum(dpd1$Freq)
    colnames(dpd1) <- c('dpd', paste(i,'count',sep = '_j_'), 
                        paste(i, 'freq',sep = '_j_'))
    
    dpd2 <- as.data.frame(table(period2$cdpd))
    dpd2$Var1 <- as.numeric(as.character(dpd2$Var1))
    dpd2$count <- dpd2$Freq/sum(dpd2$Freq)
    colnames(dpd2) <- c('dpd', paste(i,'count', sep = '_n_'), 
                        paste(i, 'freq', sep = '_n_'))
    library(dplyr)
    
    out <- merge(out, dpd1,by = 'dpd',all.x = T)  
    out <- merge(out, dpd2, by = 'dpd',all.x = T)
  }
  
  result <- t(out)
  colnames(result) <- result[1,]
  result <- result[-1,]
  
  time1 <- data.frame(sort_t = 
                        substr(rownames(result), 1, 5),
                      type = 
                        substr(rownames(result), 7, 7),
                      what = 
                        substr(rownames(result), 9, 9)
                      )
  
  result <- cbind(result, time1)
  
  match_table <- data.frame(year = m2_j$year, month = m2_j$month,
                            sort_t = m2_j$sort_t)
  match_table <- unique(match_table)
  
  result <- 
    merge(result, match_table, by = 'sort_t',all.x = T)
  
  
  # --- 整理下输出 --- #
  result_dpd <- result
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  