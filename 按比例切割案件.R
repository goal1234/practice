#---透视表还有聚合，分割的操作---#
setwd('F:/案件分隔/data')

filename <- '20171227切案导出文件.csv'
data_cut <- read.csv(filename,stringsAsFactors = T)
data_cut$oper_time <- as.Date(data_cut$oper_time)
str(data_cut)

namefile <- "namematch.csv"
name_match <- read.csv(namefile)


colnames(data_cut)
str(data_cut)

library(dplyr)
detach("package:plyr")
#summarise产生了冲突
#---离职人员全部---#
cut_user_num <- data_cut %>% group_by(USER_NAME) %>% summarise(allot_all = n())

names_all <- cut_user_num$USER_NAME
names =  name_match$USER_NAME

out_run <- subset(data_cut, !USER_NAME %in% names)
data_cut <- subset(data_cut, USER_NAME %in% names)



#-大于235的才进行切割-#
cut_user_num <- data_cut %>% 
  group_by(USER_NAME) %>% 
  summarise(allot_all = n())

cut_user_num <- merge(cut_user_num, name_match, by = 'USER_NAME', all.x = T)

data_cut <- merge(data_cut,cut_user_num,by = 'USER_NAME')

write.csv(data_cut,"data1.csv")

# -用一个数据框去搞一下-# 
data_cut <- merge(data_cut,name_match,by = 'USER_NAME')


data_cut <- subset(data_cut, allot_all >= 255)


library(reshape2)
#---按照切割比例应该切割的案件量---#
cut_num <- dcast(data_cut, USER_NAME~OVERDUE_PERIOD_NUM)


len <- ncol(cut_num)
#-切割规则-#
#-需要分隔的案件-#
#切割后剩下235笔
#先要多出来5笔
  
#cut_data <- cut_user_num$allot_all - cut_user_num$num

#cut_per <- cut_num[,c(2:len)]/cut_user_num$allot_all

#cut_num[,c(2:len)] <- round(cut_per*cut_data, 0)

cut_user_num$cut <- cut_user_num$allot_all - cut_user_num$num

a <- merge(cut_num, cut_user_num, by = "USER_NAME")

a[,2:21] <- (a[,2:21]/a$allot_all)*a$cut
a[, 2:21] <- round(a[,2:21],0)

a$cut1 <- rowSums(a[, 2:21])
rowSums()

b <- a$cut - a$cut1

for (i in 1:length(b)) {
  a[i,9] <- a[i,9] + b[i]
}



a$cut1 <- rowSums(a[, 2:21])
cut_num <- a[, 1:21]
(a$cut - a$cut1) != 0

#a <- round(cut_per*cut_data, 0)
#a
cut_data
(rowSums(a))

#---乘以的话笔数不一样----#

# ---调整下不一样的地方
#a<-which( (rowSums(a)-cut_data) != 0)
#cut_num[1,a]

#cut_num[10,2] = cut_num[10,2] + 1
#cut_num[15,2] = cut_num[15,2] + 1
#cut_num[22,2] = cut_num[22,2] - 1
#cut_num[27,2] = cut_num[27,2] + 1

#-----这个分组排序有的不正确---#

#---形成一个序号---#
cut_sort<-
  data_cut %>%　
    mutate(time = as.numeric(oper_time)) %>% 
    group_by(USER_NAME,OVERDUE_PERIOD_NUM) %>% 
    arrange(time) %>% 
    mutate(cut_sort = 1:length(OVERDUE_PERIOD_NUM))

write.csv(cut_sort,'aa')

#---账期还有名字的匹配---#
cut_num_1 <- 
  melt(cut_num, value.name = 'allot')

cut_var <- 
  merge(x = cut_sort, y = cut_num_1, by.x = c("USER_NAME","OVERDUE_PERIOD_NUM"),
        by.y = c("USER_NAME","variable"),all.x = T)

colnames(cut_num_1)[2] <- "OVERDUE_PERIOD_NUM"
cut_num_1$OVERDUE_PERIOD_NUM <- as.integer(cut_num_1$OVERDUE_PERIOD_NUM)

cut_var1 <- 
  left_join(x = cut_sort, y = cut_num_1, by = c('USER_NAME','OVERDUE_PERIOD_NUM'))

result_cut <- 
  cut_var %>%　mutate(is_cut = ifelse(cut_sort <= allot , 1, 0))

#---写出数据---#
write.csv(result_cut,'F:/案件分隔/output/result.csv')
write.csv(out_run,'F:/案件分隔/output/out_run.csv')
#-------------------------------------------------------------------------------#
#-----------------按照目标城市进行匹配------------------


list.files(getwd())














