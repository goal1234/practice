# melt reshape2
# 汇总，提取行列，按照维度聚合
a <- array(rep(1:3, each = 3), dim = c(3, 3))
rowSums(a)
colSums(a)
table(a)

b <- array(rep(1:3, each = 9), dim = c(3,3,3))
rowSums(b, na.rm = T, dims = 1)
colSums(b, na.rm = T, dims = 1)

table(b)  # 因子统计

c <- sample(letters[1:5], 10, replace = T)
table(c)


# 两个输入
a <- rep(letters[1:3], each = 4)
b <- sample(LETTERS[1:3], 12, replace = T)

# 交叉表行示b大写字母，列是a小写字母, 填充的是出现的次数
table(a, b)

#　在行上或者列上进行函数操作
# apply系列

# alt + svm
a-l-t-s-v-m

apply
lapply
sapply
vapply
tapply
mapply

a <- array(rep(1:3, each = 3), dim = c(3, 3))
apply(X = a, MARGIN = 1, FUN = sum)  # 1+2+3 
b <- apply(a, 1, sum);typeof(b)

# 在函数里有参数
apply(a, MARGIN=1, FUN=quantile, probs = seq(0,1, 0.25))
b <- apply(a, MARGIN=1, FUN=quantile, probs=seq(0,1, 0.25))
typeof(b)

# lapply 在列表中每个用，返回了列表,列表内运算
scores <- list(YuWen=c(80,88,94,70), 
               ShuXue=c(99,87,100,68,77))

lapply(scores, mean)
lapply(score, quantile, probs = seq(0, 1, 0.25))

b <- lapply(scores, mean)
b
typeof(b)

# 列表的名字
names(b)
# 数值
as.numeric(b)


val <- list()
len <- length(b)
for(i in 1:len) {
    val[i] <- b[i]
}

# 返回太过麻烦用sapply改进返回
sapply(scores, mean)
sapply(scores, quantile, probs=c(0.5,0.7,0.9))
b <- sapply(scores, quantile, probs=c(0.5,0.7,0.9))
b
typeof(b)
as.data.frame(b) # 行为名字，列为数值或，行为参数列为名字

# 对返回的长度进行限制等vapply(),补充了sapply()

# rt.value限制了３个长度
# 当4个的时候会出现报错
probs <- c(1:3)/4
rt.value <- c(0, 0, 0)
vapply(scores, quantile, FUN.VALUE = rt.value, probs = probs)

# 出现报错
probs <- c(1:4)/4
rt.value <- c(0, 0, 0)
vapply(scores, quantile, FUN.VALUE = rt.value, probs = probs)

# 修正了错误
probs <- c(1:4)/4
rt.value <- c(0, 0, 0, 0)
vapply(scores, quantile, FUN.VALUE = rt.value, probs = probs)

# 类型出现错误
probs <- c(1:4)/4
rt.value <- c(0, 0, 0, "")
vapply(scores, quantile, FUN.VALUE = rt.value, probs = probs)


# mapply 在列表和向量上用，不过sapply()没有列表嵌套进行遍历
mapply(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)

# 得到的是一样的
sapply(X = 1:4, FUN = rep, times = 4)
mapply(rep, x = 1:4, times = 4)

# 有趣
# 重复1一次，2两次， 3三次,rep中each不能进行多次,如each = c(1,2)
mapply(rep, x = 1:4, times = 1:4)
# 重复1一次，2两次， 3一次， 4两次
mapply(rep, x = 1:4, times = 1:2)

# tapply()
# tapply()是拓展了table()函数，table()函数按因子组合计算频度，而tapply可以按因子组合
# 应用各种函数
tapply(X, INDEX, FUN = NULL, ..., simplify = TRUE)
(x <- 1:10)
(f <- gl(2,5, labels=c("CK", "T")))
tapply(x, f, length)
table(f)
tapply(x, f, sum)

# by函数是tapply函数针对数据框类型数据的应用
with(mtcars, by(mtcars, cyl, summary))

# 分组统计功能
aggregate(x, by, FUN, ..., simplify = TRUE)
aggregate(formula, data, FUN, ..., subset, na.action = na.omit)
aggregate(x, nfrequency = 1, FUN = sum, ndeltat = 1, ts.eps = getOption("ts.eps"), ...)

str(mtcars)
attach(mtcars)
aggregate(mtcars, by=list(cyl), FUN = mean)
aggregate(mtcars, by=list(cyl, gear), FUN = mean)

# 用cyl和gear因子组合对数据进行操作
aggregate(cbind(mpg,hp) ~ cyl+gear, FUN = mean)


# 长宽数据转换
# reshape2和melt函数包
library(reshape2)# 加载包

score <- data.frame(userid = c(1,2,3,4),
                    maths = c(80,90,67,78),
                    english = c(90,89,78,56),
                    chemistry = c(83,94,68,77))
score

# melt()函数将宽数据变为长数据
melt(data, #data为data frame
    id.vars, #保留的变量
    measure.vars,#选入的变量名称作为新的一个变量的分类值，默认选择id.vars外的所有变量
    variable.name = "variable",#选入变量构成的新列的名称
    ...,
    na.rm = FALSE, # 是否移除缺失值
    value.name = "value",#选入变量的数值构成的新列的名称
    factorsAsStrings = TRUE
)

course <- melt(data = score,
               measure.vars = c('maths','english','chemistry'),
               id.vars = 'userid',
               variable.name = 'course',
               value.name = 'score')
course

# cast()一种将长数据进行分类汇总的方法
# dcast() 数据框
# acast() 列表，矩阵，数组

dcast(data, formula, fun.aggregate = NULL, ..., margins = NULL,
  subset = NULL, fill = NULL, drop = TRUE,
  value.var = guess_value(data))

acast(data, formula, fun.aggregate = NULL, ..., margins = NULL,
  subset = NULL, fill = NULL, drop = TRUE,
  value.var = guess_value(data))

dcast(data = course, formula = userid~course)

dcast(data = course, formula = course~userid)

#Air quality example
names(airquality) <- tolower(names(airquality))
aqm <- melt(airquality, id=c("month", "day"), na.rm=TRUE)

acast(aqm, day ~ month ~ variable)
acast(aqm, month ~ variable, mean)
acast(aqm, month ~ variable, mean, margins = TRUE)
dcast(aqm, month ~ variable, mean, margins = c("month", "variable"))

library(plyr) # needed to access . function
acast(aqm, variable ~ month, mean, subset = .(variable == "ozone"))
acast(aqm, variable ~ month, mean, subset = .(month == 5))

#Chick weight example
names(ChickWeight) <- tolower(names(ChickWeight))
chick_m <- melt(ChickWeight, id=2:4, na.rm=TRUE)

dcast(chick_m, time ~ variable, mean) # average effect of time
dcast(chick_m, diet ~ variable, mean) # average effect of diet
acast(chick_m, diet ~ time, mean) # average effect of diet & time

# How many chicks at each time? - checking for balance
acast(chick_m, time ~ diet, length)
acast(chick_m, chick ~ time, mean)
acast(chick_m, chick ~ time, mean, subset = .(time < 10 & chick < 20))

acast(chick_m, time ~ diet, length)

dcast(chick_m, diet + chick ~ time)
acast(chick_m, diet + chick ~ time)
acast(chick_m, chick ~ time ~ diet)
acast(chick_m, diet + chick ~ time, length, margins="diet")
acast(chick_m, diet + chick ~ time, length, drop = FALSE)

#Tips example
dcast(melt(tips), sex ~ smoker, mean, subset = .(variable == "total_bill"))

ff_d <- melt(french_fries, id=1:4, na.rm=TRUE)
acast(ff_d, subject ~ time, length)
acast(ff_d, subject ~ time, length, fill=0)
dcast(ff_d, treatment ~ variable, mean, margins = TRUE)
dcast(ff_d, treatment + subject ~ variable, mean, margins="treatment")
if (require("lattice")) {
 lattice::xyplot(`1` ~ `2` | variable, dcast(ff_d, ... ~ rep), aspect="iso")
}




# 频数表
library(vcd)
library(grid)
head(Arthritis)

# 一维联表
mytable <- with(Arthritis, table(Improved))
mytable

prop.table(mytable)

# 二维联表
mytables <- table(A, B)
xtabs(~A+B, data = mydata)

#其中A为行， B为列
mytable<-xtabs(~Treatment+Improved,data = Arthritis)  
mytable

# 边际还有边际频数比例
margin.table(mytable,1)
prop.table(mytable,1) 

# 下标1指代table()语句中的第一个变量
margin.table(mytable,2) 
prop.table(mytable,2)

# 添加边际和
addmargins(mytable)

# table()函数默认忽略缺失值（NA）。要在频数统计中将NA视为一个有效的类别，请设  
# 定参数useNA="ifany"。 

# --搞个二维联表-- #

install.packages("gmodels")  
library(gmodels)  
  
install.packages("gdata")  
library(gdata)  
  
CrossTable(Arthritis$Treatment,Arthritis$Improved) 


# ftable的输出更为紧凑
mytable <- xtabs(~Treatment+Sex+Improved,data = Arthritis)  
  
ftable(mytable)
ftable(prop.table(mytable,c(1,2)))           
ftable(addmargins(prop.table(mytable,c(1,2)),3)) 
ftable(addmargins(prop.table(mytable,c(1,2)),3))*100



