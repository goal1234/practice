  #属性
y <- 1:10 
attr(y, "my_attribute") <- "This is a vector" 
attr(y, "my_attribute")
str(attributes(y))

structure(1:10, my_attribute = "This is a vector")
attributes(y[1])  #当修改向量时，大多数属性会丢失
attributes(sum(y))

names(x);class(x);dim(x)

x <- c(a = 1, b = 2, c = 3)
#modify in place
x <- 1:3; names(x) <- c("a","b", "c")
x <- setNames(1:3, c("a","b", "c"))

y <- c(a = 1, 2, 3);names(y)
z <- c(1, 2, 3);names(z)

x <- factor(c("a", "b", "b", "a"))
x;class(x);levels(x)

# 你不能使用levels中没有的值
x[2] <- "c"
#不能连接因子
c(factor("a"), factor("b"))

#---因子的用法---
sex_char <- c("m", "m", "m") 
sex_factor <- factor(sex_char, levels = c("m", "f")) 
table(sex_char);table(sex_factor)

#read.csv()中使用na.strings
z <- read.csv(text = "value\n12\n1\n.\n9") 
typeof(z$value)
as.double(z$value)
class(z$value)
as.double(as.character(z$value))

z <- read.csv(text = "value\n12\n1\n.\n9", na.strings=".") 
typeof(z$value);class(z$value)

#---------------------------------矩阵和数组---------------------------
# 两个标量参数指定了行和列 
a <- matrix(1:6, ncol = 3, nrow = 2) 
# 一个向量参数描述所有的维度 
b <- array(1:12, c(2, 3, 2)) 
# 你也可以通过设置dim()就地修改一个对象 
c <- 1:6 dim(c) <- c(3, 2);c
#小的方法
length(a);nrow(a);ncol(a)
rownames(a) <- c("A", "B")
colnames(a) <- c("a", "b", "c")
dimnames(b) <- list(c("one", "two"), c("a", "b", "c"), c("A", "B"))


str(1:3) # 一维向量
str(matrix(1:3, ncol = 1)) # 列向量
str(matrix(1:3, nrow = 1)) # 行向量
str(array(1:3, 3)) # "数组"("array")向量


#列表转换
l <- list(1:3, "a", TRUE, 1.0)
dim(l) <- c(2, 2)

#-----------------------------数据框-----------------------------#
df <- data.frame(x = 1:3, y = c("a", "b", "c")) 
str(df)
df <- data.frame( x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE);str(df)

#测试和强制转换
typeof(df);class(df);is.data.frame(df)

#连接数据框cbind() rbind()此时行，列应该是相等
cbind(df, data.frame(z = 3:1))
rbind(df, data.frame(x = 10, y = "z"))

#使用plyr::rbind.fill()可以连接列数不同的数据框
bad <- data.frame(cbind(a = 1:2, b = c("a", "b")));str(bad)
good <- data.frame(a = 1:2, b = c("a", "b"), stringsAsFactors = FALSE);str(good)

#---特殊列---#
df <- data.frame(x = 1:3) 
df$y <- list(1:2, 1:3, 1:4);df

#传入列作为数据框的一列
dfl <- data.frame(x = 1:3, y = I(list(1:2, 1:3, 1:4)));str(dfl)
dfm <- data.frame(x = 1:3, y = I(matrix(1:9, nrow = 3)));str(dfm)

#3取子集操作
#3.1.1原子向量
x <- c(2.1, 4.2, 3.3, 5.4)

#正整数返回位置
x[c(3, 1)]
x[order(x)]
x[c(1, 1)]  #得到重复索引
x[c(2.1, 2.9)]# 实数会被隐式地截断成整数

#负整数忽略该位置的元素
x[-c(3, 1)]
x[c(-1, 2)]  #不能进行正负混合操作

#逻辑向量选出对应位置为TRUE的元素
x[c(TRUE, TRUE, FALSE, FALSE)]
x[x > 3]
x[c(TRUE, FALSE)]# 相当于
x[c(TRUE, FALSE, TRUE, FALSE)]
#缺失值总是在输出中产生缺失值：
x[c(TRUE, TRUE, NA, FALSE)]
x[]
x[0]  #0返回零长度的向量

(y <- setNames(x, letters[1:4]))
# 就像整数索引，你可以重复索引。 
y[c("a", "a", "a")]

#3.1.3矩阵和数组
a <- matrix(1:9, nrow = 3) 
colnames(a) <- c("A", "B", "C")
a[1:2, ];a[c(T, F, T), c("B", "A")]
a[0, -2]
(vals <- outer(1:5, 1:5, FUN = "paste", sep = ","))
vals[c(4, 15)]

#3.1.4数据框
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3]) 
df[df$x == 2, ]
df[c(1, 3), ]
df[c("x", "z")];df[, c("x", "z")]
#简化
str(df["x"]);str(df[, "x"])  #使用drop = FALSE参数可以避免这一点。

#取子集操作符
#[[和$
#返回某个值
a <- list(a = 1, b = 2);a[[1]]
## 如果你提供的是向量，那么会进行递归索引
b <- list(a = list(b = list(c = list(d = 1)))) 
b[[c("a", "b", "c", "d")]]
# 与以下相同
b[["a"]][["b"]][["c"]][["d"]]

#取子集方式：简化与保持
var <- "cyl" # 不可行 - 
mtcars$var #被解释成mtcars[["var"]] mtcars$

#表查询
x <- c("m", "f", "u", "f", "f", "m", "m") 
lookup <- c(m = "Male", f = "Female", u = NA)
unname(lookup[x])

## 或者更少的输出值
c(m = "Known", f = "Known", u = "Unknown")[x]

#3.4.2手动匹配与合并(整数取子集操作)
grades <- c(1, 2, 2, 3, 1)
info <- data.frame(grade = 3:1, desc = c("Excellent", "Good", "Poor"), fail = c(F, F, T))
# 使用match
id <- match(grades, info$grade);info[id, ]
# 使用rownames
rownames(info) <- info$grade
info[as.character(grades), ]

#多列需要匹配，你可能需要首先把它们压缩到
#interaction()、paste()或plyr::id())。 你也可以使用merge()或plyr::join()，

#3.4.3 随机采样/自助法
df <- data.frame(x = rep(1:3, each = 2), y = 6:1, z = letters[1:6])
# 随机重排序 
df[sample(nrow(df)), ]
# 随机选择3行 
df[sample(nrow(df), 3), ]
# 有放回抽样选择6行 
df[sample(nrow(df), 6, rep = T), ]

#3.4.4 排序(整数取子集操作)
x <- c("b", "c", "a"); order(x)
#使用decreasing = TRUE na.last = NA来删除缺失值或设置na.last = FALSE把缺失值排在向量的开头
# 对df进行随机重排序
df2 <- df[sample(nrow(df)), 3:1];df2
df2[order(df2$x), ];df2[, order(names(df2))]

#3.4.5 展开聚合的数据(整数取子集操作)
df <- data.frame(x = c(2, 4, 1), y = c(9, 11, 6), n = c(3, 5, 1)) 
rep(1:nrow(df), df$n)
df[rep(1:nrow(df), df$n), ]

#3.4.6 从数据框中删除列(字符取子集操作)
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3]) 
df$z <- NULL

#取出只需要的
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3]);df[c("x", "y")]

#使用集合操作计算出来
df[setdiff(names(df), "z")]

#3.4.7 基于某些条件选择行(逻辑取子集操作)
mtcars[mtcars$gear == 5, ]
mtcars[mtcars$gear == 5 & mtcars$cyl == 4, ]

#要使用向量的布尔操作符&和|，而不是有短路功能的标量操作符&&和||，后者是适用于if语句的。 不要忘了德摩根法则
subset(mtcars, gear == 5)
subset(mtcars, gear == 5 & cyl == 4)

#3.4.8 布尔代数与集合(逻辑和整数取子集操作)
x <- sample(10) < 4;which(x)

unwhich <- function(x, n) { out <- rep_len(FALSE, n); out[x] <- TRUE; out }
unwhich(which(x), 10)
(x1 <- 1:10 %% 2 == 0)
(x2 <- which(x1))
(y1 <- 1:10 %% 5 == 0)
(y2 <- which(y1))
x1 & y1
intersect(x2, y2)
x1 | y1;union(x2, y2);x1 & !y1;setdiff(x2, y2);xor(x1, y1);setdiff(union(x2, y2), intersect(x2, y2))


#--------------------------------------------------------函数--------------------------------#
f <- function(x) x^2
#函数体
body(f)
#形式参数列表
formals(f)
#环境
environment(f)

#原语函数(Primitive functions)，比如SUM它们都是C和C++写的，其函数体还有形式参数，环境都是空
objs <- mget(ls("package:base"), inherits = TRUE)
funs <- Filter(is.function, objs)
#codetools包
x <- 10; y <- 5
x + y
for (i in 1:2) print(i)
`for`(i, 1:2, print(i))  #顺序不同时的写法
if (i == 1) print("yes!") else print("no.")
`if`(i == 1, print("yes!"), print("no."))
x[3]
`[`(x, 3)

#应用函数
add <- function(x, y) x + y 
sapply(1:10, add, 3)  #在每个元素上面加上3
sapply(1:5, `+`, 3)
sapply(1:5, "+", 3)

  
x <- list(1:3, 4:9, 10:12) 
sapply(x, "[", 2) 
#> [1] 2 5 11 
# 相当于 
sapply(x, function(x) x[2]) 
#> [1] 2 5 11

mean(1:10) 
mean(1:10, trim = 0.05)

#6.4.2 给定一个参数列表来调用函数
args <- list(1:10, na.rm = TRUE)
do.call(mean, list(1:10, na.rm = TRUE))
# 相当于
mean(1:10, na.rm = TRUE)

#6.4.3 默认参数与缺失参数
f <- function(a = 1, b = 2) { c(a, b) }

#使用missing()函数来确定某个参数是否已经提供了
i <- function(a, b) { c(missing(a), missing(b)) }
i()

#6.4.4 延迟计算

f <- function(x){ 10 };
f(stop("This is an error!"))

#如果你想确保参数被计算过了，那么你可以使用force()函数：
f <- function(x){ 
  force(x) 
  10} 
f(stop("This is an error!"))

#当使用lapply()创建闭环还是循环时候，是非常重要的
add <- function(x){ 
  function(y) x + y 
  } 
adders <- lapply(1:10, add) 
adders[[1]](10)

x <- NULL 
if(!is.null(x) && x > 0){ }

#自己实现一个&&运算
`&&` <- function(x, y){ 
  if(!x) return(FALSE) 
  if(!y) return(FALSE) 
  TRUE 
  } 
  a <- NULL 
  !is.null(a) && a > 0

#6.5 特殊调用-----------------中缀函数还有替换函数
#%%、%*%、%/%、%in%、%o%、%x%。
  `%+%` <- function(a, b) paste(a, b, sep = "")
#中缀函数的名字比普通R函数更加灵活
  `% %` <- function(a, b) paste(a, b) 
  `%'%` <- function(a, b) paste(a, b) 
  `%/\\%` <- function(a, b) paste(a, b)
  `%||%` <- function(a, b) if (!is.null(a)) a else b


#---------------------------6.6.1 退出时---------------------------#
#on.exit()函数，设置其它的触发动作。
in_dir <- function(dir, code){ 
  old <- setwd(dir) 
  on.exit(setwd(old)) 
  force(code) 
  } 
getwd()
in_dir("~", getwd())

#7.1 基本类型
# 函数的类型是"closure"
f <- function(){} 
typeof(f)
is.function(f)
typeof(sum)
is.primitive(sum)

#7.2 S3
#7.2.1 认识对象、泛型函数和方法
is.object(x) & !isS4(x)
#pryr::otype()：

library(pryr) 
df <- data.frame(x = 1:10, y = letters[1:10]) 
otype(df)
otype(df$x)
otype(df$y)

#调用UseMethod()函数查看它的源代码
#pryr还提供了ftype()
#大多数的现代编程风格指南，都不鼓励把.用在函数名中
#使用methods()查看所有属于某个泛型函数的方法
methods("mean");methods("t.test")
#getS3method()阅读它们的源代码主要是BASE包的数据
methods(class = "ts")

#-------------7.2.2 定义类与创建对象---------------#
foo <- structure(list(), class = "foo")
# 先创建，然后再设置类
foo <- list() 
class(foo) <- "foo"
class(foo)
#是否继承
inherits(foo, "foo")

foo <- function(x) { if (!is.numeric(x)) stop("X must be numeric") structure(list(x), class = "foo") }

#-------------7.2.3 创建新方法和泛型函数--------------#
#要添加一个新的泛型函数，可以创建一个函数，然后调用UseMethod()
f <- function(x) UseMethod("f")
f.a <- function(x) "Class a" 
a <- structure(list(), class = "a") 
class(a)
mean.a <- function(x) "a" 
mean(a)

#--------------7.3 S4---------------------#
library(stats4)
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
nLL <- function(lambda) - sum(dpois(y, lambda, log = TRUE))
fit <- mle(nLL, start = list(lambda = 5), nobs = length(y))
# 一个S4对象
isS4(fit)
otype(fit)
isS4(nobs)
ftype(nobs)
mle_nobs <- method_from_call(nobs(fit))
isS4(mle_nobs)
ftype(mle_nobs)

is(fit)
is(fit, "mle")
#getGenerics()得到所有S4泛型函数的列表，可以使用getclass()得到

#------------7.3.2 定义类与创建对象
#必须使用setClass()定义一个类的表示方式
#UpperCamelCase风格的名称
list(name = "character", age = "numeric")

#创建一个类
setClass("Person",
         slots = list(name = "character", age = "numeric"))
setClass("Employee",
         slots = list(boss = "Person"),contains = "Person")
alice <- new("Person", name = "Alice", age = 40)
john <- new("Employee", name = "John", age = 20, boss = alice)
#可以使用@或slot()来访问S4对象的



  