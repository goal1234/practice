  #����
y <- 1:10 
attr(y, "my_attribute") <- "This is a vector" 
attr(y, "my_attribute")
str(attributes(y))

structure(1:10, my_attribute = "This is a vector")
attributes(y[1])  #���޸�����ʱ����������Իᶪʧ
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

# �㲻��ʹ��levels��û�е�ֵ
x[2] <- "c"
#������������
c(factor("a"), factor("b"))

#---���ӵ��÷�---
sex_char <- c("m", "m", "m") 
sex_factor <- factor(sex_char, levels = c("m", "f")) 
table(sex_char);table(sex_factor)

#read.csv()��ʹ��na.strings
z <- read.csv(text = "value\n12\n1\n.\n9") 
typeof(z$value)
as.double(z$value)
class(z$value)
as.double(as.character(z$value))

z <- read.csv(text = "value\n12\n1\n.\n9", na.strings=".") 
typeof(z$value);class(z$value)

#---------------------------------���������---------------------------
# ������������ָ�����к��� 
a <- matrix(1:6, ncol = 3, nrow = 2) 
# һ�����������������е�ά�� 
b <- array(1:12, c(2, 3, 2)) 
# ��Ҳ����ͨ������dim()�͵��޸�һ������ 
c <- 1:6 dim(c) <- c(3, 2);c
#С�ķ���
length(a);nrow(a);ncol(a)
rownames(a) <- c("A", "B")
colnames(a) <- c("a", "b", "c")
dimnames(b) <- list(c("one", "two"), c("a", "b", "c"), c("A", "B"))


str(1:3) # һά����
str(matrix(1:3, ncol = 1)) # ������
str(matrix(1:3, nrow = 1)) # ������
str(array(1:3, 3)) # "����"("array")����


#�б�ת��
l <- list(1:3, "a", TRUE, 1.0)
dim(l) <- c(2, 2)

#-----------------------------���ݿ�-----------------------------#
df <- data.frame(x = 1:3, y = c("a", "b", "c")) 
str(df)
df <- data.frame( x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE);str(df)

#���Ժ�ǿ��ת��
typeof(df);class(df);is.data.frame(df)

#�������ݿ�cbind() rbind()��ʱ�У���Ӧ�������
cbind(df, data.frame(z = 3:1))
rbind(df, data.frame(x = 10, y = "z"))

#ʹ��plyr::rbind.fill()��������������ͬ�����ݿ�
bad <- data.frame(cbind(a = 1:2, b = c("a", "b")));str(bad)
good <- data.frame(a = 1:2, b = c("a", "b"), stringsAsFactors = FALSE);str(good)

#---������---#
df <- data.frame(x = 1:3) 
df$y <- list(1:2, 1:3, 1:4);df

#��������Ϊ���ݿ��һ��
dfl <- data.frame(x = 1:3, y = I(list(1:2, 1:3, 1:4)));str(dfl)
dfm <- data.frame(x = 1:3, y = I(matrix(1:9, nrow = 3)));str(dfm)

#3ȡ�Ӽ�����
#3.1.1ԭ������
x <- c(2.1, 4.2, 3.3, 5.4)

#����������λ��
x[c(3, 1)]
x[order(x)]
x[c(1, 1)]  #�õ��ظ�����
x[c(2.1, 2.9)]# ʵ���ᱻ��ʽ�ؽضϳ�����

#���������Ը�λ�õ�Ԫ��
x[-c(3, 1)]
x[c(-1, 2)]  #���ܽ���������ϲ���

#�߼�����ѡ����Ӧλ��ΪTRUE��Ԫ��
x[c(TRUE, TRUE, FALSE, FALSE)]
x[x > 3]
x[c(TRUE, FALSE)]# �൱��
x[c(TRUE, FALSE, TRUE, FALSE)]
#ȱʧֵ����������в���ȱʧֵ��
x[c(TRUE, TRUE, NA, FALSE)]
x[]
x[0]  #0�����㳤�ȵ�����

(y <- setNames(x, letters[1:4]))
# ��������������������ظ������� 
y[c("a", "a", "a")]

#3.1.3���������
a <- matrix(1:9, nrow = 3) 
colnames(a) <- c("A", "B", "C")
a[1:2, ];a[c(T, F, T), c("B", "A")]
a[0, -2]
(vals <- outer(1:5, 1:5, FUN = "paste", sep = ","))
vals[c(4, 15)]

#3.1.4���ݿ�
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3]) 
df[df$x == 2, ]
df[c(1, 3), ]
df[c("x", "z")];df[, c("x", "z")]
#��
str(df["x"]);str(df[, "x"])  #ʹ��drop = FALSE�������Ա�����һ�㡣

#ȡ�Ӽ�������
#[[��$
#����ĳ��ֵ
a <- list(a = 1, b = 2);a[[1]]
## ������ṩ������������ô����еݹ�����
b <- list(a = list(b = list(c = list(d = 1)))) 
b[[c("a", "b", "c", "d")]]
# ��������ͬ
b[["a"]][["b"]][["c"]][["d"]]

#ȡ�Ӽ���ʽ�����뱣��
var <- "cyl" # ������ - 
mtcars$var #�����ͳ�mtcars[["var"]] mtcars$

#����ѯ
x <- c("m", "f", "u", "f", "f", "m", "m") 
lookup <- c(m = "Male", f = "Female", u = NA)
unname(lookup[x])

## ���߸��ٵ����ֵ
c(m = "Known", f = "Known", u = "Unknown")[x]

#3.4.2�ֶ�ƥ����ϲ�(����ȡ�Ӽ�����)
grades <- c(1, 2, 2, 3, 1)
info <- data.frame(grade = 3:1, desc = c("Excellent", "Good", "Poor"), fail = c(F, F, T))
# ʹ��match
id <- match(grades, info$grade);info[id, ]
# ʹ��rownames
rownames(info) <- info$grade
info[as.character(grades), ]

#������Ҫƥ�䣬�������Ҫ���Ȱ�����ѹ����
#interaction()��paste()��plyr::id())�� ��Ҳ����ʹ��merge()��plyr::join()��

#3.4.3 �������/������
df <- data.frame(x = rep(1:3, each = 2), y = 6:1, z = letters[1:6])
# ��������� 
df[sample(nrow(df)), ]
# ���ѡ��3�� 
df[sample(nrow(df), 3), ]
# �зŻس���ѡ��6�� 
df[sample(nrow(df), 6, rep = T), ]

#3.4.4 ����(����ȡ�Ӽ�����)
x <- c("b", "c", "a"); order(x)
#ʹ��decreasing = TRUE na.last = NA��ɾ��ȱʧֵ������na.last = FALSE��ȱʧֵ���������Ŀ�ͷ
# ��df�������������
df2 <- df[sample(nrow(df)), 3:1];df2
df2[order(df2$x), ];df2[, order(names(df2))]

#3.4.5 չ���ۺϵ�����(����ȡ�Ӽ�����)
df <- data.frame(x = c(2, 4, 1), y = c(9, 11, 6), n = c(3, 5, 1)) 
rep(1:nrow(df), df$n)
df[rep(1:nrow(df), df$n), ]

#3.4.6 �����ݿ���ɾ����(�ַ�ȡ�Ӽ�����)
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3]) 
df$z <- NULL

#ȡ��ֻ��Ҫ��
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3]);df[c("x", "y")]

#ʹ�ü��ϲ����������
df[setdiff(names(df), "z")]

#3.4.7 ����ĳЩ����ѡ����(�߼�ȡ�Ӽ�����)
mtcars[mtcars$gear == 5, ]
mtcars[mtcars$gear == 5 & mtcars$cyl == 4, ]

#Ҫʹ�������Ĳ���������&��|���������ж�·���ܵı���������&&��||��������������if���ġ� ��Ҫ���˵�Ħ������
subset(mtcars, gear == 5)
subset(mtcars, gear == 5 & cyl == 4)

#3.4.8 ���������뼯��(�߼�������ȡ�Ӽ�����)
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


#--------------------------------------------------------����--------------------------------#
f <- function(x) x^2
#������
body(f)
#��ʽ�����б�
formals(f)
#����
environment(f)

#ԭ�ﺯ��(Primitive functions)������SUM���Ƕ���C��C++д�ģ��亯���廹����ʽ�������������ǿ�
objs <- mget(ls("package:base"), inherits = TRUE)
funs <- Filter(is.function, objs)
#codetools��
x <- 10; y <- 5
x + y
for (i in 1:2) print(i)
`for`(i, 1:2, print(i))  #˳��ͬʱ��д��
if (i == 1) print("yes!") else print("no.")
`if`(i == 1, print("yes!"), print("no."))
x[3]
`[`(x, 3)

#Ӧ�ú���
add <- function(x, y) x + y 
sapply(1:10, add, 3)  #��ÿ��Ԫ���������3
sapply(1:5, `+`, 3)
sapply(1:5, "+", 3)

  
x <- list(1:3, 4:9, 10:12) 
sapply(x, "[", 2) 
#> [1] 2 5 11 
# �൱�� 
sapply(x, function(x) x[2]) 
#> [1] 2 5 11

mean(1:10) 
mean(1:10, trim = 0.05)

#6.4.2 ����һ�������б������ú���
args <- list(1:10, na.rm = TRUE)
do.call(mean, list(1:10, na.rm = TRUE))
# �൱��
mean(1:10, na.rm = TRUE)

#6.4.3 Ĭ�ϲ�����ȱʧ����
f <- function(a = 1, b = 2) { c(a, b) }

#ʹ��missing()������ȷ��ĳ�������Ƿ��Ѿ��ṩ��
i <- function(a, b) { c(missing(a), missing(b)) }
i()

#6.4.4 �ӳټ���

f <- function(x){ 10 };
f(stop("This is an error!"))

#�������ȷ��������������ˣ���ô�����ʹ��force()������
f <- function(x){ 
  force(x) 
  10} 
f(stop("This is an error!"))

#��ʹ��lapply()�����ջ�����ѭ��ʱ���Ƿǳ���Ҫ��
add <- function(x){ 
  function(y) x + y 
  } 
adders <- lapply(1:10, add) 
adders[[1]](10)

x <- NULL 
if(!is.null(x) && x > 0){ }

#�Լ�ʵ��һ��&&����
`&&` <- function(x, y){ 
  if(!x) return(FALSE) 
  if(!y) return(FALSE) 
  TRUE 
  } 
  a <- NULL 
  !is.null(a) && a > 0

#6.5 �������-----------------��׺���������滻����
#%%��%*%��%/%��%in%��%o%��%x%��
  `%+%` <- function(a, b) paste(a, b, sep = "")
#��׺���������ֱ���ͨR�����������
  `% %` <- function(a, b) paste(a, b) 
  `%'%` <- function(a, b) paste(a, b) 
  `%/\\%` <- function(a, b) paste(a, b)
  `%||%` <- function(a, b) if (!is.null(a)) a else b


#---------------------------6.6.1 �˳�ʱ---------------------------#
#on.exit()���������������Ĵ���������
in_dir <- function(dir, code){ 
  old <- setwd(dir) 
  on.exit(setwd(old)) 
  force(code) 
  } 
getwd()
in_dir("~", getwd())

#7.1 ��������
# ������������"closure"
f <- function(){} 
typeof(f)
is.function(f)
typeof(sum)
is.primitive(sum)

#7.2 S3
#7.2.1 ��ʶ���󡢷��ͺ����ͷ���
is.object(x) & !isS4(x)
#pryr::otype()��

library(pryr) 
df <- data.frame(x = 1:10, y = letters[1:10]) 
otype(df)
otype(df$x)
otype(df$y)

#����UseMethod()�����鿴����Դ����
#pryr���ṩ��ftype()
#��������ִ���̷��ָ�ϣ�����������.���ں�������
#ʹ��methods()�鿴��������ĳ�����ͺ����ķ���
methods("mean");methods("t.test")
#getS3method()�Ķ����ǵ�Դ������Ҫ��BASE��������
methods(class = "ts")

#-------------7.2.2 �������봴������---------------#
foo <- structure(list(), class = "foo")
# �ȴ�����Ȼ����������
foo <- list() 
class(foo) <- "foo"
class(foo)
#�Ƿ�̳�
inherits(foo, "foo")

foo <- function(x) { if (!is.numeric(x)) stop("X must be numeric") structure(list(x), class = "foo") }

#-------------7.2.3 �����·����ͷ��ͺ���--------------#
#Ҫ����һ���µķ��ͺ��������Դ���һ��������Ȼ�����UseMethod()
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
# һ��S4����
isS4(fit)
otype(fit)
isS4(nobs)
ftype(nobs)
mle_nobs <- method_from_call(nobs(fit))
isS4(mle_nobs)
ftype(mle_nobs)

is(fit)
is(fit, "mle")
#getGenerics()�õ�����S4���ͺ������б�������ʹ��getclass()�õ�

#------------7.3.2 �������봴������
#����ʹ��setClass()����һ����ı�ʾ��ʽ
#UpperCamelCase��������
list(name = "character", age = "numeric")

#����һ����
setClass("Person",
         slots = list(name = "character", age = "numeric"))
setClass("Employee",
         slots = list(boss = "Person"),contains = "Person")
alice <- new("Person", name = "Alice", age = 40)
john <- new("Employee", name = "John", age = 20, boss = alice)
#����ʹ��@��slot()������S4�����



  