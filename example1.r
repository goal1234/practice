
### data:2015-11-7 ###
### author:laidefa ### 

library(XML)
library(RCurl)
loginURL<-"http://hz.zu.anjuke.com/fangyuan/jiubao/"
cookieFile<-"E://cookies.txt"
loginCurl<-getCurlHandle(followlocation=TRUE,verbose=TRUE,ssl.verifyhost=FALSE,
ssl.verifypeer=FALSE,cookiejar=cookieFile,cookiefile=cookieFile)

#获取第一页的url
web<-getURL(loginURL,curl=loginCurl)

#获取第2-10页的url
url_list = ""
i=1:9
url_list[i]<-paste0('http://hz.zu.anjuke.com/fangyuan/jiubao/p',i+1,'/')

#循环读取url
for(url in url_list){
web1 <-getURL(url,curl=loginCurl)
web<-rbind(web,web1)
}

#解析url树结构
doc<-htmlParse(web)
zufang_title<-sapply(getNodeSet(doc,"//div[@class='zu-info']//h3//a[@title]"),xmlValue)
type<-sapply(getNodeSet(doc,"//div[@class='zu-info']//p[1]"),xmlValue)
address<-sapply(getNodeSet(doc,"//div[@class='zu-info']//address"),xmlValue)
address<-substring(address,34)
price<-sapply(getNodeSet(doc,"//div[@class='zu-side']//p[1]"),xmlValue)
price<-price[-seq(53,520,53)]
contacts<-sapply(getNodeSet(doc,"//p[@class='bot-tag']//span[1]"),xmlValue)

#合并数据框
data<-data.frame(zufang_title,price[-503],type,contacts,address)
View(data)



# -------------------------------- #
# -------------------------------- #
install.packages("RCurl")  
library(RCurl)  
url1='http://shenzhen.lashou.com/cate/meishi'  
web=readLines(url1,encoding='UTF-8')  
goods_name <- web[grep("goods-name",web)]  
goods_name2 <- substr(goods_name,regexpr("\">",goods_name)+2,nchar(goods_name)-4)    
goods_name2  
  
i=2  
goods_name[i]  
substr(goods_name[i],regexpr("\">",goods_name[i])+2,nchar(goods_name[i])-4)    
web[1:10] 


# ---------------------------- #
# ---------------------------- #

#　R语言爬虫之——RCurl
# RCurl三大函数

getURL()
getForm()
postForm()

# 判断url是否存在
url.exists(url="www.baidu.com") # 判断url是否存在
# [1] TRUE
d <- debugGatherer() #收集调试信息
# verbose = TRUE 这时候，d$value()值是会叠加的
tmp <- getURL(url="www.baidu.com", debugfunction = d$update, verbose = TRUE)  

names(d$value())

cat(d$value()[1]) #服务器地址及端口号
cat(d$value()[2]) #服务器返回的头信息
cat(d$value()[3]) #提交给服务器的头信息
d$reset() # 清除d$value()
d$value() # 清除之后全部为空

# 查看服务器返回的头信息
## 列表形式
h <- basicHeaderGatherer()
txt <- getURL(url="http://www.baidu.com", headerfunction = h$update)
names(h$value())
h$value()

# 查看服务器返回的头信息
## 字符串形式
h <- basicTextGatherer()
txt <- getURL("http://www.baidu.com", headerfunction = h$update)
names(h$value())
# NULL # 说明是字符串形式，没有列
h$value() # 所有的内容只是一个字符串

cat(h$value()) # 用cat显示的，会比较好看

# 查看url请求的访问信息
curl <- getCurlHandle()
txt <- getURL(url="http://www.baidu.com", curl = curl)
names(getCurlInfo(curl))

getCurlInfo(curl)$response.code
# [1] 200
getCurlInfo(curl=curl)
$effective.url
$response.code
$total.time
$namelookup.time


# --- 设置自己的header --- #
# 设置自己的header，把系统设置成ihpone的系统Mac OS
myheader <- c(
  "User-Agent"="Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_0_1 like Mac OS X; ja-jp) AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8A306 Safari/6531.22.7",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
)

d <- debugGatherer()
tmp <- getURL(url = "http://www.baidu.com", httpheader = myheader, debugfunction = d$update, verbose = T)

cat(d$value()[3]) # 提交给服务器的头信息，发现设置成功
GET / HTTP/1.1
Host: www.baidu.com
User-Agent: Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_0_1 like Mac OS X; ja-jp) AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8A306 Safari/6531.22.7
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: en-us
Connection: keep-alive
Accept-Charset: GB2312,utf-8;q=0.7,*;q=0.7

# 设置其他参数，共174个参数
listCurlOptions()



#---------------
# getForm()
# getForm()函数

# 在百度里面搜索“rcurl”的url为（浏览器为google chrome）：
url <- c("http://www.baidu.com/s?ie=utf-8&f=8&rsv_bp=1&rsv_idx=2&ch=&tn=SE_hldp02870
_0v135xhf&bar=&wd=rcurl&rsv_spt=1&rsv_pq=a3ed162a0088df8f&rsv_t=43d18gWNyd6HWpqDiKov7Dm
548s4HY4cgcJlXc8ujpzRW9Okec2aOb5screzftZo5DJ60Cp7aILvRK2Q&rsv_enter=1&inputT=2119")
# wd=rcurl 这里就是关键字为rcurl

getFormParams(query=url) # 查看url的结构和值
names(getFormParams(query=url))
[1] "ie"        "f"         "rsv_bp"    "rsv_idx"   "ch"        "tn"        "bar"       "wd"        "rsv_spt"  
[10] "rsv_pq"    "rsv_t"     "rsv_enter" "inputT"   

tmp <- getForm(uri="http://www.baidu.com/s", ie="utf-8", f="8", rsv_bp="1", rsv_idx="2", ch="", tn="SE_hldp02870_0v135xhf", bar="", wd="rcurl", rsv_spt="1", rsv_pq="a3ed162a0088df8f", rsv_t="43d18gWNyd6HWpqDiKov7Dm548s4HY4cgcJlXc8ujpzRW9Okec2aOb5screzftZo5DJ60Cp7aILvRK2Q", rsv_enter="1", inputT="2119")

# 这里的getForm函数不稳定(原因还不知道)，有时候运行2到3次，才能真正找到页面
# 出来的错误的结果，爬取的页面为：
attr(,"Content-Type")

# getBinaryURL() 下载一个文件
url <- "http://rfunction.com/code/1201/120103.R"
tmp <- getBinaryURL(url)
note <- file("120103.R", open = "wb")
writeBin(tmp, note)
close(note)


# getBinaryURL() 批量下载文件
url <- "http://rfunction.com/code/1202/"
tmp <- RCurl::getURL(url, httpheader = myheader) # 获取网页

tmp_files <- strsplit(x=tmp, split="<li><a href=\"")[[1]]
tmp_files1 <- strsplit(tmp_files, split="\"")
tmp_files2 <- lapply(X=tmp_files1, function(file) {file[1]})
files <- unlist(tmp_files2)
files <- files[c(-1, -2)]

baseURL <- "http://rfunction.com/code/1202/"
for(i in 1:length(files)){
  fullURL <- paste(baseURL, files[i], sep = "")
  tmp <- getBinaryURL(fullURL)
  note <- file(paste("1202-", files[i], sep = ""), open = "wb")
  writeBin(tmp, note)
  close(note)

  Sys.sleep(2) # 休眠2秒
}


# XML
# XML简介
# 缺点：在windows下对中文支持不理想（我在ubuntu下也不理想）
library(XML)
url <- "http://data.earthquake.cn/datashare/datashare_more_quickdata_new.jsp" # 中文界面，抓出来是乱码
url <- "http://219.143.71.11/wdc4seis@bj/earthquakes/csn_quakes_p001.jsp" # 英文界面，抓出来是对的
wp <- getURL(url)
doc <-htmlParse(wp, asText = TRUE) # 这里切记encoding  
tables <- readHTMLTable(doc, header=F, which = 2)
# 选取第二个表
head(tables)


# ----------------------------------------------------- #
# ----------------------------------------------------- #

# R的爬虫和回归模型案例
library(rvest)
library(stringr)
library(XML)
library(xml2)

WebSpider <- function(m){
  url <- str_c(cp,"?p=",m)
  web <- read_html(url,encoding = "UTF-8")#抓取网页信息
  name_rough <- web %>% html_nodes("h3")  %>%  html_text() #获取粗房屋名
  area_rough <- web %>% html_nodes("h4")  %>% html_text() #提取区位
  price_rough <- web %>% html_nodes("p.price")  %>% html_text() #提取价格
  price <- str_extract(price_rough, "[0-9]+") %>% as.numeric()#提取精确价格
  detail <- web %>% html_nodes("div.detail")  %>%  html_text() #提取其他信息
  #合并成数据框
  data.frame(name_rough,area_rough,forward,mate_num,location,price,detail)
}

dc <- "http://www.ziroom.com/z/nl/z3-d23008614.html"
xc <- "http://www.ziroom.com/z/nl/z3-d23008626.html"
cy <- "http://www.ziroom.com/z/nl/z3-d23008613.html"
hd <- "http://www.ziroom.com/z/nl/z3-d23008618.html"
ft <- "http://www.ziroom.com/z/nl/z3-d23008617.html"
sjs <- "http://www.ziroom.com/z/nl/z3-d23008623.html"
cp <- "http://www.ziroom.com/z/nl/z3-d23008611.html"


#　--- 只有逐区的来进行翻页爬了。为此，只能选定部分区域来做分析了。 --- #
results_cp <- data.frame()
for(m in 1:118){ #118为昌平区信息的总页码 
  results_cp <- rbind(results_cp,WebSpider(m))#合并单个区每一次循环输出的数据
}

#依次重复获得7个区的数据
results <- rbind(results_cp, results_cy,results_dc,results_ft,
                 results_hd,results_sjs,results_xc) #将所有各区数据的合并


# --- 数据清洗和考察 --- #
library(readxl)
library(stringr)
library(dplyr)
library(psych)
library(ggplot2)
library(nortest)
library(gridExtra)
library(mice)
library(VIM)
library(corrplot)
library(DMwR)
library(car)

zr_data <- read_excel("~/Desktop/Über R/Excesise/ziroom-excel-V4.xlsx")

zr_data$method_of_pay <- str_replace(zr_data$method_of_pay, "[M]", "1")
zr_data$method_of_pay <- str_replace(zr_data$method_of_pay, "[D]", "2")%>% as.numeric()

# subset the data of pay monthly
zr_mp <- subset(zr_data, method_of_pay == "1")

# subset the data of pay dayly
zr_dp <- zr_data[zr_data$method_of_pay == "2", ]

# subset the data of cotenant from monthly paying
zr_mp_c <- filter(zr_mp, method_of_rent == "合")

#group each variable by the data types
norminal_data <- list(zr_mp_c$forward, zr_mp_c$district, zr_mp_c$area, zr_mp_c$layout)
ordinal_data <- list(zr_mp_c$stock)
ratio_data <- list(zr_mp_c$price, zr_mp_c$area_space, zr_mp_c$distance_from_ss, zr_mp_c$stock)


# 单变量探索
h1 <- ggplot(zr_mp_c, aes(x = price, y = ..density..)) + 
  geom_histogram(binwidth = 100) + 
  geom_density()+
  labs(title = "房租价格")+
  theme(text = element_text(family = "STSong"))
h2 <- ggplot(zr_mp_c, aes(x = area_space, y = ..density..)) + 
  geom_histogram(binwidth = 1) + 
  geom_density()+
  labs(title = "房屋面积")+
  theme(text = element_text(family = "STSong")) 
h3 <- ggplot(zr_mp_c, aes(x = distance_from_ss, y = ..density..)) + 
  geom_histogram(binwidth = 50) + 
  geom_density()+
  labs(title = "距离地铁远近")+
  theme(text = element_text(family = "STSong"))
h4 <- ggplot(zr_mp_c, aes(x = stock, y = ..density..)) + 
  geom_histogram(binwidth = 1) + 
  geom_density()+
  labs(title = "楼层分布")+
  theme(text = element_text(family = "STSong"))

grid.arrange(h1, h2, h3, h4, nrow = 2)

sapply(ratio_data, lillie.test)
lillie.test(zr_mp_c$stock)


# --- 缺失值考察 --- #
#construct as the final data frame
zr_mp_c_a <- subset(zr_mp_c, select = c("price", "area_space", "distance_from_ss", "stock", "forward", "district", "area", "layout"))
#form the new stock variable
table(stock2 <- cut(zr_mp_c_a$stock,breaks = c(1,6,12,20,33), 
                    labels = c("low","low midlle","high middle",
                    "high")))

        low  low midlle high middle        high 
       4697        2136        1508         630 
zr_mp_c_a$stock2 <- stock2

#transform the factor variables
zr_mp_c_a$forward <- factor(zr_mp_c_a$forward)
zr_mp_c_a$district <- factor(zr_mp_c_a$district)
zr_mp_c_a$area <- factor(zr_mp_c_a$area)
zr_mp_c_a$layout  <- factor(zr_mp_c_a$layout)

sum(is.na(zr_mp_c_a$price))
sum(is.na(zr_mp_c_a$distance_from_ss))
sum(is.na(zr_mp_c_a$stock))
sum(is.na(zr_mp_c_a$stock2))
sum(is.na(zr_mp_c_a$area_space))
sum(is.na(zr_mp_c_a$forward))
sum(is.na(zr_mp_c_a$district))
sum(is.na(zr_mp_c_a$area))
sum(is.na(zr_mp_c_a$layout))

matrixplot(zr_mp_c_a)

micemod <- mice(zr_mp_c_a[1:4]))
zr_mp_c_a[1:4] <- complete(micemod)


# 变量间关系的探索
# 可以使用psych包的corr.test一次性的完成相关程度和显著性的检验。
corr.test(zr_mp_c_a[ ,1:4])

# 四、模型建构
fit1 <-  lm(price ~ ., data = zr_mp_c_a)
#compare the influence of "area" and "districk"
fit2 <-  lm(price ~ area_space + distance_from_ss + forward + stock + area + layout, data = zr_mp_c_a)
fit3 <-  lm(price ~ area_space + distance_from_ss + forward + stock +
              district + layout, data = zr_mp_c_a)
#drop "stock" and "area"
fit4 <-  lm(price ~ area_space + distance_from_ss + forward + district+ layout, data = zr_mp_c_a)

opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))
plot(fit4)
par(opar)


# --- 对照模型拟合情况诊断图，逐一来看 --- #
durbinWatsonTest(fit4)
fit4 <- lm(log(price) ~ area_space + distance_from_ss + forward + district+
               layout, data = zr_mp_c_a)

# 诊断多重共线情况
vif(fit4)
sqrt(vif(fit4)) > 2
