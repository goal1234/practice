
# R语言实现网络爬虫
#数据准备
library(xml2)
library(rvest)
library(downloader)
library(stringr)
library(dplyr)

#R代码
setwd("F:/download")#新建工作路径
ur <- "https://pic2.zhimg.com/v2-da7f9cc3e99f701f5171781fff8448b1_r.jpg"
download(url,"F:/download/picture.jpg",mode="wb")

#批量下载图片

htp <-read_html('https://www.zhihu.com/question/37839997')#读取网页网址
links<-htp %>%
           html_nodes("span.RichText.CopyrightRichText-richText")%>%
           html_nodes("img")%>%
html_attr("src")


#查看links对象
head(links)

useful<-"https" # 筛选带“https”的字符串
link<-grep(useful, links, value = TRUE)
for(i in 1:length(link)){
    download(link[i],paste("F:download/picture",i,".jpg",sep = ""),mode="wb")
}

# ----
#数据准备
install.packages('tidyverse')
library(tidyverse)
library(rvest)
library(stringr)

url<-"//gslb.miaopai.com/stream/7zbNUHeo2BoSoMv-EB6rZrrHq6noLjarp~nmqg__.mp4"
url<-paste0("http:",url) # 链接添加协议名称

# 下载视频
download.file(url,"F:/download/food.mp4",mode="wb") 

#读取网页链接
htp<-read_html('http://www.miaopai.com/u/paike_8o7ugjvf5c')
links<-htp %>%
        html_nodes("div.video-player") %>%
        html_nodes("viedo") %>%
        html_attr("scr")

links<-htp %>%
        html_nodes("div.MIAOPAI_player")%>%
        html_attr("data-scid")

links #查看links内容

link<-paste0("http://gslb.miaopai.com/stream/", links, ".mp4")
#尝试一下给视频取个中文名字哈
names<-c()
names=paste0(rep("美食视频",5),1:5,".mp4")

#下载视频
for(i in 1:length(link)){
    download.file(link[i],names[i],mode="wb")
}

 #不取中文名字下载视频：

for(i in 1:length(link)){
    download.file(link[i],paste("F:/download/food",i,".mp4"),mode="wb")
}



#------------------------------------------------------#
#------------------------------------------------------#

library(RCurl)
getcoments <- function(i){
    productid <- '2967929'  #商品id
    t1 <- 'http://club.jd.com/comment/productPageComments.action?productId='
    t2 <- '&score=0&sortType=1&page='  #按时间顺序
    t3 <- '&pageSize=1' #设置每页1条评论
    url <-paste0(t1,productid,t2,i,t3)
        web <- getURL(url, .encoding = 'gbk')
        comments <- substr(web,regexpr("comments", web)+10,regexpr("referenceTime", web)-4)
        content <- substr(comments,regexpr("content", comments)+10,regexpr("creationTime", comments)-4)
}

comment <- c()
n <- 30 #爬取评论条数
for(i in 0:(n-1)){
    comment <- rbind(comment,getcoments(i))
    print(i+1)
    Sys.sleep(1)
}

write.csv(comment,'jingdongcomment.csv')


# ----------------------------------------- #
# ----------------------------------------- #

#lab1
library(XML);
url1<-"http://data.caixin.com/macro/macro_indicator_more.html?id=F0001&cpage=2&pageSize=30&url=macro_indicator_more.html#top";
url<-htmlParse(url1,encoding="UTF-8")#把html文件读入R语言中并解析

#找结点
test <- getNodeSet(url,'//meta[@name]')#xpath语法找到html部件#显示的中文正常

#读取结点的内容：xmlValue内部参数只能是一个字符串
test_text_list<-sapply(test, xmlValue)#提取内容，多个的化以向量形式存储
test_text<-xmlValue(test[[2]])#把test的第2个中的内容提取出来=test_text_list[2].注意，即时test只有一组数据也要使用test[[1]],不可以直接使用test（不是字符串）

#读取结点的属性：xmlGetAttr内部参数只能是一个字符串
content1<-xmlGetAttr(test[[1]], "content")#读取test[[1]]中的content内容。注意直接用test不可以。#显示的中文不正常
content1<-iconv(content1,"UTF-8","gbk")#解决中文正常显示问题


#lab2使用R语言爬取淘宝网站的笔记本商品价格和名称
library(XML)
url1<-"http://3c.taobao.com/detail.htm?spm=872.217037.254698.6.deIiSJ&spuid=205341228&cat=1101"
url2<-"http://3c.taobao.com/detail.htm?spm=872.217037.254698.11.deIiSJ&spuid=203228104&cat=1101"
read_taobao<-function(url){
    name_text<-""
    price_text<-""
    i<-1
    for(i_url in url){
        #读取html数据
        i_url2<-htmlParse(i_url,encoding="UTF-8")
        #通过xpath找到网页中的name
        name<- getNodeSet(i_url2,"//div[@id='idetail']//div[@class='info-area']//div[@class='tlt clearfix']//h1")
        
        #xpath://任意位置的  @是属性
        name_text_tmp<-xmlValue(name[[1]])
            
        #提取name的内容
        #通过xpath找到网页中的price
        price<-getNodeSet(i_url2,"//div[@id='idetail']//div[@class='info-area']//div[@class='key-info']//span[@class='price']")
        price_text_tmp<-xmlValue(price[[1]])
            
        #提取price的内容
        name_text[i]<-name_text_tmp
        price_text[i]<-price_text_tmp
        i<-i+1
    }
    data.frame(name=name_text,price=price_text)
}

url<-c(url1,url2)
read_taobao(url)


# ------------------------------------------------------ #
# ------------------------------------------------------ #

#　RCurl httr两个包

#


# ----------------------------------------------------- #
# ----------------------------------------------------- #

#　用rvest包来抓取Google学术搜索数据

library(rvest)
library(ggplot2)

page <- read_html("https://scholar.google.com/citations?user=sTR9SIQAAAAJ&hl=en&oi=ao")
citations <- page %>% html_nodes ("#gsc_a_b .gsc_a_c") %>% html_text()%>%as.numeric()

barplot(citations, main="How many times has each paper been cited?", 
                　　ylab='Number of citations', col="skyblue", xlab="")

page <- read_html("https://scholar.google.com/citations?view_op=list_colleagues&hl=en&user=sTR9SIQAAAAJ")
Coauthors = page%>% html_nodes(css=".gsc_1usr_name a") %>% html_text()
Coauthors = as.data.frame(Coauthors)
names(Coauthors)　=　'Coauthors'

head(Coauthors)

page <- read_html("https://scholar.google.com/citations?view_op=list_colleagues&hl=en&user=sTR9SIQAAAAJ")
citations = page　%>% html_nodes(css = ".gsc_1usr_cby")　%>%　html_text()

citations = gsub('Cited by','', citations)
citations = as.numeric(citations)
citations = as.data.frame(citations)

page <- read_html("https://scholar.google.com/citations?view_op=list_colleagues&hl=en&user=sTR9SIQAAAAJ")
affilation = page %>% html_nodes(css = ".gsc_1usr_aff")　%>%　html_text()
affilation = as.data.frame(affilation)
names(affilation)='Affilation'

cauthors=cbind(Coauthors, citations, affilation)
cauthors$Coauthors <- factor(cauthors$Coauthors, levels = cauthors$Coauthors[order(cauthors$citations, decreasing=F)])

ggplot(cauthors,aes(Coauthors,citations)) + geom_bar(stat="identity", fill="#ff8c1a",size=5)　+
theme(axis.title.y   = element_blank())　+　ylab("# of citations")　+
theme(plot.title=element_text(size = 18,colour="blue"), axis.text.y = element_text(colour="grey20",size=12))+
              ggtitle('Citations of his coauthors')　+　coord_flip()


# ------------------------------------------------------ #
# ------------------------------------------------------ #

library(quantmod)
stock=read.csv('F:/Program Files/RStudio/stockid.csv',stringsAsFactors=F)
data=list()
for(i in 1:length(stock$id)){
  try(setSymbolLookup(TEMP=list(name=paste0(stock$id[i],'.ss'))))
  try(getSymbols("TEMP",warnings=F))
  try(data[stock$name[i]]<-list(TEMP))
}

library(plyr)
closedata<-lapply(data,function(x){
  x=as.data.frame(x)
  return(list(x[,4])) #提取第4列，即收盘价
})
ldply(closedata,function(x)summary(x[[1]])) #对每个股票求summary
