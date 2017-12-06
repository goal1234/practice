
# --------------------------------------- #
# --------------------------------------- #

library(XML)
library(RCurl)
library(stringr)

giveNovel_name = function(rootNode){
  novel_name <- xpathSApply(rootNode,"//div[@class='title']/h1/text()",xmlValue)
  novel_name=gsub("([\r\n ])","",novel_name)
}

giveAuthor_name = function(rootNode){
  author_name <- xpathSApply(rootNode,c("//div[@class='title']/span/a/span/text()"),xmlValue)
  author_name=gsub("([\r\n ])","",author_name)
}

giveUri = function(rootNode){
  uri <- xpathSApply(rootNode,c("//div[@class='title']//span//a"),xmlAttrs,"href")#xpath路径中属性获取
  uri=gsub("([\r\n\t ])","",uri)
}

giveRead = function(rootNode){
  read_num <- xpathSApply(rootNode,c("//div[@class='score_txt']/text()[1]"),xmlValue)
  read_num=str_extract_all(read_num,"[0-9]+[0-9]")#从字符串中获取数字
}

##页面内请求获取评论量
giveReply = function(rootNode){
  population <- xpathSApply(rootNode,c("//div[@class='data']//b//span[@id='lblReviewCnt']//text()"),xmlValue)
}

webData= function(URL){
  Sys.sleep(runif(1,1,2))
  wp<-getURL(URL,.encoding="UTF-8") #对应的网站编码
  doc<-htmlParse(wp,asText=T,encoding="UTF-8")
  rootNode<-xmlRoot(doc)
  book_id=str_extract_all(URL,"[0-9]+[0-9]")[[1]]
  url2=gsub(" ","",paste("http://c.pingba.qidian.com/BookComment.aspx?BookId=",book_id,""))##拼接页面内数据请求url
  sub_wp<-getURL(url2,.encoding="UTF-8") #对应的网站编码
  sub_doc<-htmlParse(sub_wp,asText=T,encoding="UTF-8")
  sub_rootNode<-xmlRoot(sub_doc)
  date<-Sys.Date()
  data.frame(
    novel_name=giveNovel_name(rootNode),
    author_name=giveAuthor_name(rootNode), 
    uri=giveUri(rootNode)[3,1],
    read_num=as.numeric(giveRead(rootNode)),
    month_likenum=likenum[[1]][1],
    population=giveReply(sub_rootNode),
    updatetime=date#更新时间
  )
}

##测试单个url##
URL="http://www.qidian.com/Book/3548786.aspx"
info<-webData(URL)#使用编写的函数，获得网页数据
write.table(info,"F:\\数据收集\\qidian_literature.csv",append=TRUE,col.names=FALSE,row.names = FALSE,sep=",")###将数据存到本地文件

####批处理####
con <- file("F:\\数据收集\\qidian_urls.csv", "r")
line=readLines(con,n=1)
while( length(line) != 0 ) {
  info<-webData(line)#使用编写的函数，获得网页数据
  write.table(info,"F:\\数据收集\\qidian_literature.csv",append=TRUE,col.names=FALSE,row.names = FALSE,sep=",")###将数据存到本地文件
  line=readLines(con,n=1)
}
close(con)


# --------------------------------------- #
# --------------------------------------- #

#安装XML包
install.packages("XML")
#载入XML包
library(XML)
#确定网页地址，通过网页地址分析网页表格
url<-"http://hz.house.ifeng.com/detail/2014_10_28/50087618_1.shtml"
tbls<-readHTMLTable(url)
sapply(tbls,nrow)

#读取网页url的第一张表
pop<-readHTMLTable(url,which = 1)
#存储pop为CSV文档
write.csv(pop,file="F:/pop.csv")

# --------------------------------------- #
# --------------------------------------- #
#该爬虫爬取得是某地新闻内容 
#pa1:用于找到href链接；
#pa2:用于根据链接找到新闻内容
#pa3:用于存储进数据库

#参考别人写的一只：Kindle榜小爬虫：http://supstat.com.cn/blog/2015/03/31/amazon-kindle/

#爬虫pa1  
library(XML)        #引入XML包  
            
givehref<-function(rootNode){  
  hrefs<-xpathSApply(rootNode,"//a/@href")   #XPath方法应用  //a:匹配到所有a不管a在哪 @href:href属性  
  hrefs<-paste0("http://www.hancheng.gov.cn",hrefs[1:(length(hrefs)-4)]) #paste0() 粘贴字符串不�%9���空格  
  hrefs  
}  

givenames<-function(rootNode){  
  names<-xpathSApply(rootNode,"//tbody/tr/td",xmlValue)      # /td:td层的内容xmlValue  
  lists<-names[c(F,T,T,T,F,F)]       #   x[c(F,T)] 选择x中序号为偶数的元素  
  titles<-lists[c(T,F,F)]  
  way<-lists[c(F,T,F)]  
  time<-lists[c(F,F,T)]  
  return(list(titles=titles,way=way,time=time))  
}  
      
      
getpage<-function(address){  
  doc<-htmlParse(address,encoding = "UTF-8")     #htmlParse():解析HTML  
  rootNode<-xmlRoot(doc,skip = TRUE)             #转化为XML格式  
  data_com<-data.frame(givenames(rootNode),Hrefs=givehref(rootNode))   
  return(data_com)  
}
    
data<-function(URL){  
  x<-NULL  
  for(i in 1:28){  
    x<-rbind(x,getpage(URL[i]))    
  }  
  x          
}  
      
URL<-paste0("http://www.hancheng.gov.cn/info/iList.jsp?node_id=&site_id=CMShanch&catalog_id=51&cur_page=",1:28)  
data_all<-data(URL)  
      
write.csv(data_all,file=paste("./hancheng.csv"))    #写入csv文件  

#pa2:根据pa1的href地址找到文件并爬取内容  
library(XML)  
  
data_all<-read.csv("./hancheng.csv")  
  
getcontent<-function(rootNode){  
  content<-xpathSApply(rootNode,"//div[@class='con_c']",xmlValue)    #div中class为con_c的内容  
  content  
}  
getcontentcmd<-function(address){  
  doc<-htmlParse(address,encoding = "UTF-8")  
  rootNode<-xmlRoot(doc,skip = TRUE)  
  data<-getcontent(rootNode)  
  data  
}  
datacontent<-function(URL){  
  
  x<-NULL  
  for(i in 1:length(URL)){  
    x<-c(x,getcontentcmd(URL[i]))  
  }  
  x  
}  
  
content<-datacontent(data_all$Hrefs)  
  
write.table(content,file="./hanchengContent.csv") 

#pa3:将pa1,pa2中的数据导入到数据库中  
  
#install.packages("RODBC")  
library(RODBC)      #载入r的ODBC连接数据库  
  
  
mydb<-odbcConnect("hancheng",uid="SYSTEM",pwd="111111")      #连接  
hancheng<-read.csv("./hancheng.csv",stringsAsFactors = FALSE)  
content<-read.table("./hanchengContent.csv",stringsAsFactors = FALSE)  
  
fre<-length(hancheng$titles)  
  
for(i in 1:fre){  
  con<-gsub("\n","<br>",content$x[i])          #gsub(a,b,x)将x中的a替换为b  
  con<-gsub("<U\\+00A0>"," ",con)  
  query<-paste0("insert into Contents(CmsCode,CmsStats,UserID,CmsAttrib,CmsType,Dig1,Dig2,CmsTitle,OprateDate,CmsBody) values('040104',3,1,1,'3',1,0,'",hancheng$titles[i],"','",hancheng$time[i],"','",con,"')")  
  sqlQuery(channel=mydb,query=query)            #执行SQL操作  
}  
  
odbcClose(mydb) 


# --------------------------------------------- #
# --------------------------------------------- #

# 利用RSelenium包异步加载AJAX

library(XML)  
library(RCurl)  
library(rvest)  

URL = "http://book.qidian.com/info/1003354631"  
doc <- htmlParse(URL,encoding="UTF-8")  
rootNode <- xmlRoot(doc)  
book_name <- xpathSApply(rootNode,"//div[@class='book-information cf']/div/h1/em",xmlValue)#书名  
author <- xpathSApply(rootNode,"//div[@class='book-information cf']/div/h1/span/a",xmlValue)#作者  
scores = xpathSApply(rootNode,"//div[@class='book-information cf']/div/div/div/h4",xmlValue)#评分  
votes = xpathSApply(rootNode,"//p[@id='j_userCount']/span",xmlValue)#评价人数  
book_name  
#一念永恒  
author  
#耳根  
scores  
#0.0  



library(devtools)#如果没有安装要下载安装  
install_github(repo = "Rwebdriver", username = "crubba")
    
library(XML)  
library(RCurl)  
library(rvest)  
library(Rwebdriver)  

URL="http://book.qidian.com/info/1003354631"  
start_session(root = "http://localhost:4444/wd/hub/", browser ="firefox")#启动浏览器  
post.url(url=URL)#用浏览器打开网址  
pageSource <- page_source()  #存储浏览器的信息  

#后面都跟常规爬取一样  
doc<-htmlParse(pageSource,encoding="UTF-8")  
rootNode <- xmlRoot(doc)  
book_name <- xpathSApply(rootNode,"//div[@class='book-information cf']/div/h1/em",xmlValue)#书名  
author <- xpathSApply(rootNode,"//div[@class='book-information cf']/div/h1/span/a",xmlValue)  
status <- xpathSApply(rootNode,"//div[@class='book-information cf']/div/p[1]/span[1]",xmlValue)#只取第一个，看是否完结  
types <- xpathSApply(rootNode,"//div[@class='book-information cf']/div/p[1]/a[1]",xmlValue)#只取第一个，看什么类型小说  
words = xpathSApply(rootNode,"//div[@class='book-information cf']/div/p[3]/em[1]",xmlValue)#看多少万字  
cliks = xpathSApply(rootNode,"//div[@class='book-information cf']/div/p[3]/em[2]",xmlValue)#看多少万点击  
reoms = xpathSApply(rootNode,"//div[@class='book-information cf']/div/p[3]/em[3]",xmlValue)#看多少万推荐  
scores = xpathSApply(rootNode,"//div[@class='book-information cf']/div/div/div/h4",xmlValue)#看评分  
votes = xpathSApply(rootNode,"//p[@id='j_userCount']/span",xmlValue)#看平均人数  
quit_session()  
score  
#8.7  
votes  
#1203  


# --------------------------------------------- #
# --------------------------------------------- #

# 爬虫之批处理
setwd("E:/r_w/")  

#设置工作目录  
library(RCurl)  
html=getURL("http://rfunction.com/code/1202/")  

#下载页面  
temp=strsplit(html,"<li><a href=\"")[[1]]  

#分割页面  
files=strsplit(temp,"\"")  

#分割页面  
files=lapply(files,function(x){x[1]})  

#此时files为list类型，取files中的每个元素的第一个元素  
files=unlist(files)  

#转换成非list类型  
files=files[-(1:2)]  

#去除第一第二行    
base="http://rfunction.com/code/1202/"  
for(i in 1:length(files))  
{  
  url=paste(base,files[i],sep='')   
  temp=getBinaryURL(url)  
  
  #下载文件  
  note=file(paste("1202",files[i],sep='.'),open="wb")  
  
  #设置目录  
  writeBin(temp,note)  
  
  #写入  
  close(note)  
  Sys.sleep(2)  
} 


# ---------------------------------------- #
# ---------------------------------------- #

#　R网络爬虫之酒店团购
library(RCurl)  
library(XML)  
myheader=c(  
  "User-Agent"="Mozilla/4.0(compatible; MSIE 8.0; Windows NT 6.1; Win64; x64; Trident/4.0) ",  
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",  
  "Accept-Language"="en-us",  
  "Connection"="keep-alive",  
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7")  
#设置报头伪装成浏览器。因为有些服务器有限制  
  
urllist=0  
page=0:10  
urllist[page]=paste("http://t.dianping.com/hotel/xian-category_4&pageIndex=",page,sep="")  
jishuqi=0  
total_1=0  
total_2=0  
for(turn in urllist)#每次循环抓取一次页面（共抓5页）  
{   
  temp=getURL(turn,httpheader=myheader,encoding="UTF-8")  
    
    
  k=htmlParse(temp)#解析  
  # getNodeSet(k,'//li[@class="tg-floor-item"]')通过xPath定位，返回信息为list类型；  
  youhui=sapply(getNodeSet(k,'//li[@class="tg-floor-item"]'),xmlValue)  
  #youhui为字符串类型,每条信息为一个串  
    
  mydata=youhui  
  mydata  
  mydata=gsub("([N ])", "", mydata) #去除空格  
  mydata  
    
  ##　酒店名称　##  
  name=gregexpr("\\t[0-9]?[\u4e00-\u9fa5]+[0-9]*[\u4e00-\u9fa5]*[\u5e97|\u9986|\u95f4|\u623f|\u820d]",mydata)  
  #后四个编码为店，馆，间，房，舍  
  a=c()  
  for(i in 1:length(mydata))    
  {    
    temp=name[[i]]  
    a[i]=substring(mydata[i],temp[1]+1,temp[1]+attr(temp,'match.length')-1)  
  }   
  ##　[提示信息]　##  
  tip=gregexpr("\\[.+\\]",mydata)  
  b=c()  
  for(i in 1:length(mydata))    
  {    
    temp=tip[[i]]  
    b[i]=substring(mydata[i],temp[1]+1,temp[1]+attr(temp,'match.length')-2)#提示信息  
  }   
  ##　详细说明　##  
  remark=gregexpr("]\\n",mydata)  
  #向后取100+位置，然后确定详细信息中的首次出现的字母，将其后边剔除  
  c=c()  
  for(i in 1:length(mydata))    
  {    
    temp=remark[[i]]  
    c[i]=substring(mydata[i],temp[1]+2,temp[1]+attr(temp,'match.length')+100)  
    pos=gregexpr("\\n\\t",c[i])[[1]]  
    c[i]=substring(c[i],1,pos[1]-1)  
  }   
  ##　已售　##  
  num=gregexpr("[\u5df2][\u552e][0-9]+",mydata)  
  #"已售"的unicode为[\u5df2][\u552e]  
  d=c()  
  for(i in 1:length(mydata))    
  {    
    temp=num[[i]]  
    d[i]=substring(mydata[i],temp[1]+2,temp[1]+attr(temp,'match.length')-1)  
  }   
  d=as.numeric(d)  
  ##　打折价和原价　##  
  price=gregexpr("[\u00A5][0-9]+",mydata)  
  #\u00A5为人民币符号的unicode  
  x=c()  
  y=c()  
  for(i in 1:length(mydata))    
  {    
    temp=price[[i]]  
    x[i]=substring(mydata[i],temp[1]+1,temp[1]+attr(temp,'match.length')[1]-1)#打折价  
    y[i]=substring(mydata[i],temp[2]+1,temp[2]+attr(temp,'match.length')[2]-1)#原价  
  }   
  x=as.numeric(x)  
  y=as.numeric(y)  
  ##　评分　##  
  grade=gregexpr("[0-9][\u5206]",mydata)  
  #\u5206为‘分’的编码  
  e=c()  
  for(i in 1:length(mydata))    
  {    
    temp=grade[[i]]  
    e[i]=substring(mydata[i],temp[1]-2,temp[1])  
  }   
  e=as.numeric(e)  
  e  
  ##评论数量##  
  comment=gregexpr("[0-9]+[\u6761][\u8bc4]",mydata)  
  f=c()  
  for(i in 1:length(mydata))    
  {    
    temp=comment[[i]]  
    f[i]=substring(mydata[i],temp[1],temp[1]+attr(temp,'match.length')[1]-3)  
  }  
  f=as.numeric(f)  
  #########################  
  #此三行表示将所有数据存在total_2中  
  info=data.frame(a,b,c,x,y,d,e,f)  
  total_1=rbind(total_2,info)  
  total_2=total_1  
    
  #写入文件  
  #下面三行为每次抓的分别存  
  # wenjianming=paste(jishuqi,".txt",sep="")  
  # write.table(info,wenjianming,quote=FALSE)  
  # jishuqi=jishuqi+1    
    
  #存在一个文件中  
  #write.table(info,"hotle.txt",quote=FALSE,append=TRUE)  
    
}  
  
fix(total_2)  
#write.table(total_2,"hotle.txt",quote=FALSE,append=TRUE) 


# ------------------------------------------------------------- #
# ------------------------------------------------------------- #

# 案例演示——爬取上海证券交易所上市公司公告信息
require(stringr)
require(XML)
require(RCurl)
library(Rwebdriver)

# set path
setwd("ListedCompanyAnnouncement")
# base url
BaseUrl<-"http://www.sse.com.cn/disclosure/listedinfo/announcement/"

#start a session
quit_session()
start_session(root = "http://localhost:4444/wd/hub/",browser = "firefox")

# post Base Url
post.url(url = BaseUrl)

# get xpath
StockCodeField<-element_xpath_find(value = '//*[@id="inputCode"]')
ClassificationField<-element_xpath_find(value = '/html/body/div[7]/div[2]/div[2]/div[2]/div/div/div/div/div[2]/div[1]/div[3]/div/button')
StartDateField<-element_xpath_find(value = '//*[@id="start_date"]')
EndDateField<-element_xpath_find(value = '//*[@id="end_date"]')
SearchField<-element_xpath_find(value = '//*[@id="btnQuery"]')

# fill stock code
StockCode<-"600000"

element_click(StockCodeField)
keys(StockCode)

Sys.sleep(2)

#fill classification field 
element_click(ClassificationField)

# get announcement xpath
RegularAnnouncement<-element_xpath_find(value = '/html/body/div[7]/div[2]/div[2]/div[2]/div/div/div/div/div[2]/div[1]/div[3]/div/div/ul/li[2]')
Sys.sleep(2)
element_click(RegularAnnouncement)

# #fill start and end date 
# element_click(StartDateField)

# today's xpath
EndToday<-element_xpath_find(value = '/html/body/div[13]/div[3]/table/tfoot/tr/th')
Sys.sleep(2)
element_click(EndDateField)
Sys.sleep(2)
element_click(EndToday)

#click search
element_click(SearchField)

###################################
####获得所有文件的link           ##
all_links<-character()

#首页链接
pageSource<-page_source()
parsedSourcePage<-htmlParse(pageSource, encoding = 'utf-8')

pdf_links<-'//*[@id="panel-1"]/div[1]/dl/dd/em/a'

all_links<-c(all_links,xpathSApply(doc = parsedSourcePage,path = pdf_links,
                                   xmlGetAttr,"href"))

#############################
##遍历所有link，下载文件
for(i in 1:length(all_links)){
  Sys.sleep(1)
  if(!file.exists(paste0("file/",basename(all_links[i])))){
    download.file(url = all_links[i],destfile = paste0("file/",basename(all_links[i])),mode = 'wb')
    
  }
}



# -------------------------------------------- #
# -------------------------------------------- #
library(XML)
library(RCurl)
library(plyr)

truepagevalue <- list()
for(i in 1:138){                   #138是所有帖子的页面数
  Sys.sleep(1.5)                  #系统每次循环暂停1.5秒
  turl <- paste("http://bbs.pinggu.org/forum-78-",i,".html",sep="")    #将各部网页URL粘贴一起
  rd <- getURL(turl,.encoding="gb2312")
  rd <- iconv(rd,"gb2312","UTF-8")
  rdhtml <- htmlParse(rd,encoding="UTF-8")
  root <- xmlRoot(rdhtml)
  page <- getNodeSet(root,"//a")
  pagevalue <- unique(laply(page,xmlGetAttr,name='href'))
  allvalue <- gregexpr("http://bbs.pinggu.org/thread-",pagevalue)   

                                                                        #匹配寻找对应的url，因为爬取出很多url并不是我们想要的。
  torfvalue <- laply(allvalue,function(x)x[1]==1)      #并行提取列中每个元素的第一个值，并判断是否=1
  truepagevalue[[i]] <- pagevalue[torfvalue]    #抓取符合规则的url，并存贮在列表中
}

truepagevalue <- unique(unlist(truepagevalue))    #去列表化，去重。
allly <- NULL
regtitle <- list()
regdescription <- list()
for(j in 1:length(truepagevalue)){
  urlj <- truepagevalue[j]
  rdj <- getURL(urlj,.encoding="gb2312")
  rdj <- iconv(rdj,"gb2312","UTF-8")
  rdhtmlj <- htmlParse(rdj,encoding="UTF-8")
  rootj <- xmlRoot(rdhtmlj)
  otherpagej <- getNodeSet(rootj,"//div[@class='pg2015 y']/div[@class='pg']/a")
  otherpageurlj <- unique(laply(otherpagej,xmlGetAttr,name='href'))
  #################################################################  title
  pl <- getNodeSet(rootj,"//head/title")
  title <- xmlValue(pl[[1]])
  regtitle[[j]] <- substr(title,1,regexpr("-",title)[1]-2)
  ##############################################################  description
  description <- getNodeSet(root,"//head/meta[@name='description']")
  description1 <- xmlGetAttr(description[[1]],name='content')
  description2 <- iconv(description1,"UTF-8","gb2312")
  cl <- gregexpr(",",description2)[[1]]
  first <- cl[1];stop <- cl[length(cl)];
  regdescription[[j]] <- substr(description2,first+1,stop-1)
  ##################################################################  liuyan
  subvaluely <- list()
  urln <- c(urlj,otherpageurlj)
  for(n in urln){
    s <- sample(1:4,1)
    Sys.sleep(s)
    urlnj <- n
    rdnj <- getURL(urlnj,.encoding="gb2312")
    rdnj <- iconv(rdnj,"gb2312","UTF-8")
    rdhtmlnj <- htmlParse(rdnj,encoding="UTF-8")
    rootnj <- xmlRoot(rdhtmlnj)
    lynj <- getNodeSet(rootnj,"//div[@class='t_fsz']/table/tr/td[@class='t_f']")
    valuelynj <- laply(lynj,xmlValue,trim=T)
    subvaluely[[which(urln==n)]] <- gsub(pattern="[^[:graph:]]",replacement=" ",valuelynj)
    unlisubvaluely <- unlist(subvaluely) 
  }
  allly <- c(allly,unlisubvaluely)
}

###  head the data ###
head(allly)
regtitle <- unlist(regtitle);head(regtitle)
regdescription <- unlist(regdescription)
head(regdescription)



# --------------------------------------------------------- #
# --------------------------------------------------------- #

# 爬虫常用方法总结

# 使用XML抓取表格数据（爬取勇士队球员数据）
# 使用rvest抓取网页数据（爬取关于特朗普的百度新闻）
# 使用jsonlite抓取json格式数据（爬取高德地图温州各个行政区域的中心）
# 使用RSelenium模拟登录抓取数据（模拟登录人大经济论坛爬取R语言板块数据）
# 使用PhantomJS不登陆抓取数据（抓取国家数据各省的近13个月CPI）

#抓取表格数据(抓取勇士队的球员数据)

library(XML)
url <- 'http://www.stat-nba.com/team/GSW.html'
dt1 <- readHTMLTable(url,header = T)
names(dt1[[1]]) <- rvest::repair_encoding(names(dt1[[1]]))
head(dt1[[1]])


# -----
library(rvest)
library(stringr)
library(rlist)

url <- 'http://news.baidu.com/ns?cl=2&rn=20&tn=news&word=%E7%89%B9%E6%9C%97%E6%99%AE&ie=utf-8'

#抓取网页
httr_web <- read_html(url,encoding = 'utf-8')
#抓取新闻标题
title <- httr_web%>%html_nodes('h3>a')%>%html_text(trim = T)
#抓取新闻发布者与日期
author <- httr_web%>%html_nodes('p.c-author')%>%html_text(trim = T)
candidate_date=Sys.Date()%>%format('%Y年%m月%d日')
fun <- function(x){
    re=if (length(x)==3) {
        re=c(x[1],candidate_date,x[length(x)])
        } else {
            re= x[-2]
        }

    re=data.frame(发布者=re[1],日期=re[2],时间=re[3])
    return(re)
}

news_Trump <- data.frame(标题=title ,
author %>% str_split('s') %>% lapply(fun)%>%list.stack())
tail(news_Trump)

##抓取JSON数据(抓取温州各个行政区域的坐标)

library(jsonlite)

name='温州'

encoding_name <- iconv(enc2utf8(name),from='utf-8',to='ISO-8859-1',sub= "byte")%>%

str_replace_all('><','%')%>%str_sub(1,18)%>%str_to_upper()%>%
str_replace('<','%')
subdistrict_num=1

key_str='你的API'
url0 <- 'http://restapi.amap.com/v3/config/district?'

url <- paste0(url0,
            'keywords=',encoding_name,'&',
            subdistrict=',subdistrict_num,'&',
            'key=',key_str')

wz_center<- fromJSON(url)
wz_centers<-wz_center[['districts']][['districts']][[1]]
tail(wz_centers)



library(RSelenium)

remDr <- remoteDriver(remoteServerAddr = "127.0.0.1",port = 4444,browserName = "chrome")

remDr$open() #打开浏览器

remDr$navigate('http://bbs.pinggu.org/forum-69-1.html')
step1 <- remDr$findElement(using= 'xpath',"//*[@id='nv_forum']/div[6]/div[1]/div/div[4]/ul/li[3]/a")
step1$clickElement()
step21 <- remDr$findElement(using= 'xpath', '//*[@id="username"]')
step21$clickElement()

step21$sendKeysToElement(list(username ='用户名'))
step22 <- remDr$findElement(using= 'xpath', '//*[@id="password"]')
step22$clickElement()
step22$sendKeysToElement(list(password ='密码'))
step23 <- remDr$findElement(using= 'xpath', '/html/body/div[2]/div/div[2]/a')
step23$clickElement()
step3 <- remDr$findElement(using= "xpath","//*[@id='moderate']/table")

web <- step3$getElementAttribute("outerHTML")[[1]]%>%read_html()
dat3=data.frame(
    标题=web %>% html_nodes('a.xst')%>%html_text(trim = T),
    发布者=web %>% html_nodes('a.u')%>%html_text(trim = T),
    发布时间=web %>% html_nodes('p>em>span')%>%html_text(trim = T),
    最后回复者=web %>% html_nodes('p>em>a:nth-child(4)')%>%html_text(trim = T),
    最后回复日期=web %>% html_nodes('p>em>a:nth-child(5)')%>%html_text(trim = T)
)

tail(dat3)

var webPage = require('webpage');
var page = webPage.create();
var fs = require('fs');
var path = 'NDC.html'

page.open('http://data.stats.gov.cn/easyquery.htm?cn=E0101', function (status) {
var content = page.content;
fs.write(path,content,'w')
phantom.exit();
});

system("./phantomjs NDC.js")
web <- read_html("NDC.html")
dat4 <- (web%>%html_table())[[1]]
tail(dat4)



# --------------------------------------------- #
# --------------------------------------------- #

install.packages("RCurl")
install.packages("XML")
library(RCurl)
library(XML)


# 第一步，模拟浏览器行为
GET / HTTP/1.1

Host: cos.name
User-Agent: Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6)
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: en-us
Accept-Encoding: gzip,deflate
Accept-Charset: GB2312,utf-8;q=0.7,*;q=0.7
Keep-Alive: 300
Connection: keep-alive

myHttpheader <- c(
    "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
    "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language"="en-us",
    "Connection"="keep-alive",
    "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
)

# 第二步，就是模拟访问页面
url <- "https://book.douban.com/top250?icn=index-book250-all"
webpage <- getURL(url,httpheader=myHttpheader)


# 第三步，整理HTML结构
pagetree <- htmlTreeParse(webpage,encoding="GB2312", error=function(...){}, useInternalNodes = TRUE,trim=TRUE)
#注意encoding

# 第四步：节点定位
getNodeSet(doc,'/bookstore/book[1]')

# 选取属于 bookstore 子元素的最后一个 book 元素。
getNodeSet(doc,'/bookstore/book[last()]') 

# 选取最前面的两个属于 bookstore 元素的子元素的 book 元素
getNodeSet(doc,'/bookstore/book[position()<3]')  

# 选取所有拥有名为 lang 的属性的 title 元素。 
getNodeSet(doc,'//title[@lang]')  

#选取所有 title 元素，且这些元素拥有值为 eng 的 lang 属性 
getNodeSet(doc,"//title[@lang='eng']") 

# 选取 bookstore 元素的所有 book 元素，且其中的 price 元素的值须大于 35.00。 
getNodeSet(doc,"/bookstore/book[price>35.00]") 

# 选取 bookstore 元素中的 book 元素的所有 title 元素，且 price 元素的值须大于 35.00。 
getNodeSet(doc,"/bookstore/book[price>35.00]/title") 

# 选取 book 元素的所有 title 和 price 元素 
getNodeSet(doc,"//book/title | //book/price") 

# 选取文档中的所有 title 和 price 元素 
getNodeSet(doc,"//title | //price") 

# 选取 bookstore 元素的所有子元素 
getNodeSet(doc,"/bookstore/*") 

# 选取所有带有属性的 title 元素 
getNodeSet(doc,"//title[@*]") 

# 选择所有属性lang的值 
unlist(getNodeSet(doc,"//title/@lang"),use.names = FALSE) 

# title结点下的所有文本 
getNodeSet(doc,"//title/text()")

node<-getNodeSet(pagetree, "//p[@class='pl']/text()")
info<-sapply(node,xmlValue)

# --- 完整代码 --- #
install.packages("RCurl")
install.packages("XML")
library(RCurl)
library(XML)

myHttpheader <- c(
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7")

url <- "豆瓣图书 Top250"
webpage <- getURL(url,httpheader=myHttpheader,.encoding="gb2312")
pagetree <- htmlTreeParse(webpage,encoding="GB2312", error=function(...){}, useInternalNodes = TRUE,trim=TRUE)
node<-getNodeSet(pagetree, "//p[@class='pl']/text()")
info<-sapply(node,xmlValue) 
info

