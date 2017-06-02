library("httr") 
library("dplyr") 
library("jsonlite") 


url.exists('http://www.baidu.com')



i <- 1
begin_date <- Sys.Date() - i
end_date <- Sys.Date() - i
api <- "https://api.weixin.qq.com/datacube/getusersummary"
access_token <- "ACCESS_TOKEN" 

getUserSummary <- paste0(api, "?access_token=", access_token, "&begin_date=", begin_date, "&end_date=", end_date) %>%
  GET() %>%
  content() %>%
  fromJSON(simplifyDataFrame = TRUE)

# 这里返回的还是列表，其中 list 才是所需的 data.frame
getUserSummary <- getUserSummary$list

url="ftp.wcc.nrcs.usda.gov/data/snow/snow_course/table/history/idaho/"
filenames = getURL(url, dirlistonly = TRUE)

curl = getCurlHandle()
destination=getURL("http://www.sina.com”, curl = curl)
                   getCurlInfo(curl)$response.code
                   
                   followlocation=T
                   
                   
                   Sys.time()
                   strsplit(Sys.time,'')
                   class(Sys.time)
                   class(Sys.time())
                   as.character(Sys.time())
                   strsplit(as.character(Sys.time()),'')
                   strsplit(as.character(Sys.time()),' ')
                   strsplit(as.character(Sys.time()),' ')[2]
                   unlist(strsplit(as.character(Sys.time()),' '))
                   unlist(strsplit(as.character(Sys.time()),' '))[2]
                   
                   
                   url="http://job.abchina.com/abb/download_accessory.do?action=accessory&pronunciamentoId=46702"
                   temp<- getBinaryURL(url)
                   note <- file("hellodata.xls,open = "wb")
writeBin(temp,note)
close(note)




html=getURL("http://rfunction.com/code/1202/")
temp =strsplit(html, "<li><a href=\"")[[1]]
files =strsplit(temp, "\"")
files=lapply(files, function(x){ x[1] })
files= unlist(files)

files = files[-(1:2)]

base="http://rfunction.com/code/1202/"
for(i in 1:length(files)){
  URL = paste(baseURL, files[i], sep="")
  
  
  bin <- getBinaryURL(URL)
  con <- file(paste("1202", files[i], sep="."), open = "wb")
  writeBin(bin, con)
  close(con)
  Sys.sleep(2) 
}



##########第一段视频↑######
##########第二段视频↓######

library(RCurl)
library(XML)


url="http://www.bioguo.org/AnimalTFDB/BrowseAllTF.php?spe=Mus_musculus"
wp<-getURL(url)
doc <-htmlParse(wp, asText= TRUE)
tables <-readHTMLTable(doc,which=5)


url="http://219.143.71.11/wdc4seis@bj/earthquakes/csn_quakes_p001.jsp"
wp<-getURL(url)
doc <-htmlParse(wp, asText= TRUE)
tables <-readHTMLTable(doc)
tables <-readHTMLTable(doc,header=F)


url="http://www.w3school.com.cn/example/xmle/books.xml"
doc<-xmlParse(url)

getNodeSet(doc,'//title[@lang]|//book/price')

myheader=c("User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
           "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
           "Accept-Language"="en-us",
           "Connection"="keep-alive",
           "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
)

temp=getURL("http://t.dianping.com/guangzhou?q=%E7%94%B5%E5%BD%B1",httpheader=myheader,encoding="UTF-8")

k=htmlParse(temp)
getNodeSet(k,'//script[@type="text/plain"]')[95]
youhui=sapply(getNodeSet(k,'//script[@type="text/plain"]'),xmlValue)
youhui[95]

urllist=0
page=1:5
urllist[page]=paste("http://t.dianping.com/guangzhou?pageno=",page,"&q=%E7%94%B5%E5%BD%B1",sep='')
for(url in urllist)
{
  temp=getURL(url,httpheader=myheader,encoding="UTF-8")
  
  k=htmlParse(temp)
  
  youhui=sapply(getNodeSet(k,'//script[@type="text/plain"]'),xmlValue)
  cat(url,"\n")
}

d = debugGatherer()
web='https://api.stackexchange.com/'
#web="http://www.dataguru.cn/"
temp <- getURL(web,debugfunction=d$update,verbose =TRUE)
cat(d$value()[3])#提交给服务器的头信息
cat(d$value()[1])#服务器地址以及端口号
cat(d$value()[2])#


install.packages('bitops')
install.packages('rvest')
install.packages('DBI')
install.packages('stringr')
install.packages('RSQLite')
install.packages('sqldf')
install.packages('ggplot2')

install.packages('sp')
install.packages('raster')

install.packages('proto')
install.packages('gsubfn')


library(bitops)
library(rvest)
library(stringr)
library(DBI)
library(RSQLite)
library(sqldf)
library(RCurl)
library(ggplot2)
library(sp)
library(raster)
library(proto)
library(gsubfn)

url='stackexchange.com/'
combined_info <- url%>%html_session()%>%html_nodes("div.metaInfo")%>%html_text()%>%strsplit(split="\r\n")



getdata <- function(i){
  url <- paste0("www.cnblogs.com/p",i)##generate url
  combined_info <- url%>%html_session()%>%html_nodes("div.post_item div.post_item_foot")%>%html_text()%>%strsplit(split="\r\n")
  post_date <- sapply(combined_info, function(v) return(v[3]))%>%str_sub(9,24)%>%as.POSIXlt()##get the date
  post_year <- post_date$year+1900
  post_month <- post_date$mon+1
  post_day <- post_date$mday
  post_hour <- post_date$hour
  post_weekday <- weekdays(post_date)
  title <- url%>%html_session()%>%html_nodes("div.post_item h3")%>%html_text()%>%as.character()%>%trim()
  link <- url%>%html_session()%>%html_nodes("div.post_item a.titlelnk")%>%html_attr("href")%>%as.character()
  author <- url%>%html_session()%>%html_nodes("div.post_item a.lightblue")%>%html_text()%>%as.character()%>%trim()
  author_hp <- url%>%html_session()%>%html_nodes("div.post_item a.lightblue")%>%html_attr("href")%>%as.character()
  recommendation <- url%>%html_session()%>%html_nodes("div.post_item span.diggnum")%>%html_text()%>%trim()%>%as.numeric()
  article_view <- url%>%html_session()%>%html_nodes("div.post_item span.article_view")%>%html_text()%>%str_sub(4,20)
  article_view <- gsub(")","",article_view)%>%trim()%>%as.numeric()
  article_comment <- url%>%html_session()%>%html_nodes("div.post_item span.article_comment")%>%html_text()%>%str_sub(14,100)
  article_comment <- gsub(")","",article_comment)%>%trim()%>%as.numeric()
  data.frame(title,recommendation,article_view,article_comment,post_date,post_weekday,post_year,post_month,post_day,post_hour,link,author,author_hp)
  
}

cnblog<- data.frame()
for(m in 1:5){
  cnblog <- rbind(cnblog,getdata(m))
}

head(cnblog)


url='http://stackoverflow.com/questions/tagged/data-visualization'

temp=getURL(url)

k=strsplit(temp,"\r\n")[[1]]

timeadr=k[grep("<a ",k)]



getdata <- function(i){
 
  combined_info <- url%>%html_session()%>%html_nodes("div.summary div.views")%>%html_text()%>%strsplit(split="\r\n")
  post_date <- sapply(combined_info, function(v) return(v[3]))%>%str_sub(9,24)%>%as.POSIXlt()##get the date
  post_year <- post_date$year+1900
  post_month <- post_date$mon+1
  post_day <- post_date$mday
  post_hour <- post_date$hour
  post_weekday <- weekdays(post_date)
  title <- url%>%html_session()%>%html_nodes("div.post_item h3")%>%html_text()%>%as.character()%>%trim()
  link <- url%>%html_session()%>%html_nodes("div.post_item a.titlelnk")%>%html_attr("href")%>%as.character()
  author <- url%>%html_session()%>%html_nodes("div.post_item a.lightblue")%>%html_text()%>%as.character()%>%trim()
  author_hp <- url%>%html_session()%>%html_nodes("div.post_item a.lightblue")%>%html_attr("href")%>%as.character()
  recommendation <- url%>%html_session()%>%html_nodes("div.post_item span.diggnum")%>%html_text()%>%trim()%>%as.numeric()
  article_view <- url%>%html_session()%>%html_nodes("div.post_item span.article_view")%>%html_text()%>%str_sub(4,20)
  article_view <- gsub(")","",article_view)%>%trim()%>%as.numeric()
  article_comment <- url%>%html_session()%>%html_nodes("div.post_item span.article_comment")%>%html_text()%>%str_sub(14,100)
  article_comment <- gsub(")","",article_comment)%>%trim()%>%as.numeric()
  data.frame(title,recommendation,article_view,article_comment,post_date,post_weekday,post_year,post_month,post_day,post_hour,link,author,author_hp)
  
}


post-text

url='http://stackoverflow.com/questions/tagged/data-visualization'

#url='http://stackoverflow.com/questions/41173540/dynamic-data-visualization-in-angular2-using-html-angular2-template-engine'
combined_info <- url%>%html_session()%>%html_nodes("h3.question-hyperlink")%>%html_text()%>%strsplit(split="\r\n")

question-hyperlink

combined_info2 <- url%>%html_session()%>%html_nodes("div.question-hyperlink")%>%html_text()%>%strsplit(split="\r\n")



library(XML)
library(RCurl)
URL = getURL("http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_MarketHistory/stockid/603000.phtml?year=2013&jidu=1")
doc<-htmlParse(URL,encoding="utf-8")  #此处必须加上encoding方式
tables<-readHTMLTable(doc,header=F,which=20)
tables



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
    
    read_num=as.numeric(giveRead(rootNode)),
   
    population=giveReply(sub_rootNode),
    updatetime=date#更新时间
  )
}

URL="http://www.qidian.com/Book/3548786.aspx"
info<-webData(URL)#使用编写的函数，获得网页数据
write.table(info,"literature.csv",append=TRUE,col.names=FALSE,row.names = FALSE,sep=",")###将数据存到本地文件








library(rvest)

url='http://stackoverflow.com/questions/tagged/data-visualization?page=2&sort=newest&pagesize=15'

url='http://stackoverflow.com/questions/tagged/data-visualization'

web<-read_html(url)
b<-web %>% html_nodes("div.summary") %>% html_text() %>%strsplit(split='\r\n')


a=url%>%html_session()%>%html_nodes("div.summary a.question-hyperlink")%>%html_attr("href")

position2<-web %>% html_nodes("div.t-r") %>% html_text()


ateam %>% html_nodes("h3") %>% html_nodes("td")
strsplit()








library(httr)
library(dplyr)
library(rjson)
api <- "https://api.dinghuo123.com/v2/oauth2/token?"
grant_type <- "client_credentials"
client_id <- "*****"
client_secret <- "********"
scope <- "basic"
userName <- "*********"
password <- "********"

response_content <- api %>% 
  paste0("grant_type=", grant_type) %>%
  paste0("&client_id=", client_id) %>%
  paste0("&client_secret=", client_secret) %>%
  paste0("&scope=", scope) %>%
  paste0("&userName=", userName) %>%
  paste0("&password=", password) %>%
  GET() %>%
  content() # 最后返回的结果是一个json，这里指定的话，会自动识别，json就转为list

access_token <- response_contentdataaccess_token
refresh_token <- response_contentdatarefresh_token

req <- GET(paste0("https://api.dinghuo123.com/v2/order/pull_order.json?transportType=tcp&size=100&loadLog=false&loadDetail=true&loadRemark=true", "&access_token=", access_token))

status_code(req)
content(req,encoding = utf-8)
