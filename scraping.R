library('RCurl')
library('XML')
library('rvest')
setwd('c://R')

### web scrap function
scrap=function(web){
  ##get tags
  rawtags=web %>% html_nodes("div.summary") %>% html_text() %>%strsplit(split='\r\n')
  tags=c()
  tags1=c()
  for(i in 1:pagesize){
    tags[i]=rawtags[[i]][7]
    tags[i]=gsub('            ','',tags[i])
    tags1[i]=strsplit(gsub('            ','',tags[i]),split = ' ') 
  }
  ##get links
  rawlinks=web%>%html_nodes("div.summary a.question-hyperlink")%>%html_attr("href")
  links=paste('http://stackoverflow.com/',rawlinks,sep='')
  ##get titles
  titles=web %>% html_nodes("a.question-hyperlink") %>% html_text()
  data=data.frame(titles,links)
  return(data)
}

page=rep(NA,47)
web=rep(NA,47)
tablepage=NULL
for(i in 1:47){
  page[i]=paste('http://stackoverflow.com/questions/tagged/data-visualization?page=',i,'&sort=newest&pagesize=15',sep='')
  web<-read_html(page[i])
  tablepage=rbind(tablepage,scrap(web))
  #tablepage[i,]=scrap(web)
}

write.csv(tablepage,'resultsss.csv')




##get tags
rawtags=web %>% html_nodes("div.summary") %>% html_text() %>%strsplit(split='\r\n')
tags=c()
tags1=c()
for(i in 1:pagesize){
  tags[i]=rawtags[[i]][7]
  tags[i]=gsub('            ','',tags[i])
  tags1[i]=strsplit(gsub('            ','',tags[i]),split = ' ') 
}

##get links
rawlinks=web%>%html_session()%>%html_nodes("div.summary a.question-hyperlink")%>%html_attr("href")
links=paste('http://stackoverflow.com/',rawlinks,sep='')

##get titles
titles=web %>% html_nodes("a.question-hyperlink") %>% html_text()
titles=titles[1:pagesize]

data.frame(titles,tags,links)

i=1
page=paste('http://stackoverflow.com/questions/tagged/data-visualization?page=',i,'&sort=newest&pagesize=15',sep='')
web<-read_html(page)
raws=web %>% html_nodes("td.answercell") %>% html_text() %>%strsplit(split='\r\n')

n=dim(page)[1]

answerlist=NULL
for(i in 1:n){
  answer=NULL 
  weblinks=page[i,3]
  web=read_html(weblinks)
  ans=web %>% html_nodes("td.answercell") %>% html_text() %>%strsplit(split='\r\n')
  k=length(ans)
  if(k==0){
    answerlist[[i]]='no answers'
  } else {
    for(j in 1:k){
      answer=c(answer,paste('answer###',j,': ',ans[[j]][3],sep=''))
    }
    answerlist[[i]]=answer
  }
}

##²¹È«
len=rep(0,n)
for(i in 1:n){
  len[i]=length(answerlist[[i]])
}
l=max(len)
for(i in 1:n){
  if(len[i]<l) answerlist[[i]][(len[i]+1):l]='NA'
}