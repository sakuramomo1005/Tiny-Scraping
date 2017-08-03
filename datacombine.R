setwd('c:\\R')

library('xlsx')
answer=read.xlsx('answer.xls',sheetName = 'info',header=FALSE)
question=read.xlsx('questions.xls',sheetName = 'info')
names(answer)=c('ID','Answer')
names(question)[4]='Question'

data=merge(question,answer,by='ID',all=TRUE)
total=dim(data)[1]

ans=c()
i=1
num=0
while(num<total){
  n=table(anid)[i]
  ans=c(ans,1:n)
  num=num+n
  i=i+1
}

ansnum=paste('answer#',ans,sep='')
data$AnswerNumber=ansnum

for(i in 1:total){
  if(is.na(data$Answer[i])==1){
    data$AnswerNumber[i]='No answer'
  }
}

write.xlsx(data,'qustion&answer.xls',sheetName = 'Sheet1')