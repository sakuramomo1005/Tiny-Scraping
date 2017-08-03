import requests
import json
import xlwt
import time

n=17 ##there are 17*30=510 questions in total
alltitle=[]
alltext=[]
alltags=[]
allids=[]

for i in range(n):
    url='http://api.stackexchange.com/2.2/questions'
    payload={'page':i+1,'pagesize':'30','fromdate':'1199145600','todate':'1483488000','order':'desc','sort':'activity','tagged':'r;data-visualization','site':'stackoverflow','filter':'!9YdnSIN18'}
    r=requests.get(url,params=payload)
    j=json.loads(r.text)
    length=len(j['items'])
    title=range(length) 
    id=range(length)
    textbody=range(length) 
    tags=range(length) 
    for k in range(length):
        title[k]=j['items'][k]['title']
        textbody[k]=j['items'][k]['body']
        id[k]=j['items'][k]['question_id']
        t=len(j['items'][k]['tags'])
        tag=j['items'][k]['tags']
        c='&'
        for d in range(t):
            c=c+tag[d]+'&'
        tags[k]=c
    
    alltitle=alltitle+title
    alltext=alltext+textbody
    alltags=alltags+tags

##out put questions data

file = xlwt.Workbook()  
table = file.add_sheet('info',cell_overwrite_ok=True) 
for i in range(len(alltext)):
    table.write(i,0,allids[i])
    table.write(i,1,alltitle[i])
    table.write(i,2,alltags[i])
    table.write(i,3,alltext[i])
file.save('question.xls')

###deal with answers
answer=range(2000)
answerid=range(2000)

d=0
for i in allids:
    time.sleep(0.5)
    h1='http://api.stackexchange.com/2.2/questions/'
    h2=str(i)
    h3='/answers'
    url=h1+h2+h3
    payload={'order':'desc','sort':'activity','site':'stackoverflow','filter':'!9YdnSMKKT'}
    r=requests.get(url,params=payload)
    j=json.loads(r.text)
    if(j['items']!=[]):
        length=len(j['items'])
        for k in range(length):
            answer[d+k]=j['items'][k]['body']
            answerid[d+k]=id[k]=j['items'][k]['question_id']
        d=d+length

##out put
file = xlwt.Workbook() 
write_ok=True) 
table = file.add_sheet('info',cell_overwrite_ok=True)
for i in range(631):
    table.write(i,0,answerid[i])
    table.write(i,1,answer[i])
file.save('answerid.xls')