setwd("~/Dropbox/practice/crawler/scitechvista")

packages = c("httr", "XML", "magrittr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())),repos="http://cran.r-project.org")
}

library(httr)
library(XML)
library(magrittr)

url = "https://scitechvista.most.gov.tw/zh-tw/Feature/L/0/13/10/1.htm"
res = GET(url)
res = content(res, 'text', encoding="utf8")
res = htmlParse(res, encoding = 'utf-8')

finalpage = xpathApply(res, "/html//div[@id='ctl00_oCPH_Left_oHGC_Upper_Page']//a[13]", xmlAttrs) %>% 
  as.character  %>% strsplit(split='/', fixed=T) %>% unlist %>%
  .[length(.)] %>% strsplit(split='.', fixed=T)  %>% unlist %>% .[1] %>% as.numeric
  

##

# host= "https://scitechvista.most.gov.tw/zh-tw/Feature/L/0/13/10/"
reqText=vector()
for(i in 1:finalpage){
  url[i]=paste0("https://scitechvista.most.gov.tw/zh-tw/Feature/L/0/13/10/", i, ".htm")
  res = GET(url[i])
  reqText[i] = content(res, 'text', encoding='utf8')
}

data=list()
for(i in 1:finalpage){
  res = htmlParse(reqText[i], encoding = 'utf-8')
  data[[i]] = reqText[i] %>% htmlParse(encoding="utf8") %>% 
    xpathSApply("/html//div[@class='box_topic']/a", xmlAttrs) %>% .[2,]
}

for(i in 1:length(data)){
  for(j in 1:10){
    data[[i]][j] = paste0("https://scitechvista.most.gov.tw/zh-tw/Feature", sub( "../../../..", "", data[[i]][j]))
  }
}

data %<>% unlist


###every page###

# url = "https://scitechvista.most.gov.tw/zh-tw/Feature/C/0/13/10/4/2111.htm" #2015/11/20
# url2="https://scitechvista.most.gov.tw/zh-tw/Feature/C/0/13/10/1/2206.htm"  #2016/1/27
GetPage = function(url){
  res2 = GET(url)
  res2 = content(res2, 'text', encoding="utf8")
  res2 = htmlParse(res2, encoding = 'utf-8')
  data2 = xpathApply(res2, "/html//div[@class='sub_headline']/div/div/div", xmlValue) %>% unlist
  title = data2[1]
  data22 = sub("\r\n\t\t\t\t\t\t\t\t\t","", data2[2]) %>% strsplit(split="|",fixed=T) %>% unlist 
  author = data22[1] %>% gsub(" ","",.)
  institution = data22[2] %>% gsub(" ","",.)
  content= xpathApply(res2, "/html//div[@class='content']", xmlValue)
    Start<- regexpr("日期：", content)
    End<- regexpr("\\d{4}/\\d{1,}/\\d", content)
  time =  
    substr(content, Start+3, End+99) %>% gsub("\r\n\t*","",.)
  table =cbind(author, institution, title, time, url) %>% as.data.frame
  return(table)
}

table =  lapply(data, GetPage) %>% do.call(rbind,.)
write.csv(table, "data.csv")

