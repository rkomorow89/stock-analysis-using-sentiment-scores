library("BatchGetSymbols")
library("stringr")
library("dplyr")
library("rvest")
library("stringr")

setwd("C:/Users/Robert/OneDrive/Dokumente/R Shiny/Scraping Data")

stock<-c("AMZN","DAI.DE","NFLX","TSLA","GOOG","MSFT","WMT")


paste_ticker<-function(ticker){
  paste0("https://finviz.com/quote.ashx?t=",ticker)
}

ticker<-stock[3]
#get the Main Page
Webpage_topic<-paste_ticker(stock[3])

#get all links
all_links<- Webpage_topic %>% 
  read_html() %>%
  html_nodes("[class='tab-link-news']") %>% 
  html_attr("href")


#titles
titles<- Webpage_topic %>% 
  read_html() %>% 
  html_nodes("[class='tab-link-news']") %>% 
  html_text()

#dates
dates<-Webpage_topic %>% 
  read_html() %>% 
  html_nodes("[width='130']") %>% 
  html_text() 

date_indexes<-which(nchar(dates)>=10)
date_texts<-substring(dates,0,9)
#date_texts[date_indexes]

a<-date_indexes[-length(date_indexes)]
#b<-date_indexes[-1]-1
b<-date_indexes[-1]-1


if(!(length(date_texts) %in% a) & !(length(date_texts) %in% a)){
  a<-c(a,date_indexes[length(date_indexes)])
  b<-c(b,length(date_texts))
}else{print("its normal")}



l<-list()
for(i in 1:length(a)){
  l[[i]]<-print(paste(date_texts[date_indexes][i],(date_texts[a[i]:b[i]])))
}
l<-unlist(l)


l2<-list()
for(i in 1:length(l)){
  l2[[i]] <-ifelse(nchar(str_trim(l[i]))>17,paste(paste(str_sub(str_trim(l[i]),0,9),"12:00AM")),str_trim(l[i]))
}
l2<-unlist(l2)

dates<-as.POSIXct(strptime(l2, "%b-%d-%C %I:%M %p"))


stockmarket_data<-cbind.data.frame(all_links,titles,dates)
stockmarket_data<-stockmarket_data[grepl("netflix",all_links)==TRUE,]
alle_validen_links<-as.character(stockmarket_data[,1])

#loop for the texts
for(i in 1:length(stockmarket_data$all_links)){
  tryCatch({
    text<-alle_validen_links[i] %>% 
      read_html() %>% 
      html_nodes("p") %>% 
      html_text() 
    
    #the first text is always some bullshit
    cnd_text<-text
    text<-text[-1]
    test<-nchar(text)
    if(!is.null(test)){
      text<-paste0(text,". ")
      text<-gsub(".. ", ". ",text, fixed = TRUE)
      text<-paste(text,collapse = ",")
      text<-str_replace(text,";"," ")
    }else{
      text<-cnd_text
    }
    
    Sys.sleep(5)
    stockmarket_data$text[i]<-as.character(text)
    
    # all_texts<-cbind(all_texts,text)
  }, error=function(e){
    print(paste("Some error at interation:",i))
  })
}




data_fetched<-cbind.data.frame(stockmarket_data,ticker=ticker,datasource="Finviz",datatype="Article",date_create=Sys.time())
##lean them 
#duplicated out
data_fetched<-data_fetched[!duplicated(data_fetched$titles),]
#not enough text out
data_fetched<-data_fetched[(nchar(data_fetched$text)>=10),]


timestamp<-as.character(Sys.time())
timestamp<-str_replace(timestamp," ","_")
timestamp<-str_replace_all(timestamp,":","T")
timestamp<-str_sub(timestamp, 1, 16)

write.csv2(data_fetched,file=paste0(ticker,timestamp,".csv"),row.names = FALSE)
