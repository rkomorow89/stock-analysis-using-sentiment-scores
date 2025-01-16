library("BatchGetSymbols")
library("stringr")
library("dplyr")
library("rvest")
library("stringr")

setwd("D:/Thomas_Blanck/Thomas_Blanck/RStudio-Projects/stock market data/Webcrawler/Yahoo Finance")

#make the webpages
paste_ticker<-function(ticker){
  paste0("https://finance.yahoo.com/quote/",ticker,"?p=",ticker,"&.tsrc=fin-srch")
}

# #DE
# #"https://de.finance.yahoo.com/quote/"
# stock<-"DAI.DE"
# keyword<-"daimler"
# datasource_x<-"Yahoo"
# datatype_x="Article"
# 
# stock<-"NFLX"
# keyword<-"netflix"
# datasource_x<-"Yahoo"
# datatype_x="Article"

# stock<-"WMT"
# keyword<-"walma"
# datasource_x<-"Yahoo"
# datatype_x="Article"

# stock<-"TSLA"
# keyword<-"tesla"
# datasource_x<-"Yahoo"
# datatype_x="Article"



get_yahoo_stock_news<-function(stock,keyword,datatype_x="Article",datasource_x="Yahoo"){
  tryCatch({
    #get the Main Page
    Webpage_topic<-paste_ticker(stock)
    
    #get all the links from page
    all_links<- Webpage_topic %>% 
      read_html() %>% 
      html_nodes("a") %>% 
      html_attr("href")
    
    #tollower
    all_links<-tolower(all_links)
    tester<-as.data.frame(all_links)
    
    #aggregate them
    tester<-tester %>% 
      group_by(all_links) %>% 
      summarize(N=n())
    
    ##sometimes sublinks sometimes it is an other link
    #Here are the normal links
    normal_link<-tester$all_links[grepl("https:",tester$all_links)]
    normal_link<-as.character(normal_link)
    normal_link<-normal_link[grepl(keyword,normal_link)]
    
    
    #here are the sublinks
    sub_links<-tester[c(which(!(tester$all_links %in% normal_link))),]
    alle_validen_sub_links<-sub_links$all_links[grepl(keyword,sub_links$all_links)]
    alle_validen_sub_links<-paste0("https://finance.yahoo.com",alle_validen_sub_links)
    
    #bind them together
    all_links<-c(normal_link,alle_validen_sub_links)
    alle_validen_links<-all_links
    
    #exclude some links
    exclude="https://finance.yahoo.com"
    alle_validen_links<-alle_validen_links[which(alle_validen_links !=exclude)]
    
    # #all valid links- containng a certain word
    # alle_validen_links<-tester$all_links[grepl(keyword,tester$all_links)]
    # alle_validen_links<-paste0("https://finance.yahoo.com",alle_validen_links)
    # # alle_validen_links<-as.data.frame(alle_validen_links) 
    # # colnames(alle_validen_links)<-"Links"
    
    
    #How many can we scrape?
    NUMBER<-length(alle_validen_links)
    paste("WE HAVE",NUMBER,"NEWS FOR THE:",stock,"STOCK")
    
    #i<-alle_validen_links[1]

    ###go trough the links and give me the title
    all_titles<-data.frame()
    for(i in alle_validen_links[1:NUMBER]){
      tryCatch({
        #get the title
        title<-i %>% 
          read_html() %>% 
          html_nodes("h1") %>% 
          html_text() %>% 
          as.character() 
        
        title<-title[1]
        
        #some data cleaning
        title<-str_replace(title,";"," ") %>% 
          as.data.frame(stringsAsFactors=FALSE) 
        
        colnames(title)<-"title"

        Link<-as.data.frame(i)
        colnames(Link)<-"Link"
        
        Row_Link_and_Title<-cbind.data.frame(title,Link)
        
        Sys.sleep(5)
        all_titles<-rbind(all_titles,Row_Link_and_Title)
      }, error=function(e){print("Some error but coontinue")})
    }
    
    #################################
    # i <- alle_validen_links[10]
    #################################
    
    ###go trough the links and give me the date
    #i<-alle_validen_links[1]
    all_dates<-data.frame()
    for(i in alle_validen_links[1:NUMBER]){
      tryCatch({
        #get the title
        dates<-i %>% 
          read_html() %>% 
          html_nodes("time") %>% 
          html_attr("datetime") %>% 
          as.data.frame()

        ##some preprocessing
        #only use the first date
        dates<-dates[1]
        dates<-as.character(levels(dates$.))
        len_dates<-length(dates)

        if(len_dates!=0){
          dates<-str_replace(dates,"T"," ")
          dates<-str_replace(dates,".000Z","")
          dates<-as.POSIXct(dates)
          dates<-as.data.frame(dates)
        }else{
          dates<-as.POSIXct(Sys.time())
          dates<-as.data.frame(dates)
        }

        colnames(dates)<-"dates"
        Sys.sleep(5)
        all_dates<-rbind(all_dates,dates)
      }, error=function(e){print("Some error but coontinue")})
    }
    
    
    # i<-alle_validen_links[1]
    ###go trough the text
    all_texts<-data.frame()
    for(i in alle_validen_links[1:NUMBER]){
      tryCatch({
        #get the title
        text<-i %>% 
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
          text<-as.data.frame(text)  
        }else{
          text<-cnd_text
        }
        

        Sys.sleep(5)
        all_texts<-rbind(all_texts,text)
      }, error=function(e){print("Some error but coontinue")})
      
    }
    

    #bind them together;and save them
    Data<-qpcR:::cbind.na(titles=all_titles,dates=all_dates,text=all_texts)
    Data<-cbind.data.frame(Data,Ticker=stock,Datasource=datasource_x,datatype=datatype_x,date_create=as.POSIXct(Sys.time()))
    
    Data<-arrange(Data,Data$dates)
    timestamp<-as.character(Sys.time())
    timestamp<-str_replace(timestamp," ","_")
    timestamp<-str_replace_all(timestamp,":","T")
    timestamp<-str_sub(timestamp, 1, 16)
    
    ##Preprocessing
    Data[,1]<-as.character(Data[,1])
    Data[,2]<-as.character(Data[,2])
    Data[,4]<-as.character(Data[,4])
    ##if no text inside
    ##replace some char
    for(i in 1:length(Data$text)){ 
      Data$text[i]<-gsub( "[^[:alnum:][:blank:].$€\\]", " ",Data$text[i])
      #Data$text[i]<-str_replace_all(Data$text[i],"[[:punct:]]"," ")
    }
    
    
    write.csv2(Data,file=paste0(keyword,timestamp,".csv"),row.names = FALSE)
    #####################################################################
  }, error=function(e){print("Some error but coontinue")})
}

stock <- "WDI.DE"
keyword <- "wirecard"
datatype_x <- "Article"
datasource_x <- "Yahoo"

#stock<-c("AMZN","DAI.DE","NFLX","TSLA","GOOG","MSFT","WMT")
#keyword<-c("amazon","daimler","netflix","tesla","google","microsoft","walmart")
#datatype_x<-c("Article","Article","Article","Article","Article","Article","Article")
#datasource_x<-c("Yahoo","Yahoo","Yahoo","Yahoo","Yahoo","Yahoo","Yahoo")

liste_2<-cbind.data.frame(stock,keyword,datatype_x,datasource_x)
liste_2<-as.data.frame(sapply(liste_2,as.character),stringsAsFactors = FALSE)
liste_2
#liste_2<-liste_2[2,]


#Loop trough the list
for(i in 1:7){
  get_yahoo_stock_news(liste_2[i,1],liste_2[i,2],liste_2[i,3],liste_2[i,4])
}







# # # # # # #FILE READ
# keyword<-"tesl"
# path<-list.files(path = ".")
# stock_data_1 <- read.csv2(path[str_detect(path,keyword)])
 
# #lets get the function to clean the text by pattern
# source("D:\\Thomas Blanck\\Thomas Blanck\\RStudio-Projects\\stock market data\\Webcrawler\\Functions\\text_start_stop_finder.r")
# 
# 
# #some preprocessing
# text_to_analyze<-as.character(stock_data$text[1])
# text_to_analyze<-str_replace_all(text_to_analyze,","," ")
# #use the function
# indexes<-corpus_index_stopper(text_to_analyze)
# text<-text_seperator_by_point(indexes)
# text


# x <- "a1~!@#$€%^&*(){}_+:\"<>?,.?????????./;'[]-=" #or whatever
# gsub( "[^[:alnum:][:blank:].$€\\]", " ",x)
# str_replace_all(x, "[^[:alnum:]]", " ")
# 
# 
# 
# c = "In Acid-base reaction (page[4]), why does it create water and not H+?...." 
# gsub("[^[:alnum:][:blank:]+?&./\\-]", "", c)
# 
# '?&+-/
