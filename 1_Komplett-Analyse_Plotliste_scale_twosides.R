library(stringr)
library(tm)
library(syuzhet)
library("plotly")
library(moments) 
library("dplyr")
library(RMySQL)
library("scales")

##Read the data#
#Working Directory ändern#
setwd("C:\\Users\\Robert\\OneDrive\\Dokumente\\R Shiny\\R Programme")
#rufe Funktion auf, mit der Texte nach bestimmten Mustern aufgeteilt werden k?nnen#
source("C:\\Users\\Robert\\OneDrive\\Dokumente\\R Shiny\\R Programme\\text_start_stop_finder.r")



prepare_plot<-function(X){
  #####################################READ IN THE DATA FROM  DATABASE##################################
  con <- dbConnect(RMySQL::MySQL(), host = "s215.goserver.host",dbname="web187_db4",user = "web187_4", password = "STOCKMARKET2020")
  # close db connection after function call exits!!!!!!!!
  on.exit(dbDisconnect(con))
  
  ##THIS IS THE EMERGENCY CALL  
  query<-paste("SELECT * FROM stock_news")
  Data<-dbGetQuery(con,query)
  new_data<-Data[,c("title", "Link", "dates", "text", "ticker", "datasource", "datatype" )]
  colnames(new_data)<-c("title", "Link", "dates", "text", "Ticker", "Datasource", "datatype" )
  Biglist<-new_data
  ##########################################################################  
  
  
  # #####################################READ IN THE DATA IN  A CORPUS##################################
  # #Alle Dateien im WD auflisten#
  # path<-list.files(path = ".",pattern=".txt")
  # 
  # #Read in the files in a loop and bind them together
  # Biglist<-data.frame()
  # for(i in path){
  #   #Nachrichten aus einer Textdatei auslesen und als Corpus zusammenfassen#
  #   Corpus <- read.csv(i, sep=";")
  #   Corpus_to_export <- Corpus[,c(1,2,3,5)]
  #   
  #   #Spalte des Corpus, der Nachrichten-Texte enthält, in Characters umwandeln#
  #   Corpus[,4]<-as.character(Corpus[,4])
  #   Corpus<-as.data.frame(Corpus)
  #   Biglist<-rbind.data.frame(Biglist,Corpus)
  # }
  # ###############################################################################################################
  


  l <- list()
  #Preprocessing
  for(i in 1:length(Biglist[,4])){
    #rufe Funktion auf, mit der Texte nach bestimmten Mustern aufgeteilt werden k?nnen#
    source("C:\\Users\\Robert\\OneDrive\\Dokumente\\R Shiny\\R Programme\\text_start_stop_finder.r")
    
    
    #Zu analysierender Text aus Corpus anhand von Text-Nummer (Zeile) einzeln abspeichern#
    text_to_analyze_Z<-as.character(Biglist[i,4])
    
    #Teile Abschnitte in einzelne Sätze anhand von Satzpunkten auf#
    indexes<-corpus_index_stopper(text_to_analyze_Z)
    #Speichere einzelne Sätze in einer Liste ab#
    Biglist_X<-text_seperator_by_point(text_to_analyze_Z,indexes)
    Biglist_X[,1]<-as.character(Biglist_X[,1])
    
    #Verbinde aufgespaltene Nachrichtentexte mit Spalten Title, Link, Dates und Ticker#
    together<-cbind.data.frame(Biglist[i,1],Biglist[i,2],Biglist[i,3],Biglist[i,5],Biglist_X[,1])
    colnames(together)<-c("Title","Link","Dates","Ticker","Text")
    
    #Säubere einzelne Sätze
    together[,5]<-str_replace_all(together[,5], '"', '')
    together[,5]<-trimws(together[,5])
    together[,5]<-tolower(  together[,5])   
    together[,5]
    
    l[[i]] <- together
    
  }
  
  
  #Lade Dictionarys herein#
  syuzhet_dict <- get_sentiment_dictionary('syuzhet')
  bing_dict <- get_sentiment_dictionary('bing')
  afinn_dict <- get_sentiment_dictionary('afinn')
  


  Big_L<-list()
  for(i in 1:length(Biglist$text)){
    
    #Bestimme Sentiment-Scores der einzelnen Sätze einer Nachricht#
    syuzhet_vector <-get_sentiment(l[[i]][[5]], method="syuzhet")
    bing_vector <- get_sentiment(l[[i]][[5]], method="bing")
    afinn_vector <- get_sentiment(l[[i]][[5]], method="afinn")
    
  
    #START ULTIMATE  NORMLZ
    #>>>wir bekommen extrem negative punkte bei [normz] obwohl im positiven bereich

    #Falls Nachricht aus mehr als einem Satz besteht#
    if(length(syuzhet_vector)>1){


     #Ultimate nrmlz function
     normalize_both_sides_ultimate<-function(X){
     "with this function we minmax scale on both sides -100 til 0
     and also 0 till +100; special condtion: if we have +100 or -100 it will be autom. zero"
     gggg2<-as.data.frame(X)
     colnames(gggg2)<-"gggg2"
     gggg2$index_rows<-1:length(gggg2$gggg2)
     
     
     
     classes_L<-cut(gggg2$gggg2,breaks =c(-100,0,100))
     classes_L<-as.character(classes_L)
     classes_L<-gggg2[which(classes_L=="(-100,0]"),]
     
     #if the left side = zero length, then add a c(0,1)
     if(length(classes_L$index_rows)==0){
       classes_L<-rbind.data.frame(classes_L,c(0,1))
       colnames(classes_L)<-c("gggg2","index_rows")
     }else{
       print("mach weiter")
       classes_L<-cut(gggg2$gggg2,breaks =c(-100,0,100))
       classes_L<-as.character(classes_L)
       classes_L<-gggg2[which(classes_L=="(-100,0]"),]
     }
     
    #45/ 37+19
     
     classes_R<-cut(gggg2$gggg2,breaks =c(-100,0,100), right = FALSE)
     classes_R<-as.character(classes_R)
     classes_R<-gggg2[which(classes_R=="[0,100)"),]
     
     if(length(classes_R$index_rows)==0){
       classes_R<-rbind.data.frame(classes_R,c(0,1))
       colnames(classes_R)<-c("gggg2","index_rows")
     }else{
       print("mach weiter")
       classes_R<-cut(gggg2$gggg2,breaks =c(-100,0,100), right = FALSE)
       classes_R<-as.character(classes_R)
       classes_R<-gggg2[which(classes_R=="[0,100)"),]
     }
     
     
     
     
     
     
     
     #wenn mindestens 4 ungleich sind dann rescale sonst alle zu 0!!!
     if( length(unique(classes_L$gggg2))<4){
       print("dann scale nicht und mache alles zu 0 links")
       classes_L$gggg2<-0
     }
     
     if( length(unique(classes_R$gggg2))<4){
       print("dann scale nicht und mache alles zu 0 rechts")
       classes_R$gggg2<-0
     }
     
     if(length(unique(classes_R$gggg2))>=4 & length(unique(classes_L$gggg2))<4){
       #normalisiere  Klasse rechts
       print("normalisiere  Klasse rechts")
       classes_R$gggg2 <- scales::rescale(classes_R$gggg2,to=c(0,100))
       
       #normalisiere nicht Klasse links
       print("normalisiere nicht Klasse links mache 0")
       classes_L$gggg2 <- 0
     }
     
     if(length(unique(classes_R$gggg2))<4 & length(unique(classes_L$gggg2))>=4){
       #normalisiere  nicht Klasse rechts
       print("normalisiere  nicht Klasse rechts")
       classes_R$gggg2 <- 0
       
       #normalisiere  Klasse links
       print("normalisiere nicht Klasse links mache 0")
       classes_L$gggg2 <- scales::rescale(classes_L$gggg2,to=c(0,100))
     }
     
     if(length(unique(classes_R$gggg2))<4 & length(unique(classes_L$gggg2))<4){
       #normalisiere  nicht Klasse rechts
       print("normalisiere nicht Klasse links mache 0")
       classes_R$gggg2 <- 0
       
       #normalisiere  Klasse links
       print("normalisiere nicht Klasse links mache 0")
       classes_L$gggg2 <- 0
     }
     
     # classes_L$index_rows
     if(length(unique(classes_R$gggg2))>=4 & length(unique(classes_L$gggg2))>=4){
     print("scale beide!")  
     #normalisiere Klasse links
     classes_L$gggg2 <- scales::rescale(classes_L$gggg2,to=c(-100,0))

     #normalisiere Klasse rechts
     classes_R$gggg2 <- scales::rescale(classes_R$gggg2,to=c(0,100))
     }
     
     colnames(classes_L)<-c("classes_L_norm","index_rows")
     colnames(classes_R)<-c("classes_R_norm","index_rows")
     
     #merge nach index rows
     data_together_nrmlz<-merge(classes_L,classes_R,by='index_rows',all=TRUE)
     
     #dann werden max und min bzw. werte mit 100 oder -100 zu NULL
     data_together_nrmlz$classes_L_norm[which(data_together_nrmlz$classes_L_norm==-100 |data_together_nrmlz$classes_L_norm==+100)]<-0
     data_together_nrmlz$classes_R_norm[which(data_together_nrmlz$classes_R_norm==-100 |data_together_nrmlz$classes_R_norm==+100)]<-0
     
     l_new<-list()
     for(i in 1:length(data_together_nrmlz$index_rows)){
     if(!is.na(data_together_nrmlz$classes_L_norm[i])){
       l_new[[i]]<-data_together_nrmlz$classes_L_norm[i]
     }else{
       l_new[[i]]<-data_together_nrmlz$classes_R_norm[i]
     }
    }
     
   data_together_nrmlz$merged<- unlist(l_new)
   
   return(data_together_nrmlz$merged)
     }

     #use the function on them
     
     syuzhet_vector_norm<-normalize_both_sides_ultimate(syuzhet_vector)
     bing_vector_norm<-normalize_both_sides_ultimate(bing_vector)
     afinn_vector_norm<-normalize_both_sides_ultimate(afinn_vector)
     
     #Sätze mit normalisierten Scores in einer Tabelle verbinden#
     Corpus <-cbind.data.frame(l[[i]][[1]],l[[i]][[2]],l[[i]][[3]],l[[i]][[4]],l[[i]][[5]] , syuzhet_vector_norm, bing_vector_norm, afinn_vector_norm,stringsAsFactors=FALSE)
     colnames(Corpus)<-c("title","Link","dates","Ticker","Text","syuzhet_vector_norm", "bing_vector_norm", "afinn_vector_norm")    
     
     

      #Falls Nachricht aus nur einem Satz besteht#
    }else{
      
      next
    }
    

    # nzmean <- function(x) {
    # "Use this function to get the row mean and excluding the 0 value"
    #   if (all(x==0)) 0 else mean(x[x!=0])}
    
    # nzmean <- function(x) {
    #   "Use this function to get the row mean and excluding the 0 value"
    #   if (all(x==0)) 0 else mean(x)}
    
    
    all_vectors_norm_average<-apply(Corpus[,6:8],1,mean)
    all_vectors_norm_std<-apply(Corpus[,6:8],1,sd)

    #Mittelwerte mit den Sätzen verbinden#
    text_output <- cbind(Corpus, all_vectors_norm_average,all_vectors_norm_std)
    
    #then check if we have -1 because that means we have a value to low
    #check also if we have +1 because that mean we have a value to high>>
    #due to the natur of normalization it can be possible that you will have a highest value eg 0.25 shuzen 
    #which will be 1 in this case; also if you have 0 you can have -1 because of the lowest>> so putout the
    #+1 or the -1
    #6:10
    
    # text_output[text_output==+1]<-0
    # text_output[text_output==-1]<-0
    
    Big_L[[i]]<-text_output
    
  }
  
  #View(Big_L[[65]])
  

  finish_list<-data.frame()
  for(i in 1:length(Biglist$text)){
    
    if(is.null(Big_L[[i]])){
      next
    }else{
      print(paste("somevalue inside",i))
      tester<-Big_L[[i]]
      #throw out the zeros because its not intresting!
      tester<-tester[tester$all_vectors_norm_average!=0,]
      
      #pipe
      tester_2<-tester%>% 
        group_by(title, Link, dates, Ticker) %>% 
        summarize(N=n(),mean_meannorm=mean(all_vectors_norm_average),mean_stdnorm=mean(all_vectors_norm_std))
      
      finish_list<-rbind.data.frame(finish_list,tester_2)
      
    }
    

    
  }
  
  return(finish_list[complete.cases(finish_list),])
}


# asd<-finish_list[complete.cases(finish_list),]

#prepareplotfunction
#robby<-prepare_plot()
#prepare_plot()

