library(stringr)
library(tm)
library(syuzhet)
#install.packages("Rcpp")
library(Rcpp)
library(plotly)
library(moments) 
library("dplyr")
library(RMySQL)
library("scales")
library(readxl)
#install.packages("tidyquant")
library(tidyquant)
library(plotly)
library(scales)
library(RMySQL)
library(dplyr)
library(corpus)
library(textstem)

# ##Read the data#
# #Working Directory Ã¤ndern#
# setwd("C:\\Users\\Robert\\OneDrive\\Dokumente\\R Shiny\\R Programme")
# #rufe Funktion auf, mit der Texte nach bestimmten Mustern aufgeteilt werden k?nnen#
# source("C:\\Users\\Robert\\OneDrive\\Dokumente\\R Shiny\\R Programme\\text_start_stop_finder.r")



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

  l <- list()
  #Preprocessing
  for(i in 1:length(Biglist[,4])){
    
    
    #Zu analysierender Text aus Corpus anhand von Text-Nummer (Zeile) einzeln abspeichern#
    text_to_analyze_Z<-as.character(Biglist[i,4])
    
    #Teile Abschnitte in einzelne Saetze anhand von Satzpunkten auf#
    split_text<-text_split(text_to_analyze_Z)
    #Bestimme Grundformen der WÃ¶rter in einem Satz
    lemmatize_strings_text<-lemmatize_strings(as.character(split_text$text))
   
    
    #Verbinde aufgespaltene Nachrichtentexte mit Spalten Title, Link, Dates und Ticker#
    together<-cbind.data.frame(Biglist[i,1],Biglist[i,2],Biglist[i,3],Biglist[i,5],lemmatize_strings_text,stringsAsFactors =FALSE)
    colnames(together)<-c("Title","Link","Dates","Ticker","Text")
    
    
    
    l[[i]] <- together
    
  }
  
  
  #Lade Dictionarys herein#
  syuzhet_dict <- get_sentiment_dictionary('syuzhet')
  bing_dict <- get_sentiment_dictionary('bing')
  afinn_dict <- get_sentiment_dictionary('afinn')
  
  
  
  Big_L<-list()
  for(i in 1:length(Biglist$text)){
    
    #Bestimme Sentiment-Scores der einzelnen Saetze einer Nachricht#
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
      
      #Saetze mit normalisierten Scores in einer Tabelle verbinden#
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
    
    #Mittelwerte mit den Saetzen verbinden#
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










############################################################################################################################

# #source("C:\\Users\\Robert\\OneDrive\\Dokumente\\R Shiny\\R Programme\\Komplett-Analyse_Plotliste.R")
# source("C:\\Users\\Robert\\OneDrive\\Dokumente\\R Shiny\\R Programme\\1_Komplett-Analyse_Plotliste_scale_twosides.R")
# 


#load the data
Data_to_plot<-prepare_plot()
Kennzahlen_Scores<-Data_to_plot   #Dauert recht lange


#Data cleaning
#At first remove all cells where there are NA' full width;we only wan the complete cases 
Kennzahlen_Scores<-Kennzahlen_Scores[complete.cases(Kennzahlen_Scores[1:5]),]

#if there is an "NA" or an NA please replace with 0
for(i in 1:ncol(Kennzahlen_Scores)){
  tryCatch({
    Kennzahlen_Scores[which(is.na(Kennzahlen_Scores[,i]) | Kennzahlen_Scores[,i] =="NA"),i]<-0
  }, error=function(e){print(paste("There is a failure on Column:",i))})
}
#here we also need as date
#as.POSIXct(Kennzahlen_Scores$dates)
Kennzahlen_Scores$dates<-as.Date(Kennzahlen_Scores$dates)
Kennzahlen_Scores$mean_stdnorm<-as.numeric(Kennzahlen_Scores$mean_stdnorm)
Kennzahlen_Scores$mean_meannorm<-as.numeric(Kennzahlen_Scores$mean_meannorm)
Kennzahlen_Scores$Ticker<-as.character(Kennzahlen_Scores$Ticker)

Kennzahlen_Scores$mean_meannorm<-scales::rescale(Kennzahlen_Scores$mean_meannorm,to=c(-100,100))

#lets check how many are good or bad
Kennzahlen_Scores$labels <-cut(Kennzahlen_Scores$mean_meannorm, breaks=c(-100,-5,5,100), 
                               labels=c("negativ","neutral","positiv"), include.lowest=TRUE)


#then show How many news on each day
news_on_each_Day<-Kennzahlen_Scores %>% 
  group_by(dates,Ticker) %>% 
  summarize(Gesamt=n())

#then show how many news on that day positiv
news_on_each_Day_positiv<-Kennzahlen_Scores %>% 
  filter(labels=="positiv") %>% 
  group_by(dates,Ticker) %>% 
  summarize(positiv=n())

#then show how many news on that day negativ
news_on_each_Day_negativ<-Kennzahlen_Scores %>% 
  filter(labels=="negativ") %>% 
  group_by(dates,Ticker) %>% 
  summarize(negativ=n())

#then show how many news on that day neutral
news_on_each_Day_neutral<-Kennzahlen_Scores %>% 
  filter(labels=="neutral") %>% 
  group_by(dates,Ticker) %>% 
  summarize(neutral=n())


#then merge them
fully_stacked<-merge(news_on_each_Day,news_on_each_Day_positiv,by.x = c("dates","Ticker"),by.y=c("dates","Ticker"),all=TRUE)
fully_stacked<-merge(fully_stacked,news_on_each_Day_negativ,by.x = c("dates","Ticker"),by.y=c("dates","Ticker"),all=TRUE)
fully_stacked<-merge(fully_stacked,news_on_each_Day_neutral,by.x = c("dates","Ticker"),by.y=c("dates","Ticker"),all=TRUE)

#if there is an "NA" or an NA please replace with 0
for(i in 1:ncol(fully_stacked)){
  tryCatch({
    fully_stacked[which(is.na(fully_stacked[,i]) | fully_stacked[,i] =="NA"),i]<-0
  }, error=function(e){print(paste("There is a failure on Column:",i))})
}


#lets show the overall positiv by each day
fully_stacked$overall_by_positiv<-paste(fully_stacked$positiv,"von",fully_stacked$Gesamt)  
fully_stacked$overall_by_negativ<-paste(fully_stacked$negativ,"von",news_on_each_Day$Gesamt) 
fully_stacked$overall_by_neutral<-paste(fully_stacked$neutral,"von",news_on_each_Day$Gesamt)  
#we need this to evaluate
evaluate_fully_stacked<-fully_stacked

####now we will aggregeate the data and show the mean_mean,..etc
check<-Kennzahlen_Scores %>% 
  group_by(dates,Ticker) %>% 
  summarize(N=n(),mean_mean=mean(mean_meannorm),mean_Standardabweichung=mean(mean_stdnorm))

#lets merge them together
fully_stacked_Data<-merge(check[,c(1,2,4,5)],fully_stacked,by.x = c("dates","Ticker"),by.y=c("dates","Ticker"))
fully_stacked_Data
###############################################

##now some preprocessing to prepare for the plot
#lets give an NA to 0 std
fully_stacked_Data$mean_Standardabweichung[which(fully_stacked_Data$mean_Standardabweichung==0)]<-NA


##normaliz
#normalize <- function(x) {return((max(x)-x) /(max(x)-min(x)))}
#fully_stacked_Data$mean_normalz_Standardabweichung[!is.na(fully_stacked_Data$mean_Standardabweichung)]<-normalize(fully_stacked_Data$mean_Standardabweichung[!is.na(fully_stacked_Data$mean_Standardabweichung)])
fully_stacked_Data$mean_normalz_Standardabweichung[!is.na(fully_stacked_Data$mean_Standardabweichung)]<-fully_stacked_Data$mean_Standardabweichung[!is.na(fully_stacked_Data$mean_Standardabweichung)]
# library("scales")

#normalize
#normalize_otherway <- function(x){return((x- min(x)) /(max(x)-min(x)))}
#fully_stacked_Data$mean_normalz_mean[!is.na(fully_stacked_Data$mean_mean)]<-normalize_otherway(fully_stacked_Data$mean_mean[!is.na(fully_stacked_Data$mean_mean)])
fully_stacked_Data$mean_normalz_mean[!is.na(fully_stacked_Data$mean_mean)]<-fully_stacked_Data$mean_mean[!is.na(fully_stacked_Data$mean_mean)]




##################################################################PLOT################################################
#get stock data
stock<-"TSLA"     #Aktie angeben, deren Kurs geplottet werden soll
from_date<-"2020-06-05"      #Beginn des Zeitraums angeben, ab dem das Schaubild gezeigt werden soll

ggg<-Biglist[Biglist$dates>"2020-06-05",]

data_xtls<-getSymbols(stock, from = from_date,
                      to = as.Date(Sys.time()),warnings = FALSE,
                      auto.assign=FALSE)


#some preprocessing Data 
data_xtls<-as.data.frame(data_xtls)
data_xtls$date<-rownames(data_xtls)
data_xtls$date<-as.Date(data_xtls$date)


#we only need specific data
stock_of_intrest<-fully_stacked_Data[which(fully_stacked_Data[,2]== stock),]

#specifiy also the date
stock_of_intrest<-stock_of_intrest[which(stock_of_intrest$dates>=from_date),]

#lets merge them
merge_together<-merge(stock_of_intrest,data_xtls,by.x = "dates", by.y ="date" ,all = TRUE )



##preprocessing
#carry the last observation forward to replace your NA values
merge_together[,14:19]<-na.locf(merge_together[,14:19], fromLast = FALSE)
merge_together[,14:19]<-na.locf(merge_together[,14:19], fromLast = TRUE)

#merge_together<-merge_together[630:length(merge_together$dates),]


# #lets get the points to show as an index on the line plot
# which(colnames(merge_together)=="WMT.Close")

points<-merge_together[which(!is.na(merge_together$mean_normalz_mean)),17]
merge_together[which(!is.na(merge_together$mean_normalz_mean)),"Pointer_mean_nrmlz"]<-points



#give the circle some classes
classes_of_circle <-cut(merge_together$mean_mean, breaks=c(-100,-5,5,100), 
                        labels=c("negative","neutral","positive"), include.lowest=TRUE)

classes_of_circle<-classes_of_circle[!is.na(classes_of_circle)]


#give the plot circle sizes
sizes_of_circle<-merge_together$mean_normalz_mean[!is.na(merge_together$mean_normalz_mean)]
sizes_of_circle<-abs(sizes_of_circle)



#give the plot colors
colors_plot<-colorRampPalette(c("red","gray", "green"))( 5 )
color_of_a_circle <-cut(merge_together$mean_mean, breaks=c(-100,-70,-5,5,70,100), 
                        labels=colors_plot, include.lowest=TRUE)

color_of_a_circle<-color_of_a_circle[!is.na(color_of_a_circle)]

merge_together$Pointer_mean_nrmlz[which(!is.na(merge_together$Pointer_mean_nrmlz))]

#Aktienplot erzeugen
plot_ly(merge_together, x = ~dates,showlegend = T) %>% 
  add_markers(y = ~Pointer_mean_nrmlz, name = 'NEWS',
              text = ~paste("<b>NEWS AT THIS DAY</b>","<br>Positiv: ", overall_by_positiv, 
                            '<br>Negativ:', overall_by_negativ,
                            '<br>neutral:', overall_by_neutral,
                            "<br><br>",
                            "Mean:",mean_mean),
              marker = list(size = ~sizes_of_circle, opacity = 1, opacity = 0.5, 
                            color = color_of_a_circle,line = list(color = 'gray',width = 2)
              )) %>% 
  add_lines( y = ~merge_together[,17], line = list(color = 'black'), name = 'Closing price', titlefont = list(size = 15)) %>% 
  layout(title = paste('Stockmarket News of:',stock), titlefont = list(size = 15),
         yaxis = list(title="Closing price",
                      zeroline = FALSE, 
                      titlefont = list(size = 15), 
                      tickfont = list(size = 15)),
         xaxis = list(zeroline = FALSE, 
                      titlefont = list(size = 15), 
                      tickfont = list(size = 15))) %>% 
  add_annotations(x = merge_together$dates[!is.na(merge_together$Pointer_mean_nrmlz)],
                  y = merge_together$Pointer_mean_nrmlz[!is.na(merge_together$Pointer_mean_nrmlz)],
                  text = paste(classes_of_circle),
                  xref = "x",
                  yref = "y",
                  showarrow = TRUE,
                  arrowhead = 4,
                  arrowsize = .5,
                  ax = 20,
                  ay = -40)


list(tickfont = list(size = 15))

###########################ERGEBNISSE#########################################################################
print(paste("evaluate_fully_stacked","is defined inside the skript AGGNewsPlot"))
date<-"2020-06-06"
#stock<-"TSLA"
evaluate_fully_stacked<-evaluate_fully_stacked
#sum(evaluate_fully_stacked$Gesamt)



###############Anteile der nachrichtexte############################
evaluate_alla_rob<-function(evaluate_fully_stacked,stock){
  
  # stock_evaluate_fully_stacked<-evaluate_fully_stacked[which(evaluate_fully_stacked$dates>=date),] %>% 
  #   filter(Ticker==stock)
  
  stock_evaluate_fully_stacked<-evaluate_fully_stacked %>%
    filter(Ticker==stock)
  
  
  stock_evaluate_fully_stacked<-colSums(stock_evaluate_fully_stacked[,sapply(stock_evaluate_fully_stacked,is.numeric)])
  stock_evaluate_fully_stacked<-as.data.frame(as.matrix(t(as.data.frame(stock_evaluate_fully_stacked))))
  rownames(stock_evaluate_fully_stacked) <- c()
  
  
  stock_evaluate_fully_stacked$proportionPOS<-stock_evaluate_fully_stacked$positiv/stock_evaluate_fully_stacked$Gesamt*100
  stock_evaluate_fully_stacked$proportionNEG<-stock_evaluate_fully_stacked$negativ/stock_evaluate_fully_stacked$Gesamt*100
  stock_evaluate_fully_stacked$proportionNEU<-stock_evaluate_fully_stacked$neutral/stock_evaluate_fully_stacked$Gesamt*100
  
  
  stock_evaluate_fully_stacked[is.na(stock_evaluate_fully_stacked)]<-0
  
  return(stock_evaluate_fully_stacked)
  
}




#length(evaluate_fully_stacked[which(evaluate_fully_stacked$dates>="2020-06-06"),])
########################################################


##############Kursveraenderung##################################
#get stock data
kursver_fun<-function(stock,datum){
  data_xtls<-getSymbols(stock, from = datum,
                        to = as.Date(Sys.time()),warnings = FALSE,
                        auto.assign=FALSE)
  
  #some preprocessing Data 
  data_xtls<-as.data.frame(data_xtls)
  data_xtls$date<-rownames(data_xtls)
  data_xtls$date<-as.Date(data_xtls$date)
  
  #price diff.
  column_of_intrest<-data_xtls[,6]
  date_of_intrest<-data_xtls[,7]
  data_of_intrest<-cbind.data.frame(column_of_intrest,date_of_intrest)
  data_of_intrest[1,1]-data_of_intrest[length(data_of_intrest$column_of_intrest),1]
  kurs_veraenderung<-(data_of_intrest[length(data_of_intrest$column_of_intrest),1]*100/data_of_intrest[1,1])-100
  paste(i,round(kurs_veraenderung,2),"%")
  
  ifelse(kurs_veraenderung>0,paste("+",round(kurs_veraenderung,2),"%"),paste(round(kurs_veraenderung,2),"%"))
  
  return(kurs_veraenderung)
}


xs <-unique(evaluate_fully_stacked$Ticker)

total_X <- data.frame()

for (x in xs) {
  total_X_ROW<-cbind.data.frame(evaluate_alla_rob(evaluate_fully_stacked, x) , kursver=kursver_fun(x,"2020-06-06"), Aktie = x)
  total_X <- rbind(total_X, total_X_ROW)
}

print(total_X)
sum(total_X$Gesamt)


cor.test(total_X$proportionPOS, total_X$kursver, conf.level = 0.75)
cor.test(total_X$proportionNEG, total_X$kursver)
cor.test(total_X$proportionNEU, total_X$kursver)






#date_liste<-Biglist[Biglist$dates>="2020-06-06",]
##################################

#Anteil der positiven Scores plotten
positiv_desc <- arrange(total_X, desc(total_X$proportionPOS))
positiv_desc
positiv <- round(t(positiv_desc[, c(5,8)]),2)
colnames(positiv) <- positiv_desc$Aktie
positiv
#bp <- barplot(positiv, main = "Aktien mit positiven Scores zusammen mit Kursveraenderung", 
#              ylab = "%", col=c("darkblue","red"), las=2, ylim=c(0,100),
#              legend = rownames(positiv), beside = TRUE)

#Saeulendiagramm zeichnen
par(mfrow=c(1,1))
barplot(positiv,
        las=2, 
        cex.axis=1.3,
        cex.names=1.3,
        cex.lab=1.3,
        col=c("darkblue","red"), 
        ylab ="%", 
        ylim = c(min(positiv)-10,max(positiv)+10),
        beside = TRUE, 
        legend.text = c("percentage of positive news", "share price change"), 
        args.legend = list(x = 'topright', cex= 1.1, bty='n', inset=c(-0.5, -0.15)),
        ann=FALSE,
        axes=TRUE
        )

#Raster hinzufügen
grid(nx=NULL, ny=NULL)

#Anzahl negativer Nachrichten
sum(positiv_desc$positiv)

#Punktdiagramm ausgeben
#plot(x = positiv[1,], y = positiv[2,], xlab = "proportionPOS", ylab = "kursver")

#Korrelation und p-value berechnen
cor.test(x = positiv[1,], y = positiv[2,])

#Anteil der negativen Scores plotten
negativ_desc <- arrange(total_X, desc(total_X$proportionNEG))
negativ_desc
negativ <- round(t(negativ_desc[, c(6,8)]),2)
colnames(negativ) <- negativ_desc$Aktie
negativ
#bp <- barplot(negativ, main = "Aktien mit negativen Scores zusammen mit Kursveraenderung", 
#              ylab = "%", col=c("darkblue","red"), las=2, ylim=c(0,100), 
#              legend = rownames(negativ), beside = TRUE)

#Saeulendiagramm zeichnen
par(mfrow=c(1,1))
barplot(negativ, 
        las=2,
        cex.axis=1.3,
        cex.names=1.3,
        cex.lab=1.3,
        col=c("darkblue","red"), 
        ylab = "%", 
        ylim = c(min(negativ)-10,max(negativ)+10),
        beside = TRUE,
        legend.text = c("percentage of negative news", "share price change"), 
        args.legend = list(x = 'topright', bty='n', cex = 1.1, inset=c(-0.5, -0.15)),
        ann=FALSE,
        axes=TRUE
        )
#Raster hinzufügen
grid(nx=NULL, ny=NULL)

#Anzahl negativer Nachrichten
sum(negativ_desc$negativ)

#Punktdiagramm ausgeben
#plot(x = negativ[1,], y = negativ[2,], xlab = "proportionNEG", ylab = "kursver")

#Korrelation und p-value berechnen
cor.test(x = negativ[1,], y = negativ[2,])

#Anteil der neutralen Scores plottens
neutral_desc <- arrange(total_X, desc(total_X$proportionNEU))
neutral_desc
neutral <- round(t(neutral_desc[, c(7,8)]),2)
colnames(neutral) <- neutral_desc$Aktie
neutral

# barplot(neutral, main = "Aktien mit neutralen Scores zusammen mit Kursveraenderung", 
#               ylab = "%", col=c("darkblue","red"), las=2, ylim=c(0,100),
#               legend = rownames(neutral), beside = TRUE)
# # axis(2,at=seq(0,20,5))



#Saeulendiagramm zeichnen
par(mfrow=c(1,1))
barplot(neutral,
        las=2, 
        cex.axis=1.3,
        cex.names=1.3,
        cex.lab=1.3,
        col=c("darkblue","red"), 
        ylab ="%", 
        ylim = c(min(neutral)-10,max(neutral)+10),
        beside = TRUE,
        legend.text = c("percentage of neutral news", "share price change"), 
        args.legend = list(x = 'topright', cex =1.1, bty='n', inset=c(-0.5, -0.15)),
        ann=FALSE,
        axes=TRUE
        )
#Raster hinzufügen
grid(nx=NULL, ny=NULL)

#Anzahl neutraler Nachrichten
sum(neutral_desc$neutral)

#Punktdiagramm ausgeben
#plot(x = neutral[1,], y = neutral[2,], xlab = "proportionNEU", ylab = "kursver")

#Korrelation berechnen
cor.test(x = neutral[1,], y = neutral[2,])

