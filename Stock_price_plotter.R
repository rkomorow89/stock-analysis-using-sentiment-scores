install.packages("quantmod")
install.packages("plotly")
install.packages("dplyr")

library(quantmod)
library(plotly)
library(dplyr)

#Aktie angeben, deren Kurs geplottet werden soll (auf Yahoo Finance Namen des Unternehmens eingeben, um Kürzel zu finden)

#Beispiele:
#Moderna: 0QF.F
#Biontech: 22UA.F
#Merck: MRK.DE
#Astrazeneca: ZEG.DE
#Nvidia: NVD.DE
#Sanofi: SNW.F
#Pfizer: PFE.DE
#Curevac: 5CV.DE
#Apple: APC.DE
#Tesla: TL0.DE

stock<-"TL0.DE"     

#Beginn des Zeitraums angeben, in dem die Aktienkurse ausgegeben werden sollen

from_date<-"2020-07-01"    

#alternativ: vom aktuellen Datum eine bestimmte Anzahl Tage abziehen
from_date <- as.Date(Sys.time()) - 9

#Kursdaten von Yahoo Finance holen
stock_price_fun <- function(stock){
  
  data_xtls<-getSymbols(stock, from = from_date,
                        to = as.Date(Sys.time()),warnings = FALSE,
                        auto.assign=FALSE)
  
  data_xtls <- as.data.frame(data_xtls)
  data_xtls <- tibble::rownames_to_column(data_xtls, "date")
  data_xtls
  
}

print(data_xtls)

stock_data <- stock_price_fun(stock)
print(stock_data)

#Schlusskurse anzeigen
close_prices <- stock_data[,c(1,5)]
print(close_prices)

#Schlusskurs-Daten plotten
plot_ly(data_xtls, x = ~date, y = ~NVDA.Close, showlegend = T) %>%
add_lines( y = ~data_xtls[,5], line = list(color = 'black'), name = 'Closing price', titlefont = list(size = 15)) %>%
layout(title = paste('Stockmarket News of:',stock), titlefont = list(size = 15),
       yaxis = list(title="Closing price",
                    zeroline = FALSE, 
                    titlefont = list(size = 15), 
                    tickfont = list(size = 15)),
       xaxis = list(zeroline = FALSE, 
                    titlefont = list(size = 15), 
                    tickfont = list(size = 15)))

#Kursveränderung in % bestimmen
kursver_fun<-function(stock, from_date){
  data_xtls<-getSymbols(stock, from = from_date,
                        to = as.Date(Sys.time()),warnings = FALSE,
                        auto.assign=FALSE)
  
  data_xtls <- as.data.frame(data_xtls)
  data_xtls <- tibble::rownames_to_column(data_xtls, "date")
  
  #Preis-Differenz berechnen
  column_of_interest <- data_xtls[,5]
  date_of_interest <- data_xtls[,1]
  data_of_interest <- cbind.data.frame(column_of_interest,date_of_interest)
  Startkurs <- data_of_interest[1,1]
  Endkurs <- data_of_interest[length(data_of_interest$column_of_interest),1]
  kurs_veraenderung <- (Endkurs)*100/Startkurs - 100
  paste(i,round(kurs_veraenderung,2),"%")
  
  ifelse(kurs_veraenderung>0,paste("+",round(kurs_veraenderung,2),"%"),paste(round(kurs_veraenderung,2),"%"))
  
  return(kurs_veraenderung)
}

kursver_fun(stock, from_date)

#Aktien des Portfolios in einem Vektor zusammenfassen

xs <- c("0QF.F", "22UA.F", "MRK.DE", "NVD.DE", "ZEG.DE", "SNW.F", "PFE.DE", "5CV.DE", "APC.DE", "TL0.DE")

#Kursveränderungen für alle Aktien des Portfolios berechnen
total_X <- data.frame()

for (x in xs) {
  total_X_ROW <- cbind.data.frame(kursver=kursver_fun(x, from_date), Aktie = x)
  total_X <- rbind(total_X, total_X_ROW)
}

print(total_X)




