
library(dplyr)

Amazon <- c(17.65, 76.47, 5.88, 5.12)
Daimler <- c(100, 0, 0, 14.42)      
Google <- c(18.18, 72.73, 9.09, 2.21)
Microsoft <- c(37.5, 50, 12.5, 3.84)
Netflix <- c(37.5, 50, 12.5, 1.46)
Tesla <- c(41.18, 47.06, 11.76, 17.88)
Walmart <- c(40, 60, 0, -1.98)
Lufthansa <- c(25, 66.67, 8.33, 21.34)

total <- rbind(Amazon, Daimler, Google, Microsoft, Netflix, Tesla, Walmart, Lufthansa)
total <- as.data.frame(total)
total <- tibble::rownames_to_column(total)
colnames(total) <- c("Aktie", "Anteil positiv", "Anteil negativ", "Anteil neutral", "Kursveränderung")
total

positiv_desc <- arrange(total, desc(total$`Anteil positiv`))
positiv_desc
positiv <- t(positiv_desc[, c(2,5)])
colnames(positiv) <- positiv_desc$Aktie
positiv
bp <- barplot(positiv, main = "Aktien mit positiven Scores zusammen mit Kursveränderung", 
        ylab = "%", col=c("darkblue","red"), las=2, 
        legend = rownames(positiv), beside = TRUE)

negativ_desc <- arrange(total, desc(total$`Anteil negativ`))
negativ_desc
negativ <- t(negativ_desc[, c(3,5)])
colnames(negativ) <- negativ_desc$Aktie
negativ

barplot(negativ, main = "Aktien mit negativen Scores zusammen mit Kursveränderung", 
        ylab = "%", col=c("darkblue","red"), las=2,
        legend = rownames(negativ), beside = TRUE)

neutral_desc <- arrange(total, desc(total$`Anteil neutral`))
neutral_desc
neutral <- t(neutral_desc[, c(4,5)])
colnames(neutral) <- neutral_desc$Aktie
neutral

barplot(neutral, main = "Aktien mit neutralen Scores zusammen mit Kursveränderung", 
        ylab = "%", col=c("darkblue","red"), las=2,
        legend = rownames(neutral), beside = TRUE)


