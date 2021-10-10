setwd("D:/nir/hackaton")
getwd()

install.packages("forecast")
install.packages("plyr")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("nortest")
install.packages("devtools")

library("forecast")
library("lubridate")
library("plyr")
library("ggplot2")
library("dplyr")
library("ggpubr")
library("nortest")
library("RDCOMClient")
library("devtools")
library("tidyverse")




Sys.time()
Sys.setlocale("LC_TIME", "C")

trip <- read.table(file = "TripTimes.csv", header = TRUE, sep = ",")

charge <- read.table(file = "EVChargeData2.csv", header = TRUE, sep = ",")

chargetime <- read.table(file = "Charge.csv", header = TRUE, sep = ",")

charge2015peak <- read.table(file = "Charge2015Peak.csv", header = TRUE, sep = ",")
charge2015peak <- charge2015peak[,-1]

number_of_IDs <- length(unique(charge$ParticipantID))

charge$DeltaTime <- difftime(time1 = charge$BatteryChargeStopDate, time2 = charge$BatteryChargeStartDate)
charge$DeltaCharge <- charge$Ending.SoC..of.12. - charge$Starting.SoC..of.12.

chargetime$DeltaTime <- difftime(time1 = chargetime$BatteryChargeStopTime, time2 = chargetime$BatteryChargeStartTime)
chargetime$DeltaCharge <- chargetime$Ending.SoC..of.12. - chargetime$Starting.SoC..of.12.




sum(charge$Starting.SoC..of.12.> 7)
SoC <- charge$Starting.SoC..of.12.
write.csv(SoC, "SoCforMatlab.csv", row.names=FALSE)





charge2015 <- charge[as.Date(charge$BatteryChargeStartDate) >= "2015-01-01",]

charge2015$DateDayStart <- c(0)
charge2015$DayNumber <- c(0)


startdate <- as.POSIXct("2015-01-01")
stopdate <- as.POSIXct("2015-12-01")

loopvar <- startdate

peaktime <- as.POSIXct("2015-01-01 19:00:00")

daynumber <- 1

#Формирование датасета только с пиками

 charge2015peak <- charge2015[FALSE,]
 newrowcharge2015peak <- charge2015[FALSE,]

 while (loopvar <= stopdate)
 {
   newrowcharge2015peak <- charge2015[as.POSIXct(charge2015$BatteryChargeStartDate) < peaktime & as.POSIXct(charge2015$BatteryChargeStopDate) > peaktime , ]
   newrowcharge2015peak$DateDayStart <- loopvar
   newrowcharge2015peak$DayNumber <- daynumber
   charge2015peak <- rbind(charge2015peak, newrowcharge2015peak)
   loopvar <- loopvar + 86400
   peaktime <- peaktime + 86400
   daynumber <- daynumber + 1
   print(peaktime)
 }

write.csv(charge2015peak, "Charge2015Peak.csv")

Dates <- seq(as.Date("2015-01-01"), as.Date("2015-11-29"), by = "day")
numcars <- table(charge2015peak$DayNumber)

NumberOfCars <- data.frame(Dates, numcars)[,-2] 


#Графики

barplot(NumberOfCars$Freq, col = "#9acdec", cex.axis = 2.5)     #Количество машин по дням

ggplot(NumberOfCars, aes(x = Freq)) + 
  geom_histogram(fill = "#9acdec", col = "black", binwidth = 2) + 
  theme(axis.text.x = element_text(size = 30)) + 
  theme(axis.text.y = element_text(size = 30))        #Распределение


#ggdensity(NumberOfCars$Freq, size = 1)

ggqqplot(NumberOfCars$Freq) + 
  theme(axis.text.x = element_text(size = 30)) + 
  theme(axis.text.y = element_text(size = 30))

shapiro.test(NumberOfCars$Freq)

ad.test(NumberOfCars$Freq)






charge2015$IfPeak <- c(0)
charge2015$DayNumber <- c(0)



charge2015 <- charge[as.Date(charge$BatteryChargeStartDate) >= "2015-01-01",]

charge2015$DateDayStart <- c(0)
charge2015$DayNumber <- c(0)


startdate <- as.POSIXct("2015-01-01")
stopdate <- as.POSIXct("2015-11-29")

loopvar <- startdate

todaystart <- as.POSIXct("2015-01-01 00:00:01")

tomorrowstart <- as.POSIXct("2015-01-02 00:00:01")

peaktime <- as.POSIXct("2015-01-01 19:00:00")

conditionfornopeak <- as.POSIXct(charge2015$BatteryChargeStartDate) > peaktime | as.POSIXct(charge2015$BatteryChargeStopDate) < peaktime

daynumber <- 1

 charge2015ifpeak <- charge2015[FALSE,]
 newrowcharge2015ifpeak <- charge2015[FALSE,]
 newrowcharge2015ifnopeak <- charge2015[FALSE,]
 
 #Формирование датасета где 1 - попадает в пик, 0 - не попадает в пик

  while (loopvar <= stopdate)
  {
    newrowcharge2015ifpeak <- charge2015[as.POSIXct(charge2015$BatteryChargeStartDate) < peaktime & as.POSIXct(charge2015$BatteryChargeStopDate) > peaktime , ]
    newrowcharge2015ifpeak$IfPeak <- 1
    newrowcharge2015ifpeak$DayNumber <- daynumber
    newrowcharge2015ifnopeak <- charge2015[as.POSIXct(charge2015$BatteryChargeStartDate) > todaystart & as.POSIXct(charge2015$BatteryChargeStartDate) < tomorrowstart & conditionfornopeak , ]
    newrowcharge2015ifnopeak$IfPeak <- 0
    newrowcharge2015ifnopeak$DayNumber <- daynumber
    charge2015ifpeak <- rbind(charge2015ifpeak, newrowcharge2015ifpeak, newrowcharge2015ifnopeak)
    loopvar <- loopvar + 86400
    todaystart <- todaystart + 86400
    tomorrowstart <- tomorrowstart + 86400
    peaktime <- peaktime + 86400
    daynumber <- daynumber + 1
    print(peaktime)
  }

 #Убираем повторы
 
 charge2015ifpeakwithoutdoubles <- charge2015ifpeak %>% distinct(ParticipantID, BatteryChargeStartDate, BatteryChargeStopDate, Starting.SoC..of.12., Ending.SoC..of.12., DeltaTime, DeltaCharge, .keep_all = TRUE)


 write.csv(charge2015ifpeakwithoutdoubles, "Charge2015IfPeak.csv")

 dataformatlab <- charge2015ifpeakwithoutdoubles$IfPeak
 
 write.csv(dataformatlab, "MatabData.csv", row.names=FALSE)
 
 
