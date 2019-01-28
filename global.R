

############################################################
################ GLOBAL OPTIONS ############################
############################################################


library(shiny) 
library(tidyverse)
library(gsheet)
library(ggplot2)
library(shinydashboard)
library(highcharter)
library(quantmod)
library(shinyWidgets)

################ GET DATA ############################
Tue1 <-gsheet2tbl('docs.google.com/spreadsheets/d/1kd84-Rn1HKYF9DJ2pdRPbZYmqC2MD_JjM95by7RFFik/edit?usp=sharing')
Tue2 <-gsheet2tbl('docs.google.com/spreadsheets/d/1luumPYYc6r0JJEMJ3xC4mNA1GUox0GTKoPgKgbrGyno/edit?usp=sharing')
Tue1y2 <- left_join(Tue1, Tue2, by = "Email Address")
Tue1y2nona <- na.omit(Tue1y2)


################ FECHAS ############################
Hours <- format(as.POSIXct(strptime(Tue1y2nona$Timestamp.x,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%H:%M:%S")
Dates <- format(as.POSIXct(strptime(Tue1y2nona$Timestamp.x,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%Y-%m-%d")
Mes <- format(as.POSIXct(strptime(Tue1y2nona$Timestamp.x,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%Y-%m")
Tue1y2nona$Mes <- Mes
Tue1y2nona$Dates <- Dates
Tue1y2nona$Hours <- Hours


################ preparacion GRAFICOS Hichart  ############################
Tue1y2nona$CONPRE <- rowMeans(Tue1y2nona[,c(9:12)], na.rm=TRUE)
Tue1y2nona$ACCPRE <- rowMeans(Tue1y2nona[,c(13:22)], na.rm=TRUE) 
Tue1y2nona$CONONL <- rowMeans(Tue1y2nona[,c(29:32)], na.rm=TRUE) 
Tue1y2nona$ACCONL <- rowMeans(Tue1y2nona[,c(33:42)], na.rm=TRUE) 
Tue1y2nona$CONDIF <- Tue1y2nona$CONONL - Tue1y2nona$CONPRE
Tue1y2nona$ACCDIF <- Tue1y2nona$ACCONL - Tue1y2nona$ACCPRE



